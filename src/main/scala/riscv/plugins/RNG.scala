package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib._

import scala.collection.mutable

/** Unrolled Bivium stream cipher implementation
  *
  * This is an unrolled implementation that outputs `outputBits` per clock cycle.
  *
  * This SpinalHDL implementation is based on: Cassiers, Gaëtan, et al. "Randomness generation for
  * secure hardware masking– unrolled trivium to the rescue." IACR Commun. Cryptol., 1(2):4, 2024.
  *
  * @param outputBits:
  *   The number of outputbits per cycle, which is equal to the level of unrolling. Defaults to 64.
  *
  * The component has a `stream` output that provides the random bits. The stream is valid after
  * initialization and a warmup period of 708 steps.
  */
class Bivium(outputBits: Int = 64) extends Component {
  val io = new Bundle {
    val key = in Bits (80 bits)
    val iv = in Bits (80 bits)
    val initialize = in Bool ()
    val stream = master Stream (Bits(outputBits bits))
  }

  private val state = Vec(Bits(177 bits), outputBits + 1)
  private val stateReg = Reg(Bits(177 bits))
  state(0) := stateReg

  private val streamOut = Bits(outputBits bits)

  // At least 708 steps need to be executed before outputting the randomness.
  private val warmedup = Timeout(scala.math.ceil(708.0 / outputBits).toInt)
  private val initialized = Reg(Bool()) init False

  io.stream.payload := streamOut
  io.stream.valid := initialized && warmedup

  when(io.initialize) {
    stateReg := B"4'x0" ## io.iv ## B"12'x000" ## B"1'b0" ## io.key

    initialized := True
    warmedup.clear()
  } elsewhen (!warmedup) {
    stateReg := state(outputBits)
  } elsewhen (io.stream.ready) {
    stateReg := state(outputBits)
  }

  for (i <- 1 to outputBits) {
    val t1 = state(i - 1)(65) ^ state(i - 1)(92)
    val t2 = state(i - 1)(161) ^ state(i - 1)(176)

    val bit1 = t1 ^ (state(i - 1)(90) & state(i - 1)(91)) ^ state(i - 1)(170)
    val bit2 = t2 ^ (state(i - 1)(174) & state(i - 1)(175)) ^ state(i - 1)(68)

    state(i) := state(i - 1)(175 downto 93) ## bit1 ## state(i - 1)(91 downto 0) ## bit2
    streamOut(outputBits - i) := t1 ^ t2
  }
}

/** A FIFO queue to buffer the RNG values.
  *
  * @param queueDepth:
  *   The size of the FIFO queue.
  */
class RngFifo(queueDepth: Int = 2)(implicit config: Config) extends RngBuffer {
  private val rngFifo =
    new StreamFifoLowLatency(dataType = Bits(config.xlen bits), depth = queueDepth) // latency = 0

  rngFifo.io.pop.ready := False
  rngFifo.io.flush := False

  ////////////////////////////
  // Reading from the queue //
  ////////////////////////////

  def read(): UInt = {
    U(rngFifo.io.pop.payload, config.xlen bits)
  }
  def isValid(): Bool = {
    rngFifo.io.pop.valid
  }

  def request(): Unit = {
    rngFifo.io.pop.ready := True
  }

  def flush(): Unit = {
    rngFifo.io.flush := True
  }

  def isFull(): Bool = {
    !rngFifo.io.push.ready
  }

  when(isValid()) {
    rngFifo.io.pop.ready := False
  }

  /////////////////////////////////////
  // Inserting values into the queue //
  /////////////////////////////////////

  def connect(inputStream: Stream[Bits]): Unit = {
    rngFifo.io.push << inputStream
  }
}

private class RngComponent(implicit config: Config) extends Component {
  setDefinitionName("RNG")
}

private class csrRng(implicit config: Config) extends Csr {
  val rgnControl = Reg(UInt(config.xlen bits)).init(0)

  override def read(): UInt = rgnControl
  override def write(value: UInt): Unit = this.rgnControl := value
}

private class csrSeed(implicit config: Config) extends Csr {
  val seedValue = Reg(Bits(config.xlen bits)).init(0)

  override def read(): UInt = seedValue.asUInt
  def readb(): Bits = seedValue
  override def write(value: UInt): Unit = this.seedValue := value.asBits
}

private class BiviumCore(outputBits: Int = 64) extends Component {
  val io = new Bundle {
    val key = in Bits (80 bits)
    val iv = in Bits (80 bits)
    val initialize = in Bool ()
    val stream = master Stream (Bits(outputBits bits))
  }

  private val core = new Bivium(outputBits)
  core.io.key := io.key
  core.io.iv := io.iv
  core.io.initialize := io.initialize
  io.stream << core.io.stream
}

/** Pseudorandom number generator
  *
  * Random numbers are generated with Bivium. The key can be updated through CSRs.
  *
  * Each component that requires random numbers needs to to register a RngFifo. This component has
  * to be initialized after any component that registers a RngBuffer.
  *
  * ```
  *                       ( IV )
  *                      ___|____
  *             Key --> | Bivium |
  *                     |________|
  *                         |
  *          _______________|_______________
  *          |         |          |        |
  *        RNG 0     RNG 1      RNG 2     ...
  *
  * ```
  *
  * @param allowUninitializedRng:
  *   Whether to allow the RNG to generate random using the default key. Setting this to `false`
  *   (default) requires the key to be updated through the RNG Control CSR.
  */
class Rng(allowUninitializedRng: Boolean = false) extends Plugin[Pipeline] with RngService {
  // RNG CSR flags
  private val RNG_DISABLE = 0x1 /* If set, disable the RNG */
  private val RNG_UPDATEIV = 0x2 /* If set, update key using seed CSRs */

  private val CSR_RNGCONTROL = 0x863
  private val CSR_SEED0 = 0x880
  private val CSR_SEED1 = 0x881
  private val CSR_SEED2 = 0x882

  // https://numbergenerator.org/hex-code-generator#!numbers=1&length=32
  private val INIT_IV = BigInt("BC4BA40FE72CF210D226", 16)
  private val INIT_KEY = BigInt("DBBFF2853034FA92DE3D", 16)

  ////////////////////////
  // RNG queue handling //
  ////////////////////////

  // lazy because pipeline is null at the time of construction.
  private lazy val component = pipeline plug new RngComponent
  private val rngbuffers = mutable.ArrayBuffer[RngBuffer]()

  /** Register a new RNG buffer.
    *
    * @param rngbuffer:
    *   The RNG buffer to register.
    *
    * @return
    *   The index of the registered RNG buffer.
    */
  override def registerRngBuffer[T <: RngBuffer](rngbuffer: => T): Int = {
    val pluggedRngBuffer = component.plug(rngbuffer)
    val rngbufferindex = rngbuffers.length
    rngbuffers += pluggedRngBuffer

    rngbufferindex
  }

  /** Get the RNG buffer with the given index.
    *
    * @param id:
    *   The ID of the RNG buffer.
    */
  override def getRngBuffer(id: Int): RngIo = {
    assert(id >= 0 && id < rngbuffers.length)

    val area = component plug new Area {
      val rngIo = master(new RngIo())
      rngIo.setName(s"rng_$id")
      val rng = rngbuffers(id)

      rngIo.rdata := rng.read()
      rngIo.rdata_valid := rng.isValid()
      when(rngIo.rdata_request) {
        rng.request()
      }
    }

    area.rngIo
  }

  override def setup(): Unit = {
    val csrService = pipeline.service[CsrService]

    // The CSR to control the RNG
    csrService.registerCsr(CSR_RNGCONTROL, new csrRng)

    // The CSRs for changing the IV of the RNG
    csrService.registerCsr(CSR_SEED0, new csrSeed)
    csrService.registerCsr(CSR_SEED1, new csrSeed)
    csrService.registerCsr(CSR_SEED2, new csrSeed)
  }

  override def build(): Unit = {
    val rngComponent = component

    val componentArea = rngComponent plug new Area {
      import rngComponent._

      val csrRngControl = slave(new CsrIo)

      val csrSeed0 = slave(new CsrIo)
      val csrSeed1 = slave(new CsrIo)
      val csrSeed2 = slave(new CsrIo)

      ///////////////////////////////
      // Initialization of the RNG //
      ///////////////////////////////
      private val rngCore = new BiviumCore(outputBits = config.xlen)

      // Initial state
      private val rngIV_reg = U(INIT_IV, 80 bits)
      private val rngKey_reg = Reg(UInt(80 bits)) init (INIT_KEY)

      private val rngCoreInitialized = Reg(Bool()) init False
      if (allowUninitializedRng) {
        rngCoreInitialized := True
      }

      rngCore.io.key := rngKey_reg.asBits
      rngCore.io.iv := rngIV_reg.asBits
      rngCore.io.initialize := rngCoreInitialized.rise()

      private val rngDisabled = Reg(Bool()) init False

      ////////////////////////////
      // Connecting RNG Buffers //
      ////////////////////////////

      // Connect the RNG stream from the RNG core to the RNG buffers
      val rngStream = Stream(Bits(config.xlen bits))
      rngStream.valid := rngCoreInitialized && rngCore.io.stream.valid
      rngStream.payload := rngDisabled ? B(0, config.xlen bits) | rngCore.io.stream.payload
      rngCore.io.stream.ready := rngStream.ready

      // Select the first ready output (Round Robin)
      val rngBufferReady = rngbuffers.map(!_.isFull()).asBits
      val selectRngBuffer = OHToUInt(OHMasking.roundRobinNext(rngBufferReady, rngStream.fire))

      // Demux the RNG stream to the RNG buffers
      val outputRngStreams = StreamDemux(rngStream, selectRngBuffer, rngbuffers.length)
      rngbuffers.zip(outputRngStreams).foreach { case (buf, stream) =>
        buf.connect(stream)
      }

      ////////////////////////
      // Updating key logic //
      ////////////////////////

      /** Update the key of the Bivium RNG.
        *
        * The new key will be taken from the CSR registers.
        */
      private def updateKey(): Unit = {
        rngKey_reg := ((U(0, 48 bits) @@ csrSeed0.read()) |
          (csrSeed1.read() << 32).resized |
          (csrSeed2.read()(16 downto 0) << 64).resized)

        // Flush any connected RngBuffers to discard stale rngs generated
        // using the old key
        for (i <- 0 until rngbuffers.length) {
          rngbuffers(i).flush()
        }

        rngCoreInitialized := True
      }

      // Update the Bivium key from the CSR registers. The key can only be
      // initialized once.
      when((csrRngControl.read() & RNG_UPDATEIV) =/= 0 && !rngCoreInitialized) {
        updateKey()
      }

      // Disable the rng generation. All outputs will be replaced with all 0s.
      private def disableRNG(): Unit = {
        rngDisabled := True
      }

      // Disable RNG through the CSR register. The RNG cannot currently be
      // re-enabled after disabling.
      when((csrRngControl.read() & RNG_DISABLE) =/= 0) {
        disableRNG()
      }
    }

    pipeline plug new Area {
      val csrService = pipeline.service[CsrService]

      componentArea.csrRngControl <> csrService.getCsr(CSR_RNGCONTROL)

      componentArea.csrSeed0 <> csrService.getCsr(CSR_SEED0)
      componentArea.csrSeed1 <> csrService.getCsr(CSR_SEED1)
      componentArea.csrSeed2 <> csrService.getCsr(CSR_SEED2)
    }
  }
}
