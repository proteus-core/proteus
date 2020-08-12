package riscv.plugins.capabilities

import riscv._
import spinal.core._
import spinal.lib._

class RegisterFile(readStage: Stage, writeStage: Stage)
                  (implicit context: Context) extends Plugin[Pipeline] {
  override def setup(): Unit = {
    val decoder = pipeline.getService[DecoderService]

    decoder.configure {config =>
      config.addDefault(context.data.CD_DATA, Capability.Null)
    }

    val hazardInfo = DataHazardInfo(
      RegisterType.CAP,
      context.data.CS1_DATA,
      context.data.CS2_DATA,
      context.data.CD_DATA
    )

    pipeline.getService[DataHazardService].addHazard(hazardInfo)
  }

  override def build(): Unit = {
    case class ReadIo() extends Bundle with IMasterSlave {
      val cs1 = UInt(5 bits)
      val cs2 = UInt(5 bits)
      val cs1Data = Capability()
      val cs2Data = Capability()

      override def asMaster(): Unit = {
        in(cs1, cs2)
        out(cs1Data, cs2Data)
      }
    }

    case class WriteIo() extends Bundle with IMasterSlave {
      val cd = UInt(5 bits)
      val data = Capability()
      val write = Bool()

      override def asMaster(): Unit = {
        in(cd, data, write)
      }
    }

    val regFile = pipeline plug new Component {
      setDefinitionName("CapabilityRegisterFile")

      val readIo = master(ReadIo())
      val writeIo = master(WriteIo())
      val regs = Mem(Capability(), Seq.fill(config.numRegs) {Capability.Null})

      // Add a wire for each register with a readable name. This is to easily
      // view register values in a wave dump.
      for (i <- 0 until config.numRegs) {
        val regWire = Capability()
        regWire.setName(s"c$i")
        regWire := regs.readAsync(U(i).resized, writeFirst)
      }

      def readReg(addr: UInt) = regs.readAsync(addr, writeFirst)
      readIo.cs1Data := readReg(readIo.cs1)
      readIo.cs2Data := readReg(readIo.cs2)

      when (writeIo.write && writeIo.cd =/= 0) {
        regs.write(writeIo.cd, writeIo.data)
      }
    }

    val readArea = readStage plug new Area {
      import readStage._

      val regFileIo = slave(ReadIo())

      regFileIo.cs1 := value(context.data.CS1)
      regFileIo.cs2 := value(context.data.CS2)
      output(context.data.CS1_DATA) := regFileIo.cs1Data
      output(context.data.CS2_DATA) := regFileIo.cs2Data
    }

    val writeArea = writeStage plug new Area {
      import writeStage._

      val regFileIo = slave(WriteIo())

      regFileIo.cd := value(context.data.CD)
      regFileIo.data := value(context.data.CD_DATA)

      val trapHandler = pipeline.getService[TrapService]
      val hasTrapped = trapHandler.hasTrapped(writeStage)
      regFileIo.write :=
        (value(pipeline.data.RD_TYPE) === RegisterType.CAP) &&
        arbitration.isDone &&
        !hasTrapped
    }

    pipeline plug new Area {
      regFile.readIo <> readArea.regFileIo
      regFile.writeIo <> writeArea.regFileIo
    }
  }
}
