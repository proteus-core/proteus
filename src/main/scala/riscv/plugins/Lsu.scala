package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib._

class Lsu(addressStages: Set[Stage], loadStages: Seq[Stage], storeStage: Stage)
    extends Plugin[Pipeline]
    with LsuService {
  private var addressTranslator = new LsuAddressTranslator {
    override def translate(
        stage: Stage,
        address: UInt,
        operation: SpinalEnumCraft[LsuOperationType.type],
        width: SpinalEnumCraft[LsuAccessWidth.type]
    ): UInt = {
      address
    }
  }

  private var addressTranslatorChanged = false
  private var externalAddress: UInt = _
  private var hasExternalOps = false

  object Data {
    object LSU_OPERATION_TYPE extends PipelineData(LsuOperationType())
    object LSU_ACCESS_WIDTH extends PipelineData(LsuAccessWidth())
    object LSU_IS_UNSIGNED extends PipelineData(Bool())
    object LSU_IS_EXTERNAL_OP extends PipelineData(Bool())
    object LSU_TARGET_ADDRESS extends PipelineData(UInt(config.xlen bits)) // TODO: Flow?
    object LSU_TARGET_VALID extends PipelineData(Bool())
  }

  class DummyFormalService extends FormalService {
    override def lsuDefault(stage: Stage): Unit = ()
    override def lsuOnLoad(stage: Stage, addr: UInt, rmask: Bits, rdata: UInt): Unit = ()
    override def lsuOnStore(stage: Stage, addr: UInt, wmask: Bits, wdata: UInt): Unit = ()
    override def lsuOnMisaligned(stage: Stage): Unit = ()
  }

  override def addStore(
      opcode: MaskedLiteral,
      width: SpinalEnumCraft[LsuAccessWidth.type]
  ): Unit = {
    pipeline.service[DecoderService].configure { config =>
      config.addDecoding(
        opcode,
        Map(
          Data.LSU_OPERATION_TYPE -> LsuOperationType.STORE,
          Data.LSU_ACCESS_WIDTH -> width,
          Data.LSU_IS_EXTERNAL_OP -> True,
          Data.LSU_TARGET_VALID -> False
        )
      )
    }

    hasExternalOps = true

    pipeline.service[IssueService].setDestinations(opcode, addressStages)
  }

  override def addLoad(
      opcode: MaskedLiteral,
      width: SpinalEnumCraft[LsuAccessWidth.type],
      unsigned: Boolean
  ): Unit = {
    pipeline.service[DecoderService].configure { config =>
      config.addDecoding(
        opcode,
        Map(
          Data.LSU_OPERATION_TYPE -> LsuOperationType.LOAD,
          Data.LSU_ACCESS_WIDTH -> width,
          Data.LSU_IS_UNSIGNED -> Bool(unsigned),
          Data.LSU_IS_EXTERNAL_OP -> True,
          Data.LSU_TARGET_VALID -> False
        )
      )
    }

    hasExternalOps = true

    pipeline.service[IssueService].setDestinations(opcode, addressStages)
  }

  override def setAddress(address: UInt): Unit = {
    externalAddress := address
  }

  override def stage: Stage = addressStages.head // TODO: which one? needed?

  override def operation(stage: Stage): SpinalEnumCraft[LsuOperationType.type] = {
    stage.value(Data.LSU_OPERATION_TYPE)
  }

  // TODO: pretty hacky
  override def operationOutput(stage: Stage): SpinalEnumCraft[LsuOperationType.type] = {
    stage.output(Data.LSU_OPERATION_TYPE)
  }
  override def operationOfBundle(bundle: Bundle with DynBundleAccess[PipelineData[Data]]): Data = {
    bundle.element(Data.LSU_OPERATION_TYPE.asInstanceOf[PipelineData[Data]])
  }
  override def addressOfBundle(bundle: Bundle with DynBundleAccess[PipelineData[Data]]): UInt = {
    bundle.elementAs[UInt](Data.LSU_TARGET_ADDRESS.asInstanceOf[PipelineData[Data]])
  }
  override def addressValidOfBundle(
      bundle: Bundle with DynBundleAccess[PipelineData[Data]]
  ): Bool = {
    bundle.elementAs[Bool](Data.LSU_TARGET_VALID.asInstanceOf[PipelineData[Data]])
  }

  override def width(stage: Stage): SpinalEnumCraft[LsuAccessWidth.type] = {
    stage.value(Data.LSU_ACCESS_WIDTH)
  }

  override def setup(): Unit = {
    for (addressStage <- addressStages) {
      addressStage plug new Area {
        val externalAddress = UInt(config.xlen bits).assignDontCare()
        Lsu.this.externalAddress = externalAddress
      }
    }

    val intAlu = pipeline.service[IntAluService]

    val allOpcodes = Seq(
      Opcodes.LB,
      Opcodes.LH,
      Opcodes.LW,
      Opcodes.LBU,
      Opcodes.LHU,
      Opcodes.SB,
      Opcodes.SH,
      Opcodes.SW
    )

    for (opcode <- allOpcodes) {
      intAlu.addOperation(opcode, intAlu.AluOp.ADD, intAlu.Src1Select.RS1, intAlu.Src2Select.IMM)
    }

    val decoder = pipeline.service[DecoderService]

    decoder.configure { config =>
      config.addDefault(
        Map(
          Data.LSU_OPERATION_TYPE -> LsuOperationType.NONE,
          Data.LSU_IS_UNSIGNED -> False,
          Data.LSU_IS_EXTERNAL_OP -> False
        )
      )

      def addLoad(
          opcode: MaskedLiteral,
          width: SpinalEnumElement[LsuAccessWidth.type],
          unsigned: Bool
      ) = {
        config.addDecoding(
          opcode,
          InstructionType.I,
          Map(
            Data.LSU_OPERATION_TYPE -> LsuOperationType.LOAD,
            Data.LSU_ACCESS_WIDTH -> width,
            Data.LSU_IS_UNSIGNED -> unsigned,
            Data.LSU_TARGET_VALID -> False
          )
        )

        pipeline.service[IssueService].setDestinations(opcode, addressStages)
      }

      addLoad(Opcodes.LW, LsuAccessWidth.W, False)
      addLoad(Opcodes.LH, LsuAccessWidth.H, False)
      addLoad(Opcodes.LB, LsuAccessWidth.B, False)
      addLoad(Opcodes.LHU, LsuAccessWidth.H, True)
      addLoad(Opcodes.LBU, LsuAccessWidth.B, True)

      def addStore(opcode: MaskedLiteral, width: SpinalEnumElement[LsuAccessWidth.type]) = {
        config.addDecoding(
          opcode,
          InstructionType.S,
          Map(
            Data.LSU_OPERATION_TYPE -> LsuOperationType.STORE,
            Data.LSU_ACCESS_WIDTH -> width,
            Data.LSU_TARGET_VALID -> False
          )
        )

        pipeline.service[IssueService].setDestinations(opcode, addressStages)
      }

      addStore(Opcodes.SW, LsuAccessWidth.W)
      addStore(Opcodes.SH, LsuAccessWidth.H)
      addStore(Opcodes.SB, LsuAccessWidth.B)
    }
  }

  override def build(): Unit = {
    for (addressStage <- addressStages) {
      addressStage plug new Area {
        import addressStage._

        val operation = value(Data.LSU_OPERATION_TYPE)
        val accessWidth = value(Data.LSU_ACCESS_WIDTH)
        val unsigned = value(Data.LSU_IS_UNSIGNED) // TODO: another hack
        val aluResult = value(pipeline.service[IntAluService].resultData)

        val inputAddress = if (hasExternalOps) {
          // We keep track of whether we have external loads/stores to 1) prevent LSU_IS_EXTERNAL_OP
          // from being added to the pipeline regs and 2) not generate this mux when not necessary
          // (although the latter might be optimized away at some point).
          value(Data.LSU_IS_EXTERNAL_OP) ? externalAddress | aluResult
        } else {
          aluResult
        }

        val address = addressTranslator.translate(
          addressStage,
          inputAddress,
          operation,
          accessWidth
        )

        output(Data.LSU_TARGET_ADDRESS) := address
        when(operation === LsuOperationType.LOAD || operation === LsuOperationType.STORE) {
          output(Data.LSU_TARGET_VALID) := True
        }
      }
    }

    def checkAccessWidth(accessWidth: SpinalEnumCraft[LsuAccessWidth.type], address: UInt) = {
      val misaligned = Bool()
      val baseMask = Bits(config.xlen / 8 bits)

      switch(accessWidth) {
        is(LsuAccessWidth.B) {
          misaligned := False
          baseMask := B"0001"
        }
        is(LsuAccessWidth.H) {
          misaligned := (address & 1) =/= 0
          baseMask := B"0011"
        }
        is(LsuAccessWidth.W) {
          misaligned := (address & 3) =/= 0
          baseMask := B"1111"
        }
      }
      (misaligned, baseMask)
    }

    def trap(cause: TrapCause, stage: Stage) = {
      val trapHandler = pipeline.service[TrapService]
      trapHandler.trap(stage, cause)
    }

    val memService = pipeline.service[MemoryService]
    val (loadDBuses, storeDBus) = memService.createInternalDBus(loadStages, storeStage)

    val formal = pipeline.serviceOption[FormalService] match {
      case Some(service) => service
      case None => new DummyFormalService
    }

    for (loadStage <- loadStages) {
      loadStage plug {
        formal.lsuDefault(loadStage)
      }
    }

    // Make sure we don't call lsuDefault twice when loadStage == storeStage (the second call will
    // overwrite everything that was written to the formal signals in the first stage that was
    // built).
    if (!loadStages.contains(storeStage)) {
      storeStage plug {
        formal.lsuDefault(storeStage)
      }
    }

    val loadAreas = loadStages.zipWithIndex.map { case (loadStage, stageIndex) =>
      loadStage plug new Area {

        import loadStage._

        val operation = value(Data.LSU_OPERATION_TYPE)
        val accessWidth = value(Data.LSU_ACCESS_WIDTH)
        val address = value(Data.LSU_TARGET_ADDRESS)
        val valid = value(Data.LSU_TARGET_VALID) // needed as a hack either way

        val dbusCtrl = new MemBusControl(loadDBuses(stageIndex))

        val isActive = operation === LsuOperationType.LOAD

        val (misaligned, baseMask) = checkAccessWidth(accessWidth, address)

        val mask = baseMask |<< address(1 downto 0)

        val busReady = Bool()
        val loadActive = RegInit(False)
        busReady := False

        when(isActive && misaligned) {
          if (pipeline.hasService[TrapService]) {
            trap(TrapCause.LoadAddressMisaligned(address), loadStage)

            formal.lsuOnMisaligned(loadStage)
          }
        }

        when(arbitration.isValid && !misaligned) {
          when(isActive) {
            val busAddress = address & U(0xfffffffcL)
            val valid = Bool()
            valid := False
            val wValue = UInt(config.xlen bits).getZero
            busReady := dbusCtrl.isReady
            when(busReady) {
              loadActive := True
            }
            when(busReady || loadActive) {
              val tpl = dbusCtrl.read(busAddress)
              valid := tpl._1
              wValue := tpl._2
            }
            when(valid) {
              loadActive := False
            }
            arbitration.isReady := valid
            val result = UInt(config.xlen bits)
            result := wValue

            switch(value(Data.LSU_ACCESS_WIDTH)) {
              is(LsuAccessWidth.H) {
                val offset = (address(1) ## B"0000").asUInt
                val hValue = wValue(offset, 16 bits)

                when(value(Data.LSU_IS_UNSIGNED)) {
                  result := Utils.zeroExtend(hValue, config.xlen)
                } otherwise {
                  result := Utils.signExtend(hValue, config.xlen)
                }
              }
              is(LsuAccessWidth.B) {
                val offset = (address(1 downto 0) ## B"000").asUInt
                val bValue = wValue(offset, 8 bits)

                when(value(Data.LSU_IS_UNSIGNED)) {
                  result := Utils.zeroExtend(bValue, config.xlen)
                } otherwise {
                  result := Utils.signExtend(bValue, config.xlen)
                }
              }
            }

            output(pipeline.data.RD_DATA) := result
            output(pipeline.data.RD_DATA_VALID) := True
            formal.lsuOnLoad(loadStage, busAddress, mask, wValue)
          }
        } otherwise {
          loadActive := False
          dbusCtrl.invalidate()
        }
      }
    }

    storeStage plug new Area {
      import storeStage._

      val storeIndex = loadStages.indexOf(storeStage)

      val dbusCtrl = if (storeIndex != -1) {
        loadAreas(storeIndex).dbusCtrl
      } else {
        new MemBusControl(storeDBus)
      }

      val operation = value(Data.LSU_OPERATION_TYPE)
      val accessWidth = value(Data.LSU_ACCESS_WIDTH)
      val address = value(Data.LSU_TARGET_ADDRESS)
      val busAddress = address & U(0xfffffffcL)

      val isActive = operation === LsuOperationType.STORE

      val (misaligned, baseMask) = checkAccessWidth(accessWidth, address)

      val mask = baseMask |<< address(1 downto 0)

      when(isActive && misaligned) {
        if (pipeline.hasService[TrapService]) {
          trap(TrapCause.StoreAddressMisaligned(address), storeStage)

          formal.lsuOnMisaligned(storeStage)
        }
      }

      when(arbitration.isValid && !misaligned) {
        when(isActive) {
          val wValue = value(pipeline.data.RS2_DATA)
          arbitration.rs2Needed := True
          val data = UInt(config.xlen bits)
          data := wValue

          switch(value(Data.LSU_ACCESS_WIDTH)) {
            is(LsuAccessWidth.H) {
              val hValue = wValue(15 downto 0)

              when(address(1)) {
                data := hValue << 16
              } otherwise {
                data := hValue.resized
              }
            }
            is(LsuAccessWidth.B) {
              val bValue = wValue(7 downto 0)

              switch(address(1 downto 0).asBits) {
                is(B"00") {
                  data := bValue.resized
                }
                is(B"01") {
                  data := (bValue << 8).resized
                }
                is(B"10") {
                  data := (bValue << 16).resized
                }
                is(B"11") {
                  data := (bValue << 24).resized
                }
              }
            }
          }

          val accepted = dbusCtrl.write(busAddress, data, mask)
          arbitration.isReady := accepted

          formal.lsuOnStore(storeStage, busAddress, mask, data)
        }
      }
    }
  }

  override def setAddressTranslator(translator: LsuAddressTranslator): Unit = {
    assert(!addressTranslatorChanged, "LsuAddressTranslator can only be set once")

    addressTranslator = translator
    addressTranslatorChanged = true
  }
}
