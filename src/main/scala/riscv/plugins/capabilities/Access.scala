package riscv.plugins.capabilities

import riscv._

import spinal.core._

class Access(stage: Stage)(implicit context: Context) extends Plugin[Pipeline] {
  object FieldSelect extends SpinalEnum {
    val PERM, BASE, LEN, TAG, OFFSET, ADDR = newElement()
  }

  object Modification extends SpinalEnum {
    val SET_BOUNDS, CLEAR_TAG = newElement()
  }

  object Data {
    object CGET extends PipelineData(Bool())
    object CFIELD extends PipelineData(FieldSelect())
    object CMODIFY extends PipelineData(Bool())
    object CMODIFICATION extends PipelineData(Modification())
  }

  override def setup(): Unit = {
    pipeline.getService[DecoderService].configure {config =>
      config.addDefault(Map(
        Data.CGET -> False,
        Data.CMODIFY -> False
      ))

      val getters = Map(
        Opcodes.CGetPerm   -> FieldSelect.PERM,
        Opcodes.CGetBase   -> FieldSelect.BASE,
        Opcodes.CGetLen    -> FieldSelect.LEN,
        Opcodes.CGetTag    -> FieldSelect.TAG,
        Opcodes.CGetOffset -> FieldSelect.OFFSET,
        Opcodes.CGetAddr   -> FieldSelect.ADDR
      )

      for ((opcode, selector) <- getters) {
        config.addDecoding(opcode, InstructionType.R_CxR, Map(
          Data.CGET -> True,
          Data.CFIELD -> selector
        ))
      }

      val modifiers = Map(
        Opcodes.CSetBounds      -> Modification.SET_BOUNDS,
        Opcodes.CSetBoundsExact -> Modification.SET_BOUNDS
      )

      for ((opcode, modification) <- modifiers) {
        config.addDecoding(opcode, InstructionType.R_CRC, Map(
          Data.CMODIFY -> True,
          Data.CMODIFICATION -> modification
        ))
      }
    }
  }

  override def build(): Unit = {
    stage plug new Area {
      import stage._

      when (arbitration.isValid) {
        when (value(Data.CGET)) {
          stage.arbitration.rs1Needed := True
          val cap = value(context.data.CS1_DATA)
          val rd = value(Data.CFIELD).mux(
            FieldSelect.PERM   -> cap.perms.asIsaBits.asUInt.resized,
            FieldSelect.BASE   -> cap.base,
            FieldSelect.LEN    -> cap.length,
            FieldSelect.TAG    -> cap.tag.asUInt.resized,
            FieldSelect.OFFSET -> cap.offset,
            FieldSelect.ADDR   -> (cap.base + cap.offset) // TODO: use ALU
          )

          output(pipeline.data.RD_DATA) := rd
          output(pipeline.data.RD_VALID) := True
        }

        when (value(Data.CMODIFY)) {
          val cs1 = value(context.data.CS1_DATA)
          val cs2 = value(context.data.CS2_DATA)
          val rs2 = value(pipeline.data.RS2_DATA)
          val cd = Capability()
          cd := cs1

          switch (value(Data.CMODIFICATION)) {
            is (Modification.SET_BOUNDS) {
              val newTop = cs1.address + rs2

              when (!cs1.tag) {
                // TODO: tag violation
              } elsewhen (cs1.address < cs1.base) {
                // TODO: length violation
              } elsewhen (newTop > cs1.top) {
                // TODO: length violation
              } otherwise {
                cd.base := cs1.address
                cd.length := rs2
                cd.offset := 0
              }
            }
          }

          output(context.data.CD_DATA) := cd
          output(pipeline.data.RD_VALID) := True
        }
      }
    }
  }
}
