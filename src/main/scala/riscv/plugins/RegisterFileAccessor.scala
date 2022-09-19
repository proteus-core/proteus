package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib._

class RegisterFileAccessor(readStage: Stage, writeStage: Stage) extends Plugin[Pipeline] {

  override def setup(): Unit = {
    pipeline.serviceOption[DataHazardService] foreach { dataHazardService =>
      val hazardInfo = DataHazardInfo(
        RegisterType.GPR,
        pipeline.data.RS1_DATA,
        pipeline.data.RS2_DATA,
        pipeline.data.RD_DATA
      )

      dataHazardService.addHazard(hazardInfo)
    }
  }

  override def build(): Unit = {
    val regFile = pipeline plug new RegisterFile()

    val readArea = readStage plug new Area {
      import readStage._

      val regFileIo = slave(RegisterFileReadIo())

      regFileIo.rs1 := value(pipeline.data.RS1)
      regFileIo.rs2 := value(pipeline.data.RS2)
      output(pipeline.data.RS1_DATA) := regFileIo.rs1Data
      output(pipeline.data.RS2_DATA) := regFileIo.rs2Data
    }

    val writeArea = writeStage plug new Area {
      import writeStage._

      val regFileIo = slave(RegisterFileWriteIo())

      regFileIo.rd := value(pipeline.data.RD)
      regFileIo.data := value(pipeline.data.RD_DATA)

      val hasTrapped = pipeline.serviceOption[TrapService] match {
        case Some(trapService) => trapService.hasTrapped(writeStage)
        case None => False
      }

      regFileIo.write :=
        value(pipeline.data.RD_TYPE) === RegisterType.GPR &&
          arbitration.isDone &&
          !hasTrapped
    }

    pipeline plug new Area {
      regFile.readIo <> readArea.regFileIo
      regFile.writeIo <> writeArea.regFileIo
    }
  }
}
