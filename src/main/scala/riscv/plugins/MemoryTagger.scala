package riscv.plugins

import riscv._
import spinal.core._

class MemoryTagger(implicit config: Config)
    extends Plugin[DynamicPipeline]
    with MemoryTaggerService {

  object Data {
    object RAW_BUS_TAG extends PipelineData(UInt(config.xlen / config.tagGranularity bits))
  }

  override def busTag: PipelineData[UInt] = Data.RAW_BUS_TAG

  override def setup(): Unit = {
    assert (pipeline.hasService[PipelineTaintService], "Taint tracking required for memory tagger")

    pipeline.service[DecoderService].configure { dConfig =>
      dConfig.addDefault(
        Map(
          Data.RAW_BUS_TAG -> U(0, config.xlen / config.tagGranularity bits)
        )
      )
    }
  }

  override def build(): Unit = {
    val lsu = pipeline.service[LsuService]

    for (stage <- lsu.loadStages) {
      stage plug new Area {
        val accessWidth = lsu.width(stage)
        val address = lsu.address(stage)

        val fullTag = UInt(config.xlen / config.tagGranularity bits)
        fullTag := stage.output(Data.RAW_BUS_TAG)

        val resultTag = Bool()
        resultTag := fullTag.orR

        when(accessWidth === LsuAccessWidth.H || accessWidth === LsuAccessWidth.B) {
          config.tagGranularity match {
            case 64 =>
            case 32 => resultTag := fullTag(if (config.xlen == 64) address(2).asUInt else U(0))
            case 16 => resultTag := fullTag(if (config.xlen == 64) address(2 downto 1) else address(1).asUInt)
            case 8 =>
              val tagOffset = if (config.xlen == 64) address(2 downto 0) else address(1 downto 0)
              when(accessWidth === LsuAccessWidth.B) {
                resultTag := fullTag(tagOffset)
              } otherwise {
                resultTag := fullTag(tagOffset, 2 bits).orR
              }
          }
        }

        if (config.xlen == 64) {
          when(accessWidth === LsuAccessWidth.W) {
            config.tagGranularity match {
              case 64 =>
              case 32 => resultTag := fullTag(address(2).asUInt)
              case 16 => resultTag := fullTag(address(2).asUInt << 1, 2 bits).orR
              case 8 => resultTag := fullTag(address(2).asUInt << 2, 4 bits).orR
            }
          }
        }
        pipeline.service[PipelineTaintService].tainted(stage) := resultTag
      }
    }
  }
}
