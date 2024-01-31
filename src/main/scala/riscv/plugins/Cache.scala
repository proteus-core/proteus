package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib._

class Cache(
    sets: Int,
    ways: Int,
    busFilter: ((Stage, MemBus, MemBus) => Unit) => Unit,
    prefetcher: Option[PrefetchService] = None
)(implicit config: Config)
    extends Plugin[Pipeline] {
  private val byteIndexBits = log2Up(config.xlen / 8)
  private val wordIndexBits = log2Up(config.memBusWidth / config.xlen)
  private val setIndexBits = log2Up(sets)

  private case class CacheEntry() extends Bundle {
    val tag: UInt = UInt(config.xlen - (byteIndexBits + wordIndexBits + setIndexBits) bits)
    val value: UInt = UInt(config.memBusWidth bits)
    val age: UInt = UInt(log2Up(ways) bits)
    val valid: Bool = Bool()
  }

  private def getSetIndex(address: UInt): UInt = {
    address(byteIndexBits + wordIndexBits, log2Up(sets) bits)
  }

  private def getTagBits(address: UInt): UInt = {
    address(byteIndexBits + wordIndexBits + setIndexBits until config.xlen)
  }

  // get all address bits that determine whether two addresses fall into the same cache line
  private def getSignificantBits(address: UInt): UInt = {
    U(getTagBits(address) ## getSetIndex(address))
  }

  private def connect(_s: Stage, internal: MemBus, external: MemBus): Unit = {
    val cacheArea = pipeline plug new Area {
      private val idWidth = internal.config.idWidth
      private val maxId = UInt(idWidth bits).maxValue.intValue()

      private val cache = Vec.fill(sets)(Vec.fill(ways)(RegInit(CacheEntry().getZero)))

      private val cacheHits = RegInit(UInt(config.xlen bits).getZero)
      private val cacheMisses = RegInit(UInt(config.xlen bits).getZero)

      private val pendingPrefetch = RegInit(Flow(UInt(idWidth bits)).setIdle())

      private def oldestWay(set: UInt): UInt = {
        val result = UInt(log2Up(ways) bits)
        result := 0
        for (i <- 0 until ways) {
          when(cache(set)(i).age === ways - 1 || !cache(set)(i).valid) {
            result := i
          }
        }
        result
      }

      private def increaseAgesUpTo(set: UInt, oldest: UInt): Unit = {
        for (i <- 0 until ways) {
          when(cache(set)(i).age < oldest) {
            cache(set)(i).age := cache(set)(i).age + 1
          }
        }
      }

      private def decreaseAgesUntil(set: UInt, youngest: UInt): Unit = {
        for (i <- 0 until ways) {
          when(cache(set)(i).age > youngest) {
            cache(set)(i).age := cache(set)(i).age - 1
          }
        }
      }

      private val sendingImmediateCmd = Bool()
      private val sendingBufferedCmd = Reg(Bool()).init(False)
      private val cmdBuffer = Reg(MemBusCmd(internal.config))

      // rsp sending buffer
      private val sendingRsp = Bool()
      sendingRsp := False
      private val rspBuffer = Reg(MemBusRsp(internal.config))
      private val returningCache = Reg(Bool()).init(False)

      // initial state: not sending or acknowledging anything
      internal.rsp.valid := False
      internal.rsp.payload.assignDontCare()
      internal.cmd.ready := False
      external.cmd.valid := False
      external.cmd.payload.assignDontCare()
      external.rsp.ready := False

      sendingImmediateCmd := False

      private case class OutstandingTracker() extends Bundle {
        val address: UInt = UInt(config.xlen bits)
        val storeInvalidated: Bool = Bool()
        val pending: Bool = Bool()
      }

      private val outstandingLoads = Vec.fill(maxId + 1)(RegInit(OutstandingTracker().getZero))

      private def forwardRspToInternal(): Unit = {
        sendingRsp := True
        internal.rsp.valid := True
        internal.rsp.payload := external.rsp.payload
      }

      private def insertRspInCache(address: UInt): Unit = {
        val setIndex = getSetIndex(address)
        val tag = getTagBits(address)

        outstandingLoads(external.rsp.id).pending := False
        outstandingLoads(external.rsp.id).storeInvalidated := False
        when(!outstandingLoads(external.rsp.id).storeInvalidated) {
          val way = oldestWay(setIndex)
          cache(setIndex)(way).valid := True
          cache(setIndex)(way).tag := tag
          cache(setIndex)(way).value := external.rsp.payload.rdata
          cache(setIndex)(way).age := U(0).resized
          increaseAgesUpTo(setIndex, ways - 1)
        }
        external.rsp.ready := True
      }

      // handling an incoming result from the memory
      when(external.rsp.valid) {
        val address = outstandingLoads(external.rsp.id).address

        prefetcher match {
          case None =>
            forwardRspToInternal()
            when(internal.rsp.ready) {
              insertRspInCache(address)
            }
          case Some(_) =>
            when(pendingPrefetch.valid && pendingPrefetch.payload === external.rsp.id) {
              // handle prefetch response without forwarding
              pendingPrefetch.setIdle()
              insertRspInCache(address)
            } otherwise {
              forwardRspToInternal()
              when(internal.rsp.ready) {
                insertRspInCache(address)
              }
            }
        }
      }

      private def returnFromCache(cacheLine: CacheEntry): Unit = {
        // result served from cache
        when(!returningCache) {
          internal.cmd.ready := True
          rspBuffer.id := internal.cmd.id
          rspBuffer.rdata := cacheLine.value
          when(!sendingRsp) {
            internal.rsp.valid := True
            internal.rsp.id := internal.cmd.id
            internal.rsp.rdata := cacheLine.value
            when(!internal.rsp.ready) {
              returningCache := True
            }
          } otherwise {
            returningCache := True
          }
        }
        // if buffer is currently full, we do not ack the cmd, it will stay on the bus for the next cycle
      }

      when(returningCache && !sendingRsp) {
        // when not forwarding rsp but have a stored cache hit, return that
        internal.rsp.valid := True
        internal.rsp.payload := rspBuffer
        when(internal.rsp.ready) {
          returningCache := False
        }
      }

      private def initiateCmdForwarding(): Unit = {
        when(!sendingBufferedCmd) {
          sendingImmediateCmd := True
          cacheMisses := cacheMisses + 1
          internal.cmd.ready := True
          external.cmd.valid := True

          cmdBuffer := internal.cmd.payload
          external.cmd.payload := internal.cmd.payload

          if (internal.config.readWrite) {
            when(!internal.cmd.write) {
              outstandingLoads(internal.cmd.id).address := internal.cmd.address
              outstandingLoads(internal.cmd.id).pending := True
            }
          } else {
            outstandingLoads(internal.cmd.id).address := internal.cmd.address
            outstandingLoads(internal.cmd.id).pending := True
          }
          when(!external.cmd.ready) {
            sendingBufferedCmd := True
          }
        }
      }

      when(sendingBufferedCmd) {
        external.cmd.valid := True
        external.cmd.payload := cmdBuffer
        when(external.cmd.ready) {
          sendingBufferedCmd := False
        }
      }

      private def wayForAddress(address: UInt): Flow[UInt] = {
        val set = cache(getSetIndex(address))
        val tag = getTagBits(address)
        val result = Flow(UInt(log2Up(ways) bits))
        result.setIdle()
        for (i <- 0 until ways) {
          when(set(i).valid && set(i).tag === tag) {
            result.push(i)
          }
        }
        result
      }

      prefetcher foreach { pref =>
        when(!sendingBufferedCmd && !sendingImmediateCmd && !pendingPrefetch.valid) {
          val prefetchAddress = pref.getPrefetchTarget

          val targetWay = wayForAddress(prefetchAddress)

          val emptySlot = Flow(UInt(idWidth bits)).setIdle()
          // send a prefetch command on the unused external bus if there's an unused ID and no pending prefetch
          for (i <- 0 until outstandingLoads.length) {
            when(!outstandingLoads(i).pending) {
              emptySlot.push(i)
            }
          }

          when(emptySlot.valid && !targetWay.valid) {
            pendingPrefetch := emptySlot

            external.cmd.valid := True
            external.cmd.address := prefetchAddress
            external.cmd.id := emptySlot.payload
            cmdBuffer := external.cmd

            outstandingLoads(emptySlot.payload).address := prefetchAddress
            outstandingLoads(emptySlot.payload).pending := True

            when(!external.cmd.ready) {
              sendingBufferedCmd := True
            }
          }
        }
      }

      private def getResult(address: UInt): Unit = {
        prefetcher foreach { pref =>
          pref.updatePrefetcherState(internal.cmd.address, byteIndexBits + wordIndexBits)
        }

        when(!outstandingLoads(internal.cmd.id).pending) {
          val targetWay = wayForAddress(address)
          val setIndex = getSetIndex(address)
          val cacheSet = cache(setIndex)
          val tagBits = getTagBits(address)

          when(targetWay.valid) {
            cacheSet(targetWay.payload).age := U(0).resized
            increaseAgesUpTo(setIndex, cacheSet(targetWay.payload).age)
            returnFromCache(cacheSet(targetWay.payload))
            cacheHits := cacheHits + 1
          } otherwise {
            val alreadyPending = False
            for (i <- 0 until outstandingLoads.length) {
              val load = outstandingLoads(i)
              when(
                getSignificantBits(load.address) === U(
                  tagBits ## setIndex
                ) && load.pending && !load.storeInvalidated
              ) {
                alreadyPending := True
              }
            }
            when(!alreadyPending) {
              // forward cmd to external bus if there's no pending load for the same cache line
              initiateCmdForwarding()
            }
          }
        }
      }

      // handling a load/write request from the CPU
      when(internal.cmd.valid) {
        val indexBits = getSetIndex(internal.cmd.address)
        val tagBits = getTagBits(internal.cmd.address)

        if (internal.config.readWrite) {
          when(internal.cmd.write) {
            // write command: invalidates line and forwards to external bus
            for (i <- 0 until ways) {
              when(cache(indexBits)(i).tag === tagBits) {
                cache(indexBits)(i).valid := False
                cache(indexBits)(i).age := ways - 1
                decreaseAgesUntil(indexBits, cache(indexBits)(i).age)
              }
            }

            for (i <- 0 until outstandingLoads.length) {
              when(
                getSignificantBits(outstandingLoads(i).address) === getSignificantBits(
                  internal.cmd.address
                ) && outstandingLoads(i).pending
              ) {
                outstandingLoads(i).storeInvalidated := True
              }
            }

            initiateCmdForwarding()
            // if currently forwarding a cmd, we do not ack it, it will stay on the bus for the next cycle
          } otherwise {
            getResult(internal.cmd.address)
          }
        } else {
          getResult(internal.cmd.address)
        }
      }
    }
    cacheArea.setName("cache_" + external.name)
  }

  override def build(): Unit = {
    busFilter(connect)
  }
}
