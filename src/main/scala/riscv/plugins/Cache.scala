package riscv.plugins

import riscv._
import spinal.core._
import spinal.lib._

class Cache(
    sets: Int,
    ways: Int,
    busFilter: ((Stage, MemBus, MemBus) => Unit) => Unit,
    prefetcher: Option[PrefetchService] = None,
    maxPrefetches: Int = 1,
    cacheable: (UInt => Bool) = (_ => True)
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
      private val forwardedLoads = RegInit(UInt(config.xlen bits).getZero)

      private val externalId = RegInit(UInt(external.config.idWidth bits).getZero)

      private val storeInCycle = Bool()
      storeInCycle := False

      private val outstandingPrefetches = RegInit(UInt(log2Up(maxPrefetches + 1) bits).getZero)
      private val incrementOutstandingPrefetches = Bool()
      private val decrementOutstandingPrefetches = Bool()
      incrementOutstandingPrefetches := False
      decrementOutstandingPrefetches := False

      // this logic is to avoid problems when incrementing and decrementing in the same cycle
      when(incrementOutstandingPrefetches && !decrementOutstandingPrefetches) {
        outstandingPrefetches := outstandingPrefetches + 1
      } elsewhen (!incrementOutstandingPrefetches && decrementOutstandingPrefetches) {
        outstandingPrefetches := outstandingPrefetches - 1
      }

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
      private val alreadySendingRsp = Reg(Bool()).init(False)
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
        val isPrefetch: Bool = Bool()
        val internalIds: Bits = Bits(1 << internal.config.idWidth bits)
      }

      private val outstandingLoads = Vec.fill(maxId + 1)(RegInit(OutstandingTracker().getZero))

      private def forwardRspToInternal(): Unit = {
        sendingRsp := True
        internal.rsp.valid := True

        internal.rsp.rdata := external.rsp.rdata
        // the index of 1's in internalIds indicate to which internal ids the response should be forwarded
        val internalId = OHToUInt(OHMasking.first(outstandingLoads(external.rsp.id).internalIds))
        internal.rsp.id := internalId
        when(internal.rsp.ready) {
          // set the bit to 0 once it has been forwarded
          outstandingLoads(external.rsp.id).internalIds(internalId) := False
        }
      }

      private def insertRspInCache(address: UInt): Unit = {
        val setIndex = getSetIndex(address)
        val tag = getTagBits(address)

        outstandingLoads(external.rsp.id).pending := False
        outstandingLoads(external.rsp.id).storeInvalidated := False
        // make sure we don't insert values that have been overwritten with a store
        // either before or in the current cycle
        when(
          !outstandingLoads(external.rsp.id).storeInvalidated &&
            cacheable(address) &&
            !(storeInCycle &&
              getSignificantBits(address) === getSignificantBits(internal.cmd.address))
        ) {
          val way = oldestWay(setIndex)
          cache(setIndex)(way).valid := True
          cache(setIndex)(way).tag := tag
          cache(setIndex)(way).value := external.rsp.rdata
          cache(setIndex)(way).age := U(0).resized
          increaseAgesUpTo(setIndex, ways - 1)
        }
        external.rsp.ready := True
      }

      // handling an incoming result from the memory
      when(external.rsp.valid) {
        val address = outstandingLoads(external.rsp.id).address

        when(!alreadySendingRsp) {
          prefetcher foreach { pref =>
            when(outstandingLoads(external.rsp.id).isPrefetch) {
              // inform prefetcher of prefetch response from memory
              pref.notifyPrefetchResponseFromMemory(address, external.rsp.rdata)
              // subscract 1 from outstandingPrefetches
              decrementOutstandingPrefetches := True
            } otherwise {
              // inform prefetcher of load response from memory
              pref.notifyLoadResponseFromMemory(address, external.rsp.rdata)
            }
          }
        }

        when(outstandingLoads(external.rsp.id).internalIds === 0) {
          // store result in cache without forwarding
          insertRspInCache(address)
        } otherwise {
          // forward result and store in cache
          forwardRspToInternal()
          when(
            // when there is only one id left to forward, put result in cache and inform external bus we are done
            internal.rsp.ready && CountOne(outstandingLoads(external.rsp.id).internalIds) === 1
          ) {
            insertRspInCache(address)
            alreadySendingRsp := False
          } otherwise {
            alreadySendingRsp := True
          }
        }
      }

      private def returnFromCache(cacheLine: CacheEntry): Unit = {
        // result served from cache
        when(!returningCache) {
          cacheHits := cacheHits + 1
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
          internal.cmd.ready := True
          external.cmd.valid := True

          cmdBuffer := external.cmd.payload

          external.cmd.address := internal.cmd.address
          external.cmd.id := externalId

          if (internal.config.readWrite) {
            external.cmd.write := internal.cmd.write
            external.cmd.wdata := internal.cmd.wdata
            external.cmd.wmask := internal.cmd.wmask

            when(!internal.cmd.write) {
              outstandingLoads(externalId).address := internal.cmd.address
              outstandingLoads(externalId).pending := True
              outstandingLoads(externalId).isPrefetch := False
              outstandingLoads(externalId).internalIds := B(0).resized
              outstandingLoads(externalId).internalIds(internal.cmd.id) := True
              externalId := externalId + 1
            }
          } else {
            outstandingLoads(externalId).address := internal.cmd.address
            outstandingLoads(externalId).pending := True
            outstandingLoads(externalId).isPrefetch := False
            outstandingLoads(externalId).internalIds := B(0).resized
            outstandingLoads(externalId).internalIds(internal.cmd.id) := True
            externalId := externalId + 1
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
        when(
          !sendingBufferedCmd && !sendingImmediateCmd && outstandingPrefetches < maxPrefetches && pref.hasPrefetchTarget
        ) {
          when(!outstandingLoads(externalId).pending) {
            // at this point the cache is ready to send a prefetch command to the memory
            // getNextPrefetchTarget should not be called before the cache is ready to send the command
            // otherwise the prefetch may get lost
            val prefetchAddress = pref.getNextPrefetchTarget

            when(cacheable(prefetchAddress)) {
              val targetWay = wayForAddress(prefetchAddress)
              val setIndex = getSetIndex(prefetchAddress)
              val tagBits = getTagBits(prefetchAddress)

              val alreadyPending = False

              // find out if a load request for the given address is already pending
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
              when(!targetWay.valid && !alreadyPending) {
                // add 1 to outstandingPrefetches
                incrementOutstandingPrefetches := True

                externalId := externalId + 1

                external.cmd.valid := True
                external.cmd.address := prefetchAddress
                external.cmd.id := externalId
                cmdBuffer := external.cmd.payload

                outstandingLoads(externalId).address := prefetchAddress
                outstandingLoads(externalId).pending := True
                outstandingLoads(externalId).internalIds := B(0).resized
                outstandingLoads(externalId).isPrefetch := True

                when(!external.cmd.ready) {
                  sendingBufferedCmd := True
                }
              }
            }
          }
        }
      }

      private def getResult(address: UInt): Unit = {
        // inform prefetcher of load request
        prefetcher foreach { pref =>
          pref.notifyLoadRequest(address)
        }

        val targetWay = wayForAddress(address)
        val setIndex = getSetIndex(address)
        val cacheSet = cache(setIndex)
        val tagBits = getTagBits(address)

        when(targetWay.valid) {
          cacheSet(targetWay.payload).age := U(0).resized
          increaseAgesUpTo(setIndex, cacheSet(targetWay.payload).age)
          returnFromCache(cacheSet(targetWay.payload))
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
              // if the load is already pending but result not yet received: mark it to be forwarded + increase cache misses
              when(
                !(external.rsp.valid && getSignificantBits(load.address) === getSignificantBits(
                  outstandingLoads(external.rsp.id).address
                ))
              ) {
                load.internalIds(internal.cmd.id) := True
                cacheMisses := cacheMisses + 1
                forwardedLoads := forwardedLoads + 1
                internal.cmd.ready := True
              }
            }
          }
          // there's no pending load for the same cache line and the bus id is free:
          when(!alreadyPending && !outstandingLoads(externalId).pending) {
            // initiateCmdForwarding() will only go through when !sendingBufferedCmd,
            // also add this check here to only increase cache misses once per request
            when(!sendingBufferedCmd) {
              // increase cache misses
              cacheMisses := cacheMisses + 1
            }

            // forward cmd to external bus
            initiateCmdForwarding()
          }
        }
      }

      // handling a load/write request from the CPU
      when(internal.cmd.valid) {
        val indexBits = getSetIndex(internal.cmd.address)
        val tagBits = getTagBits(internal.cmd.address)

        if (internal.config.readWrite) {
          when(internal.cmd.write) {
            storeInCycle := True
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
