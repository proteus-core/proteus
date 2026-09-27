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
    cacheable: (UInt => Bool) = (_ => True),
    delay: Int = 0,
    enableMemoryTags: Boolean = true,
    enableSilentStoreElimination: Boolean = false,
    enableSilentStoreDefence: Boolean = false
)(implicit config: Config)
    extends Plugin[Pipeline] {
  private val byteIndexBits = log2Up(config.isa.xlen / 8)
  private val wordIndexBits = log2Up(config.memBusWidth / config.isa.xlen)
  private val setIndexBits = log2Up(sets)

  private case class CacheEntry() extends Bundle {
    val tag: UInt = UInt(config.isa.xlen - (byteIndexBits + wordIndexBits + setIndexBits) bits)
    val value: UInt = UInt(config.memBusWidth bits)
    val age: UInt = UInt(log2Up(ways) bits)
    val valid: Bool = Bool()
    val tags: UInt = if (config.memoryTagger) UInt(config.tagBusWidth bits) else null
  }

  private def getSetIndex(address: UInt): UInt = {
    address(byteIndexBits + wordIndexBits, log2Up(sets) bits)
  }

  private def getTagBits(address: UInt): UInt = {
    address(byteIndexBits + wordIndexBits + setIndexBits until config.isa.xlen)
  }

  // get all address bits that determine whether two addresses fall into the same cache line
  private def getSignificantBits(address: UInt): UInt = {
    U(getTagBits(address) ## getSetIndex(address))
  }

  private def connect(_s: Stage, internal: MemBus, external: MemBus): Unit = {
    val memoryTagger = pipeline.hasService[MemoryTaggerService]
    val cacheArea = pipeline plug new Area {
      private val idWidth = external.config.idWidth
      private val maxId = UInt(idWidth bits).maxValue.intValue()

      private val cache = Vec.fill(sets)(Vec.fill(ways)(RegInit(CacheEntry().getZero)))

      private val cacheHits = RegInit(UInt(config.isa.xlen bits).getZero)
      private val cacheMisses = RegInit(UInt(config.isa.xlen bits).getZero)
      private val issuedPrefetches = RegInit(UInt(config.isa.xlen bits).getZero)
      // number of cache misses that are not full misses because the load was already pending at the time of the miss
      private val forwardedCacheMisses = RegInit(UInt(config.isa.xlen bits).getZero)

      private val externalId = RegInit(UInt(external.config.idWidth bits).getZero)

      private val storeInCycle = Bool()
      storeInCycle := False

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
      private val cmdBuffer = Reg(MemBusCmd(external.config))

      // rsp sending buffer
      private val sendingRsp = Bool()
      sendingRsp := False
      private val rspBuffer = Reg(MemBusRsp(internal.config))
      private val returningCache = Reg(Bool()).init(False)
      private val returningCacheAddress = RegInit(UInt(config.isa.xlen bits).getZero)
      private val returningCachePc =
        if (internal.config.includePcWire) RegInit(UInt(config.isa.xlen bits).getZero) else null

      // Delay cache response with a fixed delay
      // Note that a minimal delay of 1 clock cycle is required to prevent
      // combinatorial loops in case of multiple dbus filters.
      private val internalRspBuffer = Stream(MemBusRsp(internal.config))
      internal.rsp << internalRspBuffer.delay(delay)

      // initial state: not sending or acknowledging anything
      internalRspBuffer.valid := False
      internalRspBuffer.payload.assignDontCare()
      internal.cmd.ready := False
      external.cmd.valid := False
      external.cmd.payload.assignDontCare()
      external.rsp.ready := False

      sendingImmediateCmd := False

      private case class InternalForwardingEntry() extends Bundle {
        val valid: Bool = Bool()
        val internalId: UInt = UInt(internal.config.idWidth bits)
        val addressOffset: UInt = UInt(byteIndexBits + wordIndexBits bits)
        val pc: UInt = if (internal.config.includePcWire) UInt(config.isa.xlen bits) else null
      }

      private case class OutstandingTracker() extends Bundle {
        val address: UInt = UInt(config.isa.xlen bits)
        val storeInvalidated: Bool = Bool()
        val pending: Bool = Bool()
        val internalForwarding: Vec[InternalForwardingEntry] =
          Vec.fill(1 << internal.config.idWidth)(InternalForwardingEntry())
      }

      private val outstandingLoads = Vec.fill(maxId + 1)(RegInit(OutstandingTracker().getZero))

      private val outstandingPrefetches = UInt((idWidth + 1) bits)
      outstandingPrefetches := outstandingLoads.sCount(load =>
        load.pending && !load.internalForwarding.sExist(entry => entry.valid)
      )

      private def forwardRspToInternal(internalForwardingEntry: InternalForwardingEntry): Unit = {
        sendingRsp := True
        internalRspBuffer.valid := True

        internalRspBuffer.rdata := external.rsp.rdata
        if (memoryTagger && enableMemoryTags) internalRspBuffer.ruser := external.rsp.ruser
        internalRspBuffer.id := internalForwardingEntry.internalId
        when(internalRspBuffer.ready) {
          // set the bit to 0 once it has been forwarded
          internalForwardingEntry.valid := False
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
          if (memoryTagger && enableMemoryTags) cache(setIndex)(way).tags := external.rsp.ruser
          increaseAgesUpTo(setIndex, ways - 1)
        }
        external.rsp.ready := True
      }

      // handling an incoming result from the memory
      when(external.rsp.valid) {
        val address = outstandingLoads(external.rsp.id).address
        val (forwardResult, entryIdx) =
          outstandingLoads(external.rsp.id).internalForwarding.sFindFirst(entry => entry.valid)

        prefetcher foreach { pref =>
          when(!forwardResult) {
            // inform prefetcher of prefetch response
            pref.notifyPrefetchResponse(
              address,
              external.rsp.rdata,
              external.rsp.id,
              external.rsp.ruser
            )
          } elsewhen (internalRspBuffer.ready) {
            // inform prefetcher of load response
            pref.notifyLoadResponse(
              (address(
                config.isa.xlen - 1 downto byteIndexBits + wordIndexBits
              ) ## outstandingLoads(external.rsp.id)
                .internalForwarding(entryIdx)
                .addressOffset).asUInt,
              outstandingLoads(external.rsp.id).internalForwarding(entryIdx).pc,
              external.rsp.rdata,
              cacheHit = false,
              external.rsp.ruser
            )
          }
        }

        when(!forwardResult) {
          // store result in cache without forwarding
          insertRspInCache(address)
        } otherwise {
          // forward result and store in cache
          forwardRspToInternal(outstandingLoads(external.rsp.id).internalForwarding(entryIdx))
          when(
            // when there is only one id left to forward, put result in cache and inform external bus we are done
            internalRspBuffer.ready && outstandingLoads(external.rsp.id).internalForwarding.sCount(
              entry => entry.valid
            ) === 1
          ) {
            insertRspInCache(address)
          }
        }
      }

      private def returnFromCache(cacheLine: CacheEntry, address: UInt): Unit = {
        // result served from cache
        when(!returningCache) {
          internal.cmd.ready := True
          rspBuffer.id := internal.cmd.id
          rspBuffer.rdata := cacheLine.value
          if (memoryTagger && enableMemoryTags) rspBuffer.ruser := cacheLine.tags
          when(!sendingRsp) {
            internalRspBuffer.valid := True
            internalRspBuffer.id := internal.cmd.id
            internalRspBuffer.rdata := cacheLine.value
            if (memoryTagger && enableMemoryTags) internalRspBuffer.ruser := cacheLine.tags
            when(!internalRspBuffer.ready) {
              returningCache := True
              returningCacheAddress := address
              if (internal.config.includePcWire) {
                returningCachePc := internal.cmd.pc
              }
            } otherwise {
              cacheHits := cacheHits + 1
              prefetcher foreach { pref =>
                // inform prefetcher of load response
                pref.notifyLoadResponse(
                  address,
                  internal.cmd.pc,
                  cacheLine.value,
                  cacheHit = true,
                  cacheLine.tags
                )
              }
            }
          } otherwise {
            returningCache := True
            returningCacheAddress := address
            if (internal.config.includePcWire) {
              returningCachePc := internal.cmd.pc
            }
          }
        }
        // if buffer is currently full, we do not ack the cmd, it will stay on the bus for the next cycle
      }

      when(returningCache && !sendingRsp) {
        // when not forwarding rsp but have a stored cache hit, return that
        internalRspBuffer.valid := True
        internalRspBuffer.payload := rspBuffer
        when(internalRspBuffer.ready) {
          returningCache := False
          returningCacheAddress := 0
          if (internal.config.includePcWire) {
            returningCachePc := 0
          }

          cacheHits := cacheHits + 1
          prefetcher foreach { pref =>
            // inform prefetcher of load response
            pref.notifyLoadResponse(
              returningCacheAddress,
              returningCachePc,
              rspBuffer.rdata,
              cacheHit = true,
              rspBuffer.ruser
            )
          }
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
          if (external.config.includePcWire) {
            external.cmd.pc := internal.cmd.pc
          }

          if (internal.config.readWrite) {
            external.cmd.write := internal.cmd.write
            external.cmd.wdata := internal.cmd.wdata
            external.cmd.wmask := internal.cmd.wmask
            if (memoryTagger && enableMemoryTags) external.cmd.wuser := internal.cmd.wuser

            when(!internal.cmd.write) {
              outstandingLoads(externalId).address := internal.cmd.address
              outstandingLoads(externalId).pending := True
              outstandingLoads(externalId).internalForwarding := outstandingLoads(
                externalId
              ).internalForwarding.getZero
              outstandingLoads(externalId).internalForwarding(0).valid := True
              outstandingLoads(externalId).internalForwarding(0).internalId := internal.cmd.id
              outstandingLoads(externalId).internalForwarding(0).addressOffset := internal.cmd
                .address(byteIndexBits + wordIndexBits - 1 downto 0)
              if (internal.config.includePcWire) {
                outstandingLoads(externalId).internalForwarding(0).pc := internal.cmd.pc
              }
              externalId := externalId + 1
            }
          } else {
            outstandingLoads(externalId).address := internal.cmd.address
            outstandingLoads(externalId).pending := True
            outstandingLoads(externalId).internalForwarding := outstandingLoads(
              externalId
            ).internalForwarding.getZero
            outstandingLoads(externalId).internalForwarding(0).valid := True
            outstandingLoads(externalId).internalForwarding(0).internalId := internal.cmd.id
            outstandingLoads(externalId).internalForwarding(0).addressOffset := internal.cmd
              .address(byteIndexBits + wordIndexBits - 1 downto 0)
            if (internal.config.includePcWire) {
              outstandingLoads(externalId).internalForwarding(0).pc := internal.cmd.pc
            }
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
            val prefetchAddress = pref.getNextPrefetchTarget(externalId)

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
                issuedPrefetches := issuedPrefetches + 1
                externalId := externalId + 1

                external.cmd.valid := True
                external.cmd.address := prefetchAddress
                external.cmd.id := externalId
                if (external.config.includePcWire) {
                  external.cmd.pc := 0
                }
                cmdBuffer := external.cmd.payload

                outstandingLoads(externalId).address := prefetchAddress
                outstandingLoads(externalId).pending := True
                outstandingLoads(externalId).internalForwarding := outstandingLoads(
                  externalId
                ).internalForwarding.getZero

                when(!external.cmd.ready) {
                  sendingBufferedCmd := True
                }
              }
            }
          }
        }
      }

      private def getResult(address: UInt): Unit = {
        when(internal.cmd.ready) {
          // inform prefetcher of load request
          prefetcher foreach { pref =>
            pref.notifyLoadRequest(address, internal.cmd.pc)
          }
        }

        val targetWay = wayForAddress(address)
        val setIndex = getSetIndex(address)
        val cacheSet = cache(setIndex)
        val tagBits = getTagBits(address)

        when(targetWay.valid) {
          cacheSet(targetWay.payload).age := U(0).resized
          increaseAgesUpTo(setIndex, cacheSet(targetWay.payload).age)
          returnFromCache(cacheSet(targetWay.payload), address)
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
                val entryIdx = PriorityMux(load.internalForwarding.zipWithIndex.map {
                  case (entry, idx) => (!entry.valid, U(idx, internal.config.idWidth bits))
                })
                load.internalForwarding(entryIdx).valid := True
                load.internalForwarding(entryIdx).internalId := internal.cmd.id
                load.internalForwarding(entryIdx).addressOffset := address(
                  byteIndexBits + wordIndexBits - 1 downto 0
                )
                if (internal.config.includePcWire) {
                  load.internalForwarding(entryIdx).pc := internal.cmd.pc
                }
                cacheMisses := cacheMisses + 1
                forwardedCacheMisses := forwardedCacheMisses + 1
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
            val performWrite = Bool()
            performWrite := True

            storeInCycle := True
            // write command: invalidates line and forwards to external bus
            for (i <- 0 until ways) {
              when(cache(indexBits)(i).tag === tagBits) {
                val bitMask = Utils.byteMaskToBitMask(internal.cmd.wmask).asUInt
                val isSilent =
                  if (enableSilentStoreElimination)
                    ((internal.cmd.wdata & bitMask) === (cache(indexBits)(
                      i
                    ).value & bitMask)) && cache(indexBits)(i).valid
                  else False
                val isTainted = Bool()
                if (config.memoryTagger && enableMemoryTags && enableSilentStoreDefence) {
                  val userMask =
                    internal.cmd.wmask.subdivideIn(config.tagGranularity / 8 bits).map(_.orR).asBits
                  val tag = cache(indexBits)(i).tags.asBits & userMask
                  isTainted := internal.cmd.wuser.orR || tag.orR
                } else {
                  isTainted := False
                }
                performWrite := !isSilent || isTainted

                when(performWrite) {
                  cache(indexBits)(i).valid := False
                }
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

            when(performWrite) {
              initiateCmdForwarding()
            } otherwise {
              internal.cmd.ready := True
            }
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
