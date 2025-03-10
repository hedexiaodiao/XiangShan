/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.mem

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.cache._
import xiangshan.cache.{DCacheWordIO, DCacheLineIO, MemoryOpConstants}
import xiangshan.backend.roq.{RoqLsqIO, RoqPtr}
import difftest._
import device.RAMHelper

class SqPtr(implicit p: Parameters) extends CircularQueuePtr[SqPtr](
  p => p(XSCoreParamsKey).StoreQueueSize
){
  override def cloneType = (new SqPtr).asInstanceOf[this.type]
}

object SqPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): SqPtr = {
    val ptr = Wire(new SqPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

class SqEnqIO(implicit p: Parameters) extends XSBundle {
  val canAccept = Output(Bool())
  val lqCanAccept = Input(Bool())
  val needAlloc = Vec(RenameWidth, Input(Bool()))
  val req = Vec(RenameWidth, Flipped(ValidIO(new MicroOp)))
  val resp = Vec(RenameWidth, Output(new SqPtr))
}

// Store Queue
class StoreQueue(implicit p: Parameters) extends XSModule with HasDCacheParameters with HasCircularQueuePtrHelper {
  val io = IO(new Bundle() {
    val enq = new SqEnqIO
    val brqRedirect = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())
    val storeIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle))) // store addr, data is not included
    val storeDataIn = Vec(StorePipelineWidth, Flipped(Valid(new StoreDataBundle))) // store data, send to sq from rs
    val sbuffer = Vec(StorePipelineWidth, Decoupled(new DCacheWordReq)) // write commited store to sbuffer
    val mmioStout = DecoupledIO(new ExuOutput) // writeback uncached store
    val forward = Vec(LoadPipelineWidth, Flipped(new PipeLoadForwardQueryIO))
    val roq = Flipped(new RoqLsqIO)
    val uncache = new DCacheWordIO
    // val refill = Flipped(Valid(new DCacheLineReq ))
    val exceptionAddr = new ExceptionAddrIO
    val sqempty = Output(Bool())
    val issuePtrExt = Output(new SqPtr) // used to wake up delayed load/store
    val storeIssue = Vec(StorePipelineWidth, Flipped(Valid(new ExuInput))) // used to update issuePtrExt
    val sqFull = Output(Bool())
  })

  println("StoreQueue: size:" + StoreQueueSize)

  // data modules
  val uop = Reg(Vec(StoreQueueSize, new MicroOp))
  // val data = Reg(Vec(StoreQueueSize, new LsqEntry))
  val dataModule = Module(new SQDataModule(StoreQueueSize, numRead = StorePipelineWidth, numWrite = StorePipelineWidth, numForward = StorePipelineWidth))
  dataModule.io := DontCare
  val paddrModule = Module(new SQPaddrModule(StoreQueueSize, numRead = StorePipelineWidth, numWrite = StorePipelineWidth, numForward = StorePipelineWidth))
  paddrModule.io := DontCare
  val vaddrModule = Module(new SyncDataModuleTemplate(UInt(VAddrBits.W), StoreQueueSize, numRead = 1, numWrite = StorePipelineWidth))
  vaddrModule.io := DontCare

  // state & misc
  val allocated = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // sq entry has been allocated
  val addrvalid = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // non-mmio addr is valid
  val datavalid = RegInit(VecInit(List.fill(StoreQueueSize)(false.B))) // non-mmio data is valid
  val allvalid  = VecInit((0 until StoreQueueSize).map(i => addrvalid(i) && datavalid(i))) // non-mmio data & addr is valid
  val issued = Reg(Vec(StoreQueueSize, Bool())) // inst has been issued by rs
  val commited = Reg(Vec(StoreQueueSize, Bool())) // inst has been commited by roq
  val pending = Reg(Vec(StoreQueueSize, Bool())) // mmio pending: inst is an mmio inst, it will not be executed until it reachs the end of roq
  val mmio = Reg(Vec(StoreQueueSize, Bool())) // mmio: inst is an mmio inst

  // ptr
  require(StoreQueueSize > RenameWidth)
  val enqPtrExt = RegInit(VecInit((0 until RenameWidth).map(_.U.asTypeOf(new SqPtr))))
  val deqPtrExt = RegInit(VecInit((0 until StorePipelineWidth).map(_.U.asTypeOf(new SqPtr))))
  val cmtPtrExt = RegInit(VecInit((0 until CommitWidth).map(_.U.asTypeOf(new SqPtr))))
  val issuePtrExt = RegInit(0.U.asTypeOf(new SqPtr))
  val validCounter = RegInit(0.U(log2Ceil(LoadQueueSize + 1).W))
  val allowEnqueue = RegInit(true.B)

  val enqPtr = enqPtrExt(0).value
  val deqPtr = deqPtrExt(0).value
  val cmtPtr = cmtPtrExt(0).value

  val deqMask = UIntToMask(deqPtr, StoreQueueSize)
  val enqMask = UIntToMask(enqPtr, StoreQueueSize)

  val commitCount = RegNext(io.roq.scommit)

  // Read dataModule
  // deqPtrExtNext and deqPtrExtNext+1 entry will be read from dataModule
  // if !sbuffer.fire(), read the same ptr
  // if sbuffer.fire(), read next
  val deqPtrExtNext = WireInit(Mux(io.sbuffer(1).fire(),
    VecInit(deqPtrExt.map(_ + 2.U)),
    Mux(io.sbuffer(0).fire() || io.mmioStout.fire(),
      VecInit(deqPtrExt.map(_ + 1.U)),
      deqPtrExt
    )
  ))
  for (i <- 0 until StorePipelineWidth) {
    dataModule.io.raddr(i) := deqPtrExtNext(i).value
    paddrModule.io.raddr(i) := deqPtrExtNext(i).value
  }

  // no inst will be commited 1 cycle before tval update
  vaddrModule.io.raddr(0) := (cmtPtrExt(0) + commitCount).value

  /**
    * Enqueue at dispatch
    *
    * Currently, StoreQueue only allows enqueue when #emptyEntries > RenameWidth(EnqWidth)
    */
  io.enq.canAccept := allowEnqueue
  for (i <- 0 until RenameWidth) {
    val offset = if (i == 0) 0.U else PopCount(io.enq.needAlloc.take(i))
    val sqIdx = enqPtrExt(offset)
    val index = sqIdx.value
    when (io.enq.req(i).valid && io.enq.canAccept && io.enq.lqCanAccept && !(io.brqRedirect.valid || io.flush)) {
      uop(index) := io.enq.req(i).bits
      allocated(index) := true.B
      datavalid(index) := false.B
      addrvalid(index) := false.B
      issued(index) := false.B
      commited(index) := false.B
      pending(index) := false.B
    }
    io.enq.resp(i) := sqIdx
  }
  XSDebug(p"(ready, valid): ${io.enq.canAccept}, ${Binary(Cat(io.enq.req.map(_.valid)))}\n")

  /**
    * Update issuePtr when issue from rs
    */

  // update state bit issued
  for (i <- 0 until StorePipelineWidth) {
    when (io.storeIssue(i).valid) {
      issued(io.storeIssue(i).bits.uop.sqIdx.value) := true.B
    }
  }

  // update issuePtr
  val IssuePtrMoveStride = 4
  require(IssuePtrMoveStride >= 2)

  val issueLookupVec = (0 until IssuePtrMoveStride).map(issuePtrExt + _.U)
  val issueLookup = issueLookupVec.map(ptr => allocated(ptr.value) && issued(ptr.value) && ptr =/= enqPtrExt(0))
  val nextIssuePtr = issuePtrExt + PriorityEncoder(VecInit(issueLookup.map(!_) :+ true.B))
  issuePtrExt := nextIssuePtr

  when (io.brqRedirect.valid || io.flush) {
    issuePtrExt := Mux(
      isAfter(cmtPtrExt(0), deqPtrExt(0)),
      cmtPtrExt(0),
      deqPtrExtNext(0) // for mmio insts, deqPtr may be ahead of cmtPtr
    )
  }
  // send issuePtrExt to rs
  // io.issuePtrExt := cmtPtrExt(0)
  io.issuePtrExt := issuePtrExt

  /**
    * Writeback store from store units
    *
    * Most store instructions writeback to regfile in the previous cycle.
    * However,
    *   (1) For an mmio instruction with exceptions, we need to mark it as addrvalid
    * (in this way it will trigger an exception when it reaches ROB's head)
    * instead of pending to avoid sending them to lower level.
    *   (2) For an mmio instruction without exceptions, we mark it as pending.
    * When the instruction reaches ROB's head, StoreQueue sends it to uncache channel.
    * Upon receiving the response, StoreQueue writes back the instruction
    * through arbiter with store units. It will later commit as normal.
    */

  // Write addr to sq
  for (i <- 0 until StorePipelineWidth) {
    paddrModule.io.wen(i) := false.B
    dataModule.io.mask.wen(i) := false.B
    val stWbIndex = io.storeIn(i).bits.uop.sqIdx.value
    when (io.storeIn(i).fire()) {
      addrvalid(stWbIndex) := true.B//!io.storeIn(i).bits.mmio
      pending(stWbIndex) := io.storeIn(i).bits.mmio

      dataModule.io.mask.waddr(i) := stWbIndex
      dataModule.io.mask.wdata(i) := io.storeIn(i).bits.mask
      dataModule.io.mask.wen(i) := true.B

      paddrModule.io.waddr(i) := stWbIndex
      paddrModule.io.wdata(i) := io.storeIn(i).bits.paddr
      paddrModule.io.wen(i) := true.B

      mmio(stWbIndex) := io.storeIn(i).bits.mmio

      XSInfo("store addr write to sq idx %d pc 0x%x vaddr %x paddr %x mmio %x\n",
        io.storeIn(i).bits.uop.sqIdx.value,
        io.storeIn(i).bits.uop.cf.pc,
        io.storeIn(i).bits.vaddr,
        io.storeIn(i).bits.paddr,
        io.storeIn(i).bits.mmio
      )
    }
    // vaddrModule write is delayed, as vaddrModule will not be read right after write
    vaddrModule.io.waddr(i) := RegNext(stWbIndex)
    vaddrModule.io.wdata(i) := RegNext(io.storeIn(i).bits.vaddr)
    vaddrModule.io.wen(i) := RegNext(io.storeIn(i).fire())
  }

  // Write data to sq
  for (i <- 0 until StorePipelineWidth) {
    dataModule.io.data.wen(i) := false.B
    io.roq.storeDataRoqWb(i).valid := false.B
    io.roq.storeDataRoqWb(i).bits := DontCare
    val stWbIndex = io.storeDataIn(i).bits.uop.sqIdx.value
    when (io.storeDataIn(i).fire()) {
      datavalid(stWbIndex) := true.B

      dataModule.io.data.waddr(i) := stWbIndex
      dataModule.io.data.wdata(i) := genWdata(io.storeDataIn(i).bits.data, io.storeDataIn(i).bits.uop.ctrl.fuOpType(1,0))
      dataModule.io.data.wen(i) := true.B

      io.roq.storeDataRoqWb(i).valid := true.B
      io.roq.storeDataRoqWb(i).bits := io.storeDataIn(i).bits.uop.roqIdx

      XSInfo("store data write to sq idx %d pc 0x%x data %x -> %x\n",
        io.storeDataIn(i).bits.uop.sqIdx.value,
        io.storeDataIn(i).bits.uop.cf.pc,
        io.storeDataIn(i).bits.data,
        dataModule.io.data.wdata(i)
      )
    }
  }

  /**
    * load forward query
    *
    * Check store queue for instructions that is older than the load.
    * The response will be valid at the next cycle after req.
    */
  // check over all lq entries and forward data from the first matched store
  for (i <- 0 until LoadPipelineWidth) {
    io.forward(i).forwardMask := 0.U(8.W).asBools
    io.forward(i).forwardData := DontCare

    // Compare deqPtr (deqPtr) and forward.sqIdx, we have two cases:
    // (1) if they have the same flag, we need to check range(tail, sqIdx)
    // (2) if they have different flags, we need to check range(tail, LoadQueueSize) and range(0, sqIdx)
    // Forward1: Mux(same_flag, range(tail, sqIdx), range(tail, LoadQueueSize))
    // Forward2: Mux(same_flag, 0.U,                   range(0, sqIdx)    )
    // i.e. forward1 is the target entries with the same flag bits and forward2 otherwise
    val differentFlag = deqPtrExt(0).flag =/= io.forward(i).sqIdx.flag
    val forwardMask = io.forward(i).sqIdxMask
    // all addrvalid terms need to be checked
    val addrValidVec = WireInit(VecInit((0 until StoreQueueSize).map(i => addrvalid(i) && allocated(i))))
    val dataValidVec = WireInit(VecInit((0 until StoreQueueSize).map(i => datavalid(i))))
    val allValidVec = WireInit(VecInit((0 until StoreQueueSize).map(i => addrvalid(i) && datavalid(i) && allocated(i))))
    val canForward1 = Mux(differentFlag, ~deqMask, deqMask ^ forwardMask) & allValidVec.asUInt
    val canForward2 = Mux(differentFlag, forwardMask, 0.U(StoreQueueSize.W)) & allValidVec.asUInt
    val needForward = Mux(differentFlag, ~deqMask | forwardMask, deqMask ^ forwardMask)

    XSDebug(p"$i f1 ${Binary(canForward1)} f2 ${Binary(canForward2)} " +
      p"sqIdx ${io.forward(i).sqIdx} pa ${Hexadecimal(io.forward(i).paddr)}\n"
    )

    // do real fwd query (cam lookup in load_s1)
    dataModule.io.needForward(i)(0) := canForward1 & paddrModule.io.forwardMmask(i).asUInt
    dataModule.io.needForward(i)(1) := canForward2 & paddrModule.io.forwardMmask(i).asUInt

    paddrModule.io.forwardMdata(i) := io.forward(i).paddr

    // Forward result will be generated 1 cycle later (load_s2)
    io.forward(i).forwardMask := dataModule.io.forwardMask(i)
    io.forward(i).forwardData := dataModule.io.forwardData(i)

    // If addr match, data not ready, mark it as dataInvalid
    // load_s1: generate dataInvalid in load_s1 to set fastUop to
    io.forward(i).dataInvalidFast := (addrValidVec.asUInt & ~dataValidVec.asUInt & paddrModule.io.forwardMmask(i).asUInt & needForward).orR
    // load_s2
    io.forward(i).dataInvalid := RegNext(io.forward(i).dataInvalidFast)
  }

  /**
    * Memory mapped IO / other uncached operations
    *
    * States:
    * (1) writeback from store units: mark as pending
    * (2) when they reach ROB's head, they can be sent to uncache channel
    * (3) response from uncache channel: mark as datavalidmask.wen
    * (4) writeback to ROB (and other units): mark as writebacked
    * (5) ROB commits the instruction: same as normal instructions
    */
  //(2) when they reach ROB's head, they can be sent to uncache channel
  val s_idle :: s_req :: s_resp :: s_wb :: s_wait :: Nil = Enum(5)
  val uncacheState = RegInit(s_idle)
  switch(uncacheState) {
    is(s_idle) {
      when(io.roq.pendingst && pending(deqPtr) && allocated(deqPtr) && datavalid(deqPtr) && addrvalid(deqPtr)) {
        uncacheState := s_req
      }
    }
    is(s_req) {
      when(io.uncache.req.fire()) {
        uncacheState := s_resp
      }
    }
    is(s_resp) {
      when(io.uncache.resp.fire()) {
        uncacheState := s_wb
      }
    }
    is(s_wb) {
      when (io.mmioStout.fire()) {
        uncacheState := s_wait
      }
    }
    is(s_wait) {
      when(io.roq.commit) {
        uncacheState := s_idle // ready for next mmio
      }
    }
  }
  io.uncache.req.valid := uncacheState === s_req

  io.uncache.req.bits.cmd  := MemoryOpConstants.M_XWR
  io.uncache.req.bits.addr := paddrModule.io.rdata(0) // data(deqPtr) -> rdata(0)
  io.uncache.req.bits.data := dataModule.io.rdata(0).data
  io.uncache.req.bits.mask := dataModule.io.rdata(0).mask

  io.uncache.req.bits.id   := DontCare

  when(io.uncache.req.fire()){
    // mmio store should not be committed until uncache req is sent
    pending(deqPtr) := false.B

    XSDebug(
      p"uncache req: pc ${Hexadecimal(uop(deqPtr).cf.pc)} " +
      p"addr ${Hexadecimal(io.uncache.req.bits.addr)} " +
      p"data ${Hexadecimal(io.uncache.req.bits.data)} " +
      p"op ${Hexadecimal(io.uncache.req.bits.cmd)} " +
      p"mask ${Hexadecimal(io.uncache.req.bits.mask)}\n"
    )
  }

  // (3) response from uncache channel: mark as datavalid
  io.uncache.resp.ready := true.B

  // (4) writeback to ROB (and other units): mark as writebacked
  io.mmioStout.valid := uncacheState === s_wb
  io.mmioStout.bits.uop := uop(deqPtr)
  io.mmioStout.bits.uop.sqIdx := deqPtrExt(0)
  io.mmioStout.bits.data := dataModule.io.rdata(0).data // dataModule.io.rdata.read(deqPtr)
  io.mmioStout.bits.redirectValid := false.B
  io.mmioStout.bits.redirect := DontCare
  io.mmioStout.bits.debug.isMMIO := true.B
  io.mmioStout.bits.debug.paddr := DontCare
  io.mmioStout.bits.debug.isPerfCnt := false.B
  io.mmioStout.bits.fflags := DontCare
  when (io.mmioStout.fire()) {
    allocated(deqPtr) := false.B
  }

  /**
    * ROB commits store instructions (mark them as commited)
    *
    * (1) When store commits, mark it as commited.
    * (2) They will not be cancelled and can be sent to lower level.
    */
  for (i <- 0 until CommitWidth) {
    when (commitCount > i.U) {
      commited(cmtPtrExt(i).value) := true.B
    }
  }
  cmtPtrExt := cmtPtrExt.map(_ + commitCount)

  // Commited stores will not be cancelled and can be sent to lower level.
  // remove retired insts from sq, add retired store to sbuffer
  for (i <- 0 until StorePipelineWidth) {
    // We use RegNext to prepare data for sbuffer
    val ptr = deqPtrExt(i).value
    // if !sbuffer.fire(), read the same ptr
    // if sbuffer.fire(), read next
    io.sbuffer(i).valid := allocated(ptr) && commited(ptr) && !mmio(ptr)
    // Note that store data/addr should both be valid after store's commit
    assert(!io.sbuffer(i).valid || allvalid(ptr))
    io.sbuffer(i).bits.cmd  := MemoryOpConstants.M_XWR
    io.sbuffer(i).bits.addr := paddrModule.io.rdata(i)
    io.sbuffer(i).bits.data := dataModule.io.rdata(i).data
    io.sbuffer(i).bits.mask := dataModule.io.rdata(i).mask
    io.sbuffer(i).bits.id   := DontCare

    when (io.sbuffer(i).fire()) {
      allocated(ptr) := false.B
      XSDebug("sbuffer "+i+" fire: ptr %d\n", ptr)
    }
  }
  when (io.sbuffer(1).fire()) {
    assert(io.sbuffer(0).fire())
  }
  if (useFakeDCache) {
    for (i <- 0 until StorePipelineWidth) {
      val ptr = deqPtrExt(i).value
      val fakeRAM = Module(new RAMHelper(64L * 1024 * 1024 * 1024))
      fakeRAM.io.clk   := clock
      fakeRAM.io.en    := allocated(ptr) && commited(ptr) && !mmio(ptr)
      fakeRAM.io.rIdx  := 0.U
      fakeRAM.io.wIdx  := (paddrModule.io.rdata(i) - "h80000000".U) >> 3
      fakeRAM.io.wdata := dataModule.io.rdata(i).data
      fakeRAM.io.wmask := MaskExpand(dataModule.io.rdata(i).mask)
      fakeRAM.io.wen   := allocated(ptr) && commited(ptr) && !mmio(ptr)
    }
  }

  if (!env.FPGAPlatform) {
    for (i <- 0 until StorePipelineWidth) {
      val storeCommit = io.sbuffer(i).fire()
      val waddr = SignExt(io.sbuffer(i).bits.addr, 64)
      val wdata = io.sbuffer(i).bits.data & MaskExpand(io.sbuffer(i).bits.mask)
      val wmask = io.sbuffer(i).bits.mask

      val difftest = Module(new DifftestStoreEvent)
      difftest.io.clock       := clock
      difftest.io.coreid      := hardId.U
      difftest.io.index       := i.U
      difftest.io.valid       := storeCommit
      difftest.io.storeAddr   := waddr
      difftest.io.storeData   := wdata
      difftest.io.storeMask   := wmask
    }
  }

  // Read vaddr for mem exception
  io.exceptionAddr.vaddr := vaddrModule.io.rdata(0)

  // misprediction recovery / exception redirect
  // invalidate sq term using robIdx
  val needCancel = Wire(Vec(StoreQueueSize, Bool()))
  for (i <- 0 until StoreQueueSize) {
    needCancel(i) := uop(i).roqIdx.needFlush(io.brqRedirect, io.flush) && allocated(i) && !commited(i)
    when (needCancel(i)) {
        allocated(i) := false.B
    }
  }

  /**
    * update pointers
    */
  val lastCycleRedirect = RegNext(io.brqRedirect.valid)
  val lastCycleFlush = RegNext(io.flush)
  val lastCycleCancelCount = PopCount(RegNext(needCancel))
  // when io.brqRedirect.valid, we don't allow eneuque even though it may fire.
  val enqNumber = Mux(io.enq.canAccept && io.enq.lqCanAccept && !(io.brqRedirect.valid || io.flush), PopCount(io.enq.req.map(_.valid)), 0.U)
  when (lastCycleRedirect || lastCycleFlush) {
    // we recover the pointers in the next cycle after redirect
    enqPtrExt := VecInit(enqPtrExt.map(_ - lastCycleCancelCount))
  }.otherwise {
    enqPtrExt := VecInit(enqPtrExt.map(_ + enqNumber))
  }

  deqPtrExt := deqPtrExtNext

  val dequeueCount = Mux(io.sbuffer(1).fire(), 2.U, Mux(io.sbuffer(0).fire() || io.mmioStout.fire(), 1.U, 0.U))
  val validCount = distanceBetween(enqPtrExt(0), deqPtrExt(0))

  allowEnqueue := validCount + enqNumber <= (StoreQueueSize - RenameWidth).U

  // io.sqempty will be used by sbuffer
  // We delay it for 1 cycle for better timing
  // When sbuffer need to check if it is empty, the pipeline is blocked, which means delay io.sqempty
  // for 1 cycle will also promise that sq is empty in that cycle
  io.sqempty := RegNext(enqPtrExt(0).value === deqPtrExt(0).value && enqPtrExt(0).flag === deqPtrExt(0).flag)

  // perf counter
  QueuePerf(StoreQueueSize, validCount, !allowEnqueue)
  io.sqFull := !allowEnqueue
  XSPerfAccumulate("mmioCycle", uncacheState =/= s_idle) // lq is busy dealing with uncache req
  XSPerfAccumulate("mmioCnt", io.uncache.req.fire())
  XSPerfAccumulate("mmio_wb_success", io.mmioStout.fire())
  XSPerfAccumulate("mmio_wb_blocked", io.mmioStout.valid && !io.mmioStout.ready)
  XSPerfAccumulate("validEntryCnt", distanceBetween(enqPtrExt(0), deqPtrExt(0)))
  XSPerfAccumulate("cmtEntryCnt", distanceBetween(cmtPtrExt(0), deqPtrExt(0)))
  XSPerfAccumulate("nCmtEntryCnt", distanceBetween(enqPtrExt(0), cmtPtrExt(0)))

  // debug info
  XSDebug("enqPtrExt %d:%d deqPtrExt %d:%d\n", enqPtrExt(0).flag, enqPtr, deqPtrExt(0).flag, deqPtr)

  def PrintFlag(flag: Bool, name: String): Unit = {
    when(flag) {
      XSDebug(false, true.B, name)
    }.otherwise {
      XSDebug(false, true.B, " ")
    }
  }

  for (i <- 0 until StoreQueueSize) {
    if (i % 4 == 0) XSDebug("")
    XSDebug(false, true.B, "%x ", uop(i).cf.pc)
    PrintFlag(allocated(i), "a")
    PrintFlag(allocated(i) && addrvalid(i), "a")
    PrintFlag(allocated(i) && datavalid(i), "d")
    PrintFlag(allocated(i) && commited(i), "c")
    PrintFlag(allocated(i) && pending(i), "p")
    PrintFlag(allocated(i) && mmio(i), "m")
    XSDebug(false, true.B, " ")
    if (i % 4 == 3 || i == StoreQueueSize - 1) XSDebug(false, true.B, "\n")
  }

}
