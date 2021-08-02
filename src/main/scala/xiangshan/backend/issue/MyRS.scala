package xiangshan.backend.issue

import xiangshan.mem.{SqPtr, StoreDataBundle}
import xiangshan.{ExuInput, ExuOutput, HasXSParameter, MicroOp, RSFeedback, Redirect}

class ReservationStation(implicit p: Parameters) extends LazyModule with HasXSParameter {
  val params = new RSParams
  lazy val module = new LazyModuleImp(this){
    val io = IO(new Bundle {
      val redirect = Flipped(ValidIO(new Redirect))
      //Useage of ValidIO?
      val flush = Input(Bool())
      val numExist = Output(UInt(log2Up(params.numEntries + 1).W))
      //enq
      val fromDispatch = Vec(params.numEnq, Flipped(DecoupledIO(new MicroOp)))
      val srcRegValue = Vec(params.numEnq, Input(Vec(params.numSrc, UInt(params.dataBits.W))))
      val fpRegValue = if (params.delayedRf) Some(Input(UInt(params.dataBits.W))) else None
      // deq
      val deq = Vec(params.numDeq, DecoupledIO(new ExuInput))

      //      val fastUopOut = Vec(params.numDeq, ValidIO(new MicroOp))
      val fastUopsIn = Vec(params.numFastWakeup, Flipped(ValidIO(new MicroOp)))
      val fastDatas = Vec(params.numFastWakeup, Input(UInt(params.dataBits.W)))
      val slowPorts = Vec(params.numWakeup, Flipped(ValidIO(new ExuOutput)))
    })
    val io_fastWakeup = if (params.fixedLatency >= 0) Some(IO(Vec(params.numDeq, ValidIO(new MicroOp)))) else None
    val io_jump = if (params.isJump) Some(IO(new Bundle {
      val jumpPc = Input(UInt(VAddrBits.W))
      val jalr_target = Input(UInt(VAddrBits.W))
    })) else None
    val io_feedback = if (params.hasFeedback) Some(IO(new Bundle {
      val memfeedback = Flipped(ValidIO(new RSFeedback))
      val rsIdx = Output(UInt(log2Up(params.numEntries).W))
      val isFirstIssue = Output(Bool()) // NOTE: just use for tlb perf cnt
    })) else None
    val io_checkwait = if (params.checkWaitBit) Some(IO(new Bundle {
      val stIssuePtr = Input(new SqPtr())
    })) else None
    val io_store = if (params.isStore) Some(IO(new Bundle {
      val stData = ValidIO(new StoreDataBundle)
    })) else None

    val statusArray = Module(new StatusArray(params))
    val select = Module(new SelectPolicy(params))
    val dataArray = Module(new DataArray(params))
    val payloadArray = Module(new PayloadArray(new MicroOp, params))

    io.numExist := PopCount(statusArray.io.isValid)
    statusArray.io.redirect := io.redirect
    statusArray.io.flush := io.flush

    //enqueue from dispatch
    select.io.validVec := statusArray.io.isValid
    val doEnqueue = Wire(Vec(params.numEnq, Bool()))
    val needFpSource = Wire(Vec(params.numEnq, Bool()))
    for (i<-0 until params.numEnq) {
      io.fromDispatch(i).ready := select.io.allocate(i).valid
      //agreement with dispatch: don't enqueue when io.redirect.valid
      doEnqueue(i) := io.fromDispatch(i).fire() && !io.redirect.valid && !io.flush
      select.io.allocate(i).ready := doEnqueue(i)
      statusArray.io.update(i).enable := doEnqueue(i)
      statusArray.io.update(i).addr := select.io.allocate(i).bits
      statusArray.io.update(i).data.valid := true.B
      needFpSource(i) := io.fromDispatch(i).bits.needRfRPort(1, 1, false)
      statusArray.io.update(i).data.scheduled := (if (params.delayedRf) needFpSource(i) else false.B)
      statusArray.io.update(i).data.blocked := (if (params.checkWaitBit) io.fromDispatch(i).bits.cf.loadWaitBit else false.B)
      statusArray.io.update(i).data.credit := (if (params.delayedRf) Mux(needFpSource(i), 2.U, 0.U) else 0.U)
      statusArray.io.update(i).data.srcState := VecInit(io.fromDispatch(i).bits.srcIsReady.take(params.numSrc))
      statusArray.io.update(i).data.psrc := VecInit(io.fromDispatch(i).bits.psrc.take(params.numSrc))
      statusArray.io.update(i).data.srcType := VecInit(io.fromDispatch(i).bits.ctrl.srcType.take(params.numSrc))
      statusArray.io.update(i).data.roqIdx := io.fromDispatch(i).bits.roqIdx
      statusArray.io.update(i).data.sqIdx := io.fromDispatch(i).bits.sqIdx
      statusArray.io.update(i).data.isFirstIssue := true.B
      payloadArray.io.write(i).enable := doEnqueue(i)
      payloadArray.io.write(i).addr := select.io.allocate(i).bits
      payloadArray.io.write(i).data := io.fromDispatch(i).bits
    }

  })
  }
}
