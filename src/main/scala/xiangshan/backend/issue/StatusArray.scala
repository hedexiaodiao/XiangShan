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

package xiangshan.backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.roq.RoqPtr
import xiangshan.mem.SqPtr

class StatusArrayUpdateIO(params: RSParams)(implicit p: Parameters) extends Bundle {
  val enable = Input(Bool())
  // should be one-hot
  val addr = Input(UInt(params.numEntries.W))
  val data = Input(new StatusEntry(params))

  def isLegal() = {
    PopCount(addr.asBools) === 0.U
  }

  override def cloneType: StatusArrayUpdateIO.this.type =
    new StatusArrayUpdateIO(params).asInstanceOf[this.type]
}

class StatusEntry(params: RSParams)(implicit p: Parameters) extends XSBundle {
  // states
  val valid = Bool()
  val scheduled = Bool()
  val blocked = Bool()
  val credit = UInt(4.W)
  val srcState = Vec(params.numSrc, Bool())
  // data
  val psrc = Vec(params.numSrc, UInt(params.dataIdBits.W))
  val srcType = Vec(params.numSrc, SrcType())
  val roqIdx = new RoqPtr
  val sqIdx = new SqPtr
  // misc
  val isFirstIssue = Bool()

  override def cloneType: StatusEntry.this.type =
    new StatusEntry(params).asInstanceOf[this.type]
  override def toPrintable: Printable = {
    p"$valid, $scheduled, ${Binary(srcState.asUInt)}, $psrc, $roqIdx"
  }
}

class StatusArray(params: RSParams)(implicit p: Parameters) extends XSModule
  with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val redirect = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())
    // current status
    val isValid = Output(UInt(params.numEntries.W))
    val canIssue = Output(UInt(params.numEntries.W))
    // enqueue, dequeue, wakeup, flush
    val update = Vec(params.numEnq, new StatusArrayUpdateIO(params))
    val wakeup = Vec(params.allWakeup, Flipped(ValidIO(new MicroOp)))
    val wakeupMatch = Vec(params.numEntries, Vec(params.numSrc, Output(UInt(params.allWakeup.W))))
    val issueGranted = Vec(params.numDeq, Flipped(ValidIO(UInt(params.numEntries.W))))
    // TODO: if more info is needed, put them in a bundle
    val isFirstIssue = Vec(params.numDeq, Output(Bool()))
    val deqResp = Vec(params.numDeq, Flipped(ValidIO(new Bundle {
      val rsMask = UInt(params.numEntries.W)
      val success = Bool()
    })))
    val stIssuePtr = if (params.checkWaitBit) Input(new SqPtr()) else null
  })

  val statusArray = Reg(Vec(params.numEntries, new StatusEntry(params)))
  val statusArrayNext = WireInit(statusArray)
  statusArray := statusArrayNext
  when (reset.asBool) {
    statusArray.map(_.valid := false.B)
  }

  // instruction is ready for issue
  val readyVec = VecInit(statusArray.map(s => s.srcState.asUInt.andR && !s.scheduled && !s.blocked))
  val readyVecNext = VecInit(statusArrayNext.map(s => s.srcState.asUInt.andR && !s.scheduled && !s.blocked))

  // update srcState when enqueue, wakeup
  def wakeupMatch(psrc: UInt, srcType: UInt) = {
    val matchVec = VecInit(io.wakeup.map(w =>
      w.valid && w.bits.pdest === psrc && (SrcType.isReg(srcType) && w.bits.ctrl.rfWen && psrc =/= 0.U || SrcType.isFp(srcType) && w.bits.ctrl.fpWen)
    ))
    XSError(PopCount(matchVec) > 1.U, p"matchVec ${Binary(matchVec.asUInt)} should be one-hot\n")
    matchVec.asUInt
  }
  def deqRespSel(i: Int) : (Bool, Bool) = {
    val mask = VecInit(io.deqResp.map(resp => resp.valid && resp.bits.rsMask(i)))
    XSError(PopCount(mask) > 1.U, p"feedbackVec ${Binary(mask.asUInt)} should be one-hot\n")
    val successVec = io.deqResp.map(_.bits.success)
    (mask.asUInt.orR, Mux1H(mask, successVec))
  }
  for (((status, statusNext), i) <- statusArray.zip(statusArrayNext).zipWithIndex) {
    val selVec = VecInit(io.update.map(u => u.enable && u.addr(i)))
    XSError(PopCount(selVec) > 1.U, "should not update the same entry\n")
    val updateEn = selVec.asUInt.orR

    when (updateEn) {
      val updateStatus = Mux1H(selVec, io.update.map(_.data))
      val wakeupEnVec = VecInit(updateStatus.psrc.zip(updateStatus.srcType).map{ case (p, t) => wakeupMatch(p, t) })
      val wakeupEn = wakeupEnVec.map(_.orR)
      io.wakeupMatch(i) := wakeupEnVec
      statusNext.valid := true.B
      statusNext.srcState := VecInit(updateStatus.srcState.zip(wakeupEn).map {
        case (update, wakeup) => update || wakeup
      })
      statusNext.scheduled := updateStatus.scheduled
      statusNext.blocked := updateStatus.blocked
      statusNext.credit := updateStatus.credit
      statusNext.psrc := updateStatus.psrc
      statusNext.srcType := updateStatus.srcType
      statusNext.roqIdx := updateStatus.roqIdx
      statusNext.sqIdx := updateStatus.sqIdx
      statusNext.isFirstIssue := true.B
      XSError(status.valid, p"should not update a valid entry\n")
    }.otherwise {
      val hasIssued = VecInit(io.issueGranted.map(iss => iss.valid && iss.bits(i))).asUInt.orR
      val (deqResp, deqGrant) = deqRespSel(i)
      XSError(deqResp && !status.valid, "should not deq an invalid entry\n")
      if (params.hasFeedback) {
        XSError(deqResp && !status.scheduled, "should not deq an un-scheduled entry\n")
      }
      val wakeupEnVec = VecInit(status.psrc.zip(status.srcType).map{ case (p, t) => wakeupMatch(p, t) })
      val wakeupEn = wakeupEnVec.map(_.orR)
      io.wakeupMatch(i) := wakeupEnVec
      statusNext.valid := Mux(deqResp && deqGrant, false.B, status.valid && !status.roqIdx.needFlush(io.redirect, io.flush))
      // (1) when deq is not granted, unset its scheduled bit; (2) set scheduled if issued
      statusNext.scheduled := Mux(deqResp && !deqGrant || status.credit === 1.U, false.B, status.scheduled || hasIssued)
      XSError(hasIssued && !status.valid, "should not issue an invalid entry\n")
      if (params.checkWaitBit) {
        statusNext.blocked := status.blocked && isAfter(status.sqIdx, io.stIssuePtr)
      }
      else {
        statusNext.blocked := false.B
      }
      statusNext.credit := Mux(status.credit > 0.U, status.credit - 1.U, status.credit)
      XSError(status.valid && status.credit > 0.U && !status.scheduled,
        p"instructions $i with credit ${status.credit} must not be scheduled\n")
      statusNext.srcState := VecInit(status.srcState.zip(wakeupEn).map {
        case (current, wakeup) => current || wakeup
      })
      // when the entry is not granted to leave the RS, set isFirstIssue to false.B
      when (deqResp && !deqGrant) {
        statusNext.isFirstIssue := false.B
      }
    }

    XSDebug(status.valid, p"entry[$i]: $status\n")
  }

  io.isValid := VecInit(statusArray.map(_.valid)).asUInt
  io.canIssue := VecInit(statusArray.map(_.valid).zip(readyVec).map{ case (v, r) => v && r}).asUInt
  io.isFirstIssue := VecInit(io.issueGranted.map(iss => Mux1H(iss.bits, statusArray.map(_.isFirstIssue))))

}
