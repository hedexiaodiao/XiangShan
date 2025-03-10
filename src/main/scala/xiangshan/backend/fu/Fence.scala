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

package xiangshan.backend.fu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._

class FenceToSbuffer extends Bundle {
  val flushSb = Output(Bool())
  val sbIsEmpty = Input(Bool())
}

class Fence(implicit p: Parameters) extends FunctionUnit with HasExceptionNO {

  val sfence = IO(Output(new SfenceBundle))
  val fencei = IO(Output(Bool()))
  val toSbuffer = IO(new FenceToSbuffer)
  val disableSfence = IO(Input(Bool()))

  val (valid, src1) = (
    io.in.valid,
    io.in.bits.src(0)
  )

  val s_idle :: s_wait :: s_tlb :: s_icache :: s_fence :: Nil = Enum(5)
  val state = RegInit(s_idle)
  /* fsm
   * s_idle    : init state, send sbflush
   * s_wait  : send sbflush, wait for sbEmpty
   * s_tlb   : flush tlb, just hold one cycle
   * s_icache: flush icache, just hold one cycle
   * s_fence : do nothing, for timing optimiaztion
   */

  val sbuffer = toSbuffer.flushSb
  val sbEmpty = toSbuffer.sbIsEmpty
  val uop = RegEnable(io.in.bits.uop, io.in.fire())
  val func = uop.ctrl.fuOpType

  // NOTE: icache & tlb & sbuffer must receive flush signal at any time
  sbuffer      := state === s_wait && !(func === FenceOpType.sfence && disableSfence)
  fencei       := state === s_icache
  sfence.valid := state === s_tlb && !disableSfence
  sfence.bits.rs1  := uop.ctrl.lsrc(0) === 0.U
  sfence.bits.rs2  := uop.ctrl.lsrc(1) === 0.U
  sfence.bits.addr := RegEnable(src1, io.in.fire())

  when (state === s_idle && valid) { state := s_wait }
  when (state === s_wait && func === FenceOpType.fencei && sbEmpty) { state := s_icache }
  when (state === s_wait && func === FenceOpType.sfence && (sbEmpty || disableSfence)) { state := s_tlb }
  when (state === s_wait && func === FenceOpType.fence  && sbEmpty) { state := s_fence }
  when (state =/= s_idle && state =/= s_wait) { state := s_idle }

  io.in.ready := state === s_idle
  io.out.valid := state =/= s_idle && state =/= s_wait
  io.out.bits.data := DontCare
  io.out.bits.uop := uop
  io.out.bits.uop.cf.exceptionVec(illegalInstr) := uop.cf.exceptionVec(illegalInstr) || (func === FenceOpType.sfence && disableSfence)

  XSDebug(valid, p"In(${io.in.valid} ${io.in.ready}) state:${state} Inpc:0x${Hexadecimal(io.in.bits.uop.cf.pc)} InroqIdx:${io.in.bits.uop.roqIdx}\n")
  XSDebug(state =/= s_idle, p"state:${state} sbuffer(flush:${sbuffer} empty:${sbEmpty}) fencei:${fencei} sfence:${sfence}\n")
  XSDebug(io.out.valid, p" Out(${io.out.valid} ${io.out.ready}) state:${state} Outpc:0x${Hexadecimal(io.out.bits.uop.cf.pc)} OutroqIdx:${io.out.bits.uop.roqIdx}\n")

  assert(!(io.out.valid && io.out.bits.uop.ctrl.rfWen))
  assert(!io.out.valid || io.out.ready, "when fence is out valid, out ready should always be true")
}
