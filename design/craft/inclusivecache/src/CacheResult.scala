// See LICENSE for license details.

package sifive.blocks.inclusivecache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.CacheAccessResult

class CacheBlockAddress(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val set = UInt(params.setBits.W)
  val tag = UInt(params.tagBits.W)
}

class CacheAccessRequest(params: InclusiveCacheParameters) extends CacheBlockAddress(params)
{
  val source = UInt(params.inner.bundle.sourceBits.W)
}

class CacheAccessLookup(params: InclusiveCacheParameters) extends CacheAccessRequest(params)
{
  val miss = Bool()
}

class CacheResultTracker(params: InclusiveCacheParameters, sources: Seq[Int]) extends Module
{
  require(sources.nonEmpty && sources.distinct.size == sources.size)
  require(sources.forall(s => s >= 0 && s < params.inner.client.endSourceId))

  val io = IO(new Bundle {
    val request = Flipped(Valid(new CacheAccessRequest(params)))
    val ready = Output(Bool())
    val lookup = Flipped(Vec(params.mshrs, Valid(new CacheAccessLookup(params))))
    val pending_miss = Flipped(Vec(params.mshrs, Valid(new CacheBlockAddress(params))))
    val complete = Flipped(Valid(UInt(params.inner.bundle.sourceBits.W)))
    val result = Decoupled(new CacheAccessResult(params.inner.bundle.sourceBits))
  })

  val active = RegInit(VecInit(Seq.fill(sources.size)(false.B)))
  val known = RegInit(VecInit(Seq.fill(sources.size)(false.B)))
  val miss = Reg(Vec(sources.size, Bool()))
  val queued = RegInit(VecInit(Seq.fill(sources.size)(false.B)))
  val sent = RegInit(VecInit(Seq.fill(sources.size)(false.B)))
  val completed = RegInit(VecInit(Seq.fill(sources.size)(false.B)))
  val blocks = Reg(Vec(sources.size, new CacheBlockAddress(params)))

  val selected = sources.map(s => io.request.bits.source === s.U)
  io.ready := !(VecInit(selected.zip(active).map { case (s, a) => s && a }).asUInt.orR)

  val arbiter = Module(new RRArbiter(new CacheAccessResult(params.inner.bundle.sourceBits), sources.size))
  val output = Module(new Queue(new CacheAccessResult(params.inner.bundle.sourceBits), 2))
  output.io.enq <> arbiter.io.out
  io.result <> output.io.deq

  for ((source, i) <- sources.zipWithIndex) {
    val accept = io.request.valid && selected(i)
    val live = active(i) || accept
    val block_set = Mux(accept, io.request.bits.set, blocks(i).set)
    val block_tag = Mux(accept, io.request.bits.tag, blocks(i).tag)
    def sameBlock(b: CacheBlockAddress): Bool = b.set === block_set && b.tag === block_tag

    val lookups = VecInit(io.lookup.map(l => l.valid && sameBlock(l.bits)))
    val waiting = io.pending_miss.map(m => m.valid && sameBlock(m.bits)).reduce(_ || _)
    val lookup_miss = io.lookup.zip(lookups).map { case (l, hit) => hit && l.bits.miss }.reduce(_ || _)
    val own_lookup = io.lookup.zip(lookups).map { case (l, hit) =>
      hit && l.bits.source === source.U
    }.reduce(_ || _)
    val classify = live && (!known(i) || accept) && (waiting || lookup_miss || own_lookup)
    val delivered = io.result.fire && io.result.bits.source === source.U
    val finished = active(i) && io.complete.valid && io.complete.bits === source.U

    arbiter.io.in(i).valid := active(i) && known(i) && !queued(i)
    arbiter.io.in(i).bits.source := source.U
    arbiter.io.in(i).bits.miss := miss(i)

    when (accept) {
      assert(io.ready, "cache result source reused before result and response completed")

      active(i) := true.B
      known(i) := false.B
      queued(i) := false.B
      sent(i) := false.B
      completed(i) := false.B
      blocks(i).set := io.request.bits.set
      blocks(i).tag := io.request.bits.tag
    }
    when (classify) {
      known(i) := true.B
      miss(i) := waiting || lookup_miss

      assert(PopCount(lookups) <= 1.U, "multiple L2 lookups classified the same line")
    }
    when (arbiter.io.in(i).fire) { queued(i) := true.B }
    when (delivered) {
      assert(active(i) && known(i) && queued(i), "unexpected cache result delivery")
      sent(i) := true.B
    }
    when (finished) {
      completed(i) := true.B
    }
    when (active(i) && (sent(i) || delivered) && (completed(i) || finished)) {
      active(i) := false.B
    }
  }
}
