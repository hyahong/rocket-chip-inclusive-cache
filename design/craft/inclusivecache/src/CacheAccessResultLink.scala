// See LICENSE for license details.

package sifive.blocks.inclusivecache

import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.Location

// the subsystem registers the L2 here when connecting client result ports
case object CacheAccessResultLocation
  extends Location[InclusiveCache]("cache_access_results")

case class CacheAccessResultMapping(local: IdRange, remote: IdRange) {
  require(local.size == remote.size && local.size > 0)
  val offset = remote.start - local.start
}

object CacheAccessResultMapping {
  def apply(client: TLClientNode, sources: IdRange, cache: TLAdapterNode,
            edge: TLEdgeIn): CacheAccessResultMapping = {
    require(client.edges.out.size == 1, "cache result client must have one TL port")
    val localEdge = client.edges.out.head
    val locals = localEdge.client.clients.filter(c => c.sourceId.contains(sources))
    require(locals.size == 1 && locals.head.supports.probe.max > 0,
      "cache results require a coherent client containing the observed sources")
    val local = locals.head
    val remotes = edge.client.clients.filter(c =>
      c.nodePath.exists(_ eq client) && c.supports.probe.max > 0 && c.sourceId.size == local.sourceId.size)
    require(remotes.size == 1, "cache result client lost or remapped on its path to L2")
    val remote = remotes.head
    remote.nodePath.foreach { n =>
      val supported = (n eq client) || (n eq cache) || (n match {
        case _: TLIdentityNode | _: TLEphemeralNode | _: TLBufferNode => true
        case _ => n.lazyModule match {
          case _: TLXbar | _: TLJbar | _: TLBuffer | _: TLWidthWidget |
               _: TLFIFOFixer | _: TLFilter => true
          case _ => false
        }
      })
      require(supported, s"unsupported cache result source/clock/reset path at ${n.name}")
    }
    val coherent = localEdge.manager.managers.filter(_.supportsAcquireB)
    require(coherent.nonEmpty && coherent.forall(_.nodePath.exists(_ eq cache)),
      "every coherent address of an observed client must pass through this L2")
    CacheAccessResultMapping(sources,
      sources.shift(remote.sourceId.start - local.sourceId.start))
  }
}
