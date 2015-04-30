package com.twitter.finagle

import java.net.SocketAddress
import java.net.InetSocketAddress

/**
 * A SocketAddress with a weight
 */
object WeightedSocketAddress {
    private case class Impl(
        addr: SocketAddress,
        weight: Double) extends SocketAddress

    def apply(addr: SocketAddress, weight: Double): SocketAddress =
        Impl(addr, weight)

    def unapply(addr: SocketAddress): Option[(SocketAddress, Double)] =
        addr match {
            case Impl(addr, weight) => Some(addr, weight)
            case addr               => Some(addr, 1D)
        }
}

object WeightedInetSocketAddress {
    def unapply(addr: SocketAddress): Option[(InetSocketAddress, Double)] =
        addr match {
            case WeightedSocketAddress(ia: InetSocketAddress, weight) => Some(ia, weight)
            case ia: InetSocketAddress => Some(ia, 1D)
            case _ => None
        }
}