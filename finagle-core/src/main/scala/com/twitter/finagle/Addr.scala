package com.twitter.finagle

import java.util.{ List => JList, Collection => JCollection }
import java.net.SocketAddress

import scala.annotation.varargs
import scala.collection.immutable
import scala.collection.JavaConverters._

/**
 * An address identifies the location of an object--it is a bound
 * name. An object may be replicated, and thus bound to multiple
 * physical locations; it may be delegated to an unbound name.
 * (Similar to a symbolic link in Unix.)
 */
sealed trait Addr

object Addr {
    case class Bound(addrs: immutable.Set[SocketAddress]) extends Addr

    case class Failed(cause: Throwable) extends Addr

    object Pending extends Addr {
        override def toString = "Pending"
    }

    object Neg extends Addr {
        override def toString = "Neg"
    }

    object Bound {
        @varargs
        def apply(addrs: SocketAddress*): Addr =
            Bound(Set(addrs: _*))

        def apply(addrs: JList[SocketAddress]): Addr = Addrs.newBoundAddr(addrs)
    }

    object Failed {
        def apply(why: String): Addr = Failed(new Exception(why))
    }
}

/**
 *  A Java adaptation of the [[com.twitter.finagle.Addr]] companion object
 */
object Addrs {
    @varargs
    def newBoundAddr(addrs: SocketAddress*): Addr = Addr.Bound(addrs: _*)

    def newBoundAddr(addrs: JCollection[SocketAddress]): Addr =
        Addr.Bound(addrs.asScala.toSet)

    def newFailedAddr(cause: Throwable): Addr = Addr.Failed(cause)

    def newFailedAddr(why: String): Addr = Addr.Failed(why)

    val pendingAddr: Addr = Addr.Pending

    val negAddr: Addr = Addr.Neg
}