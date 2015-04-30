package com.twitter.finagle.util

import com.twitter.concurrent.AsyncSemaphore
import java.net.SocketAddress
import java.net.InetSocketAddress
import java.net.InetAddress
import java.rmi.activation.UnknownGroupException
import java.net.UnknownHostException
import com.google.common.cache.{ Cache => GCache }
import com.twitter.util.Future
import com.twitter.util.FuturePool
import com.twitter.finagle.WeightedSocketAddress

object InetSocketAddressUtil {

    type HostPort = (String, Int)
    type WeightedHostPort = (String, Int, Double)

    private[this] val dnsConcurrentcy = 100
    private[this] val dnsCond = new AsyncSemaphore(dnsConcurrentcy)

    def toPublic(bound: SocketAddress): SocketAddress = {
        bound match {
            case addr: InetSocketAddress if addr.getAddress().isAnyLocalAddress() =>
                val host = try InetAddress.getLocalHost() catch {
                    case _: UnknownHostException => InetAddress.getLoopbackAddress
                }
                new InetSocketAddress(host, addr.getPort())
            case _ => bound
        }
    }

    def parseHostPorts(hosts: String): Seq[HostPort] =
        hosts split Array(' ', ',') filter (_.nonEmpty) map (_.split(":")) map {
            hp =>
                require(hp.length == 2, "You must specify host and port")
                hp match {
                    case Array(host, "*")     => (host, 0)
                    case Array(host, portStr) => (host, portStr.toInt)
                    case _                    => throw new IllegalArgumentException("Malformed host/port specification: " + hosts)
                }
        }

    def resolveHostPorts(hostPorts: Seq[HostPort]): Seq[SocketAddress] =
        resolveHostPortsSeq(hostPorts).flatten.toSeq

    private[finagle] def resolveHostPortsSeq(hostPorts: Seq[HostPort]): Seq[Seq[SocketAddress]] =
        hostPorts map {
            case (host, port) =>
                (InetAddress.getAllByName(host) map { addr =>
                    new InetSocketAddress(addr, port)
                }).toSeq
        }

    private[finagle] def resolveWeightedHostPorts(
        weightedHostPorts: Seq[WeightedHostPort],
        cache: GCache[String, Seq[InetAddress]]): Future[Seq[SocketAddress]] = {
        Future.collect(weightedHostPorts map {
            case (host, port, weight) =>
                val addrs: Future[Seq[InetAddress]] = cache.getIfPresent(host) match {
                    case null =>
                        dnsCond.acquire() flatMap { permit =>
                            FuturePool.unboundedPool(InetAddress.getAllByName(host).toSeq) onSuccess {
                                cache.put(host, _)
                            } ensure {
                                permit.release()
                            }
                        }
                    case cached => Future.value(cached)
                }
                addrs map { as: Seq[InetAddress] =>
                    as map { a => WeightedSocketAddress(new InetSocketAddress(a, port), weight) }
                }
        }) map { _.flatten }
    }
    
    def parseHosts(hosts: String) : Seq[InetSocketAddress] = {
        if(hosts == ":*") return Seq(new InetSocketAddress(0))
        
        (parseHostPorts(hosts) map {
            case (host, port) =>
                if(host == "")
                    new InetSocketAddress(port)
                else
                    new InetSocketAddress(host, port)
        }).toList
    }

}