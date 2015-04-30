package com.twitter.finagle.util

import com.twitter.util.{ NetUtil => UtilNetUtil }
import java.net.InetAddress
import java.net.Inet4Address

object InetAddressUtil {
    val Loopback = InetAddress.getLoopbackAddress

    def isPrivateAddress(ip: InetAddress): Boolean =
        ip match {
            case ip: Inet4Address =>
                val addr = ip.getAddress
                if (addr(0) == 10.toByte)
                    true
                else if (addr(0) == 172.toByte && (addr(1) & 0xf0) == 16.toByte)
                    true
                else if (addr(0) == 192.toByte && addr(1) == 168.toByte)
                    true
                else
                    false
            case _ =>
                false
        }

    def getByName(host: String): InetAddress = {
        UtilNetUtil.ipToOptionInt(host) match {
            case Some(i) =>
                val bytes = Array[Byte](
                    ((i & 0xff000000) >> 24).toByte,
                    ((i & 0x00ff0000) >> 16).toByte,
                    ((i & 0x0000ff00) >> 8).toByte,
                    ((i & 0x000000ff)).toByte)
                InetAddress.getByAddress(host, bytes)
            case None =>
                InetAddress.getByName(host)
        }
    }
}