package com.twitter.finagle.util

import scala.collection.mutable.ArrayBuffer

class AsyncLatch(initialCount: Int = 0) {
    require(initialCount >= 0)
    @volatile private[this] var count = initialCount
    private[this] var waiters = new ArrayBuffer[() => Unit]

    def await(f: => Unit): Unit = synchronized {
        if (count == 0)
            f
        else
            waiters += { () => f }
    }

    def incr(): Int = synchronized { count += 1; count }

    def decr(): Int = {
        val pendingTasks = synchronized {
            require(count > 0)
            count -= 1
            if (count == 0) {
                val pending = waiters
                waiters = new ArrayBuffer[() => Unit]
                Left(pending)
            } else {
                Right(count)
            }
        }

        pendingTasks match {
            case Left(tasks) =>
                tasks foreach { _() }; 0
            case Right(count) =>
                count
        }
    }

    def getCount: Int = count

}