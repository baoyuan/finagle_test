package com.twitter.finagle.util

import com.twitter.util.Duration

private[finagle] trait TokenBucket {
    def put(n: Int): Unit
    def tryGet(n: Int): Boolean
    def count: Long
}

private[finagle] object TokenBucket {
    def newLeakyBucket(ttl: Duration, reserve: Int): TokenBucket = new TokenBucket {
        private[this] val w = new WindowedAdder(ttl.inMicroseconds, 10, WindowedAdder.timeMs)

        def put(n: Int): Unit = w.add(n)

        def tryGet(n: Int): Boolean = synchronized {
            val ok = count >= n
            if (ok)
                w.add(-n)
            ok
        }

        def count: Long = w.sum() + reserve
    }
}