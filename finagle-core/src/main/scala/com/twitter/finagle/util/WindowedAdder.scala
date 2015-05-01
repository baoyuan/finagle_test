package com.twitter.finagle.util

import scala.actors.threadpool.AtomicInteger
import scala.actors.threadpool.Arrays
import com.twitter.util.Time
import com.twitter.jsr166e.LongAdder

private[this] class WindowedAdder(range: Long, slices: Int, now: () => Long) {
    require(slices > 1)
    private[this] val window = range / slices
    private[this] val N = slices - 1

    private[this] val writer = new LongAdder
    @volatile private[this] var gen = 0
    private[this] val expiredGen = new AtomicInteger(gen)

    private[this] val buf = new Array[Long](N)
    @volatile private[this] var i = 0
    @volatile private[this] var old = now()

    private[this] def expired() {
        if (!expiredGen.compareAndSet(gen, gen + 1))
            return

        buf(i) = writer.sumThenReset()
        i = (i + 1) % N
        val nskip = math.min(((now() - old) / window - 1).toInt, N)
        if (nskip > 0) {
            val r = math.min(nskip, (N - i))
            Arrays.fill(buf, i, i + r, 0L)
            Arrays.fill(buf, 0, nskip - r, 0L)
            i = (i + nskip) % N
        }
        old = now()
        gen += 1
    }

    def reset() {
        Arrays.fill(buf, 0, N, 0L)
        writer.reset()
        old = now()
    }

    def incr() = add(1)

    def add(x: Int) {
        if ((now() - old) >= window)
            expired()
        writer.add(x)
    }

    def sum(): Long = {
        if ((now() - old) >= window)
            expired()
        val _ = gen
        var sum = writer.sum()
        var i = 0;
        while (i < N) {
            sum += buf(i)
            i += 1
        }
        sum
    }
}
private[finagle] object WindowedAdder {
    val systemMs: () => Long = () => System.nanoTime() / (1000 * 1000)
    val timeMs: () => Long = () => Time.now.inMilliseconds
}