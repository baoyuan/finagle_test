package com.twitter.finagle.util

private[finagle] class Ema(window: Long) {
    private[this] var time = Long.MinValue
    private[this] var ema = 0D

    def isEmpty: Boolean = synchronized { time < 0 }

    def update(stamp: Long, x: Long): Double = synchronized {
        if (time == Long.MinValue) {
            time = stamp
            ema = x
        } else {
            val td = stamp - time
            assert(td >= 0, "Nonmonotonic timestamp")
            time = stamp
            val w = if (window == 0) 0 else math.exp(-td.toDouble / window)
            ema = x * (1 - w) + ema * w
        }
        ema
    }

    def last: Double = synchronized { ema }
}

private[finagle] object Ema {
    class Monotime {
        private[this] var last = System.nanoTime()

        def nanos(): Long = synchronized {
            val sample = System.nanoTime()
            if (sample - last > 0)
                last = sample
            last
        }
    }
}