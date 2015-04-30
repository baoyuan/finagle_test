package com.twitter.finagle.util

import java.util.concurrent.ThreadLocalRandom

/**
 * A random number generator
 */
trait Rng {
    def nextDouble(): Double
    def nextInt(n: Int): Int
    def nextInt(): Int
    def nextLong(n: Long): Long
}

object Rng {
    def apply(): Rng = Rng(new java.util.Random)
    def apply(seed: Long): Rng = Rng(new java.util.Random(seed))
    def apply(r: scala.util.Random): Rng = Rng(r.self)
    def apply(r: java.util.Random): Rng = new Rng {
        def nextDouble(): Double = r.nextDouble()
        def nextInt(n: Int): Int = r.nextInt(n)
        def nextIn(): Int = r.nextInt()
        def nextLong(n: Long): Long = {
            require(n > 0)

            if ((n & -n) == n)
                return r.nextLong() % n

            var bits = 0L
            var v = 0L
            do {
                bits = (r.nextLong() << 1) >>> 1
                v = bits % n
            } while (bits - v + (n - 1) < 0L)
            v
        }
    }
    val threadLocal: Rng = new Rng {
        def nextDouble(): Double = ThreadLocalRandom.current().nextDouble()
        def nextInt(n: Int): Int = ThreadLocalRandom.current().nextInt(0, n)
        def nextInt(): Int = ThreadLocalRandom.current().nextInt()
        def nextLong(): Long = ThreadLocalRandom.current().nextLong()
    }
}