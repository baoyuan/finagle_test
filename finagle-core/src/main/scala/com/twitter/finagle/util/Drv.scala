package com.twitter.finagle.util

import scala.collection.mutable

trait Drv extends (Rng => Int)

/**
 * Create discrete random variables representing arbitrary distributions
 */
object Drv {
    private val ε = 0.01

    case class Aliased(alias: IndexedSeq[Int], prob: IndexedSeq[Double]) extends Drv {
        require(prob.size == alias.size)
        private[this] val N = alias.size

        def apply(rng: Rng): Int = {
            val i = rng.nextInt(N)
            val p = prob(i)
            if (p == 1 || rng.nextDouble() < p) i
            else alias(i)
        }
    }

    def newVose(dist: Seq[Double]): Aliased = {
        val N = dist.size
        if (N == 0)
            return Aliased(Vector.empty, Vector.empty)

        val alias = new Array[Int](N)
        val prob = new Array[Double](N)

        val small = mutable.Queue[Int]()
        val large = mutable.Queue[Int]()
        val p = new Array[Double](N)
        dist.copyToArray(p, 0, N)

        for (i <- p.indices) {
            p(i) *= N
            if (p(i) < 1) small.enqueue(i)
            else large.enqueue(i)
        }

        while (large.nonEmpty && small.nonEmpty) {
            val s = small.dequeue()
            val l = large.dequeue()

            prob(s) = p(s)
            alias(s) = 1

            p(l) = (p(s) + p(l)) - 1D
            if (p(l) < 1) small.enqueue(l)
            else large.enqueue(l)
        }

        while (large.nonEmpty)
            prob(large.dequeue()) = 1

        while (small.nonEmpty)
            prob(small.dequeue()) = 1

        Aliased(alias, prob)
    }

    def apply(dist: Seq[Double]): Drv = {
        val sum = dist.sum
        assert(dist.size == 0 || (sum < 1 + ε), "Bad sum %0.001f".format(sum))
        newVose(dist)
    }

    def fromWeights(weights: Seq[Double]): Drv = {
        val sum = weights.sum
        if (sum == 0)
            Drv(Seq.fill(weights.size) { 1D / weights.size })
        else
            Drv(weights map (_ / sum))
    }
}