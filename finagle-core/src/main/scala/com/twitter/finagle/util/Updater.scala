package com.twitter.finagle.util

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.ConcurrentLinkedQueue
import scala.collection.mutable.ArrayBuffer

private[finagle] trait Updater[T] extends (T => Unit) {
    private[this] val n = new AtomicInteger(0)
    private[this] val q = new ConcurrentLinkedQueue[T]

    protected def preprocess(elems: Seq[T]): Seq[T]

    protected def handle(elem: T): Unit

    def apply(t: T) {
        q.offer(t)
        if (n.getAndIncrement() > 0)
            return

        do {
            val elems = new ArrayBuffer[T](1 + n.get)
            while (n.get > 1) {
                n.decrementAndGet()
                elems += q.poll()
            }
            elems += q.poll()
            for (elem <- preprocess(elems.result))
                handle(elem)
        } while (n.decrementAndGet() > 0)
    }
}