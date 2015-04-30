package com.twitter.finagle.util

import scala.reflect.ClassTag
import java.util.concurrent.atomic.AtomicLong
import scala.annotation.tailrec

class ConcurrentRingBuffer[T: ClassTag](capacity: Int) {
    assert(capacity > 0)

    private[this] val nextRead, nextWrite = new AtomicLong(0)
    private[this] val publishedWrite = new AtomicLong(-1)
    private[this] val ring = new Array[T](capacity)

    private[this] def publish(which: Long) {
        while (publishedWrite.get != which - 1) {}
        val ok = publishedWrite.compareAndSet(which - 1, which)
        assert(ok)
    }

    @tailrec
    final def tryGet(): Option[T] = {
        val w = publishedWrite.get
        val r = nextRead.get

        if (w < r)
            return None

        val el = ring((r % capacity).toInt)
        if (nextRead.compareAndSet(r, r + 1))
            Some(el)
        else
            tryGet()
    }

    final def tryPeek: Option[T] = {
        val w = publishedWrite.get
        val r = nextRead.get

        if (w < r)
            None
        else
            Some(ring((r % capacity).toInt))
    }

    @tailrec
    final def tryPut(el: T): Boolean = {
        val w = nextWrite.get
        val r = nextRead.get

        if (w - r >= capacity)
            return false

        if (!nextWrite.compareAndSet(w, w + 1)) tryPut(el) else {
            ring((w % capacity).toInt) = el
            publish(w)
            true
        }
    }

    final def size: Int = (nextWrite.get - nextRead.get).toInt
}