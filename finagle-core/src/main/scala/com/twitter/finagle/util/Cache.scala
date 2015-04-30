package com.twitter.finagle.util

import com.twitter.util.Duration
import com.twitter.util.TimerTask
import com.twitter.util.Time
import java.util.ArrayDeque
import scala.annotation.tailrec
import scala.collection.JavaConverters._

private[finagle] class Cache[A](
    cacheSize: Int,
    ttl: Duration,
    timer: com.twitter.util.Timer,
    evictor: Option[A => Unit] = None) {
    
    require(cacheSize > 0)

    private[this] var deque = new ArrayDeque[(Time, A)]
    private[this] var timerTask: Option[TimerTask] = None

    private[this] def removeExpiredItems(): Seq[A] = synchronized {
        val deadline = Time.now - ttl
        @tailrec
        def constructExpiredList(acc: List[A]): List[A] = {
            Option(deque.peekLast()) match {
                case Some((ts, item)) if ts <= deadline =>
                    deque.removeLast()
                    constructExpiredList(item :: acc)
                case _ =>
                    acc
            }
        }
        constructExpiredList(Nil)
    }

    private[this] def scheduleTimer(): Unit = synchronized {
        require(!timerTask.isDefined)
        timerTask = Some(timer.schedule(ttl.fromNow) { timeout() })
    }

    private[this] def cancelTimer() = synchronized {
        timerTask foreach { _.cancel() }
        timerTask = None
    }

    private[this] def timeout() = {
        val evicted = synchronized {
            timerTask = None
            val es = removeExpiredItems()
            if (!deque.isEmpty) scheduleTimer()
            es
        }
        evicted foreach { evict(_) }
    }

    private[this] def evict(item: A) = evictor foreach { _(item) }

    def get() = synchronized {
        if (!deque.isEmpty) {
            val rv = Some(deque.pop()._2)
            if (deque.isEmpty) cancelTimer()
            rv
        } else {
            None
        }
    }

    def put(item: A) {
        val evicted = synchronized {
            if (deque.isEmpty && ttl != Duration.Top) scheduleTimer()
            deque.push((Time.now, item))
            if (deque.size > cacheSize) {
                val (time, oldest) = deque.removeLast()
                Some(oldest)
            } else {
                None
            }
        }
        evicted foreach { evict(_) }
    }

    def evictAll() = {
        val evicted = synchronized {
            val oldDeque = deque
            deque = new ArrayDeque[(Time, A)]
            cancelTimer()
            oldDeque
        }
        evicted.asScala foreach { case (_, item) => evict(item) }
    }

    def size = synchronized { deque.size }
}