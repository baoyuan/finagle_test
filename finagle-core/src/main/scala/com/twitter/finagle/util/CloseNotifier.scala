package com.twitter.finagle.util

import com.twitter.util.Future
import com.twitter.util.Closable
import com.twitter.util.Promise
import com.twitter.util.Return
import com.twitter.util.Time

/**
 * Allows resources to register their handlers to be invoked when service is closing.
 */
trait CloseNotifier {
    def onClose(h: => Unit)
}

object CloseNotifier {
    def makeLifo(closing: Future[Unit]): CloseNotifier = new CloseNotifier {
        @volatile private[this] var closeHandlers: List[() => Unit] = Nil

        def onClose(h: => Unit) = {
            if (closing.isDefined)
                h
            else
                closeHandlers ::= { () => h }
        }

        closing ensure { closeHandlers foreach { handler => handler() } }
    }

    def makeLifoCloser(): CloseNotifier with Closable = new CloseNotifier with Closable {
        private[this] val closing = new Promise[Unit]
        private[this] val notifier = makeLifo(closing)

        def close(deadline: Time) = {
            closing.updateIfEmpty(Return(()))
            Future.Done
        }

        def onClose(h: => Unit) = notifier.onClose(h)
    }
}