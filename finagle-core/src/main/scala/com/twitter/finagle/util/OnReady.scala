package com.twitter.finagle.util

import com.twitter.util.Future

trait OnReady {
    def onReady: Future[Unit]
}