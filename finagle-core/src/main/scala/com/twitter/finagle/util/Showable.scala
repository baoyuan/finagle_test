package com.twitter.finagle.util

trait Showable[-T] {
    def show(t: T): String
}

object Showable {
    def show[T](t: T)(implicit showable: Showable[T]): String =
        showable.show(t)
}