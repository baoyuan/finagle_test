package com.twitter.finagle.param

import com.twitter.finagle.Stack

case class Label(label: String) {
    def mk(): (Label, Stack.Param[Label]) =
        (this, Label.param)
}

object Label {
    implicit val param = Stack.Param(Label(""))
}