package com.twitter.finagle

import scala.collection.mutable

sealed trait Stack[T] {
    import Stack._
    val head: Stack.Head
    def make(param: Params): T
    def transform(fn: Stack[T] => Stack[T]): Stack[T] =
        fn(this) match {
            case Node(head, mk, next) => Node(head, mk, next.transform(fn))
            case leaf @ Leaf(_, _)    => leaf
        }

}

object Stack {

    case class Role(name: String) {
        private[this] lazy val _toString = name.toLowerCase
        override def toString = _toString
    }

    trait Head {
        def role: Stack.Role
        def description: String
        def parameters: Seq[Stack.Param[_]]
    }

    case class Node[T](head: Stack.Head, mk: (Params, Stack[T]) => Stack[T], next: Stack[T])
        extends Stack[T] {
        def make(params: Params) = mk(params, next).make(params)
    }

    object Node {
        def apply[T](head: Stack.Head, mk: T => T, next: Stack[T]): Node[T] =
            Node(head, (p, stk) => Leaf(head, mk(stk.make(p))), next)
    }

    case class Leaf[T](head: Stack.Head, t: T) extends Stack[T] {
        def make(params: Params) = t
    }

    object Leaf {

    }

    trait Param[P] {

    }

    trait Params extends Iterable[(Param[_], Any)] {

    }

    object Params {

    }
}