package com.twitter.finagle

import scala.collection.mutable
import scala.annotation.tailrec

sealed trait Stack[T] {
    import Stack._
    val head: Stack.Head
    def make(param: Params): T
    def transform(fn: Stack[T] => Stack[T]): Stack[T] =
        fn(this) match {
            case Node(head, mk, next) => Node(head, mk, next.transform(fn))
            case leaf @ Leaf(_, _)    => leaf
        }

    def insertBefore(target: Role, insertion: Stackable[T]): Stack[T] =
        this match {
            case Node(head, mk, next) if head.role == target =>
                insertion +: Node(head, mk, next.insertBefore(target, insertion))
            case Node(head, mk, next) =>
                Node(head, mk, next.insertBefore(target, insertion))
            case leaf @ Leaf(_, _) => leaf
        }

    def insertBefore[U](target: Role, insertion: U)(implicit csf: CanStackFrom[U, T]): Stack[T] =
        insertBefore(target, csf.toStackable(target, insertion))

    def insertAfter(target: Role, insertion: Stackable[T]): Stack[T] = transform {
        case Node(head, mk, next) if head.role == target =>
            Node(head, mk, insertion +: next)
        case stk => stk
    }

    def insertAfter[U](target: Role, insertion: U)(implicit csf: CanStackFrom[U, T]): Stack[T] =
        insertAfter(target, csf.toStackable(target, insertion))

    def remove(target: Role): Stack[T] =
        this match {
            case Node(head, mk, next) =>
                if (head.role == target) next.remove(target)
                else Node(head, mk, next.remove(target))
            case leaf @ Leaf(_, _) => leaf
        }

    def replace(target: Role, replacement: Stackable[T]): Stack[T] = transform {
        case n @ Node(head, _, next) if head.role == target =>
            replacement +: next
        case stk => stk
    }

    def replace[U](target: Role, replacement: U)(implicit csf: CanStackFrom[U, T]): Stack[T] =
        replace(target, csf.toStackable(target, replacement))

    @tailrec
    final def foreach(fn: Stack[T] => Unit): Unit = {
        fn(this)
        this match {
            case Node(_, _, next) => next.foreach(fn)
            case Leaf(_, _)       =>
        }
    }

    @tailrec
    final def exists(pred: Stack[T] => Boolean): Boolean = this match {
        case _ if pred(this)  => true
        case Node(_, _, next) => next.exists(pred)
        case Leaf(_, _)       => false
    }

    def contains(role: Stack.Role): Boolean = exists(_.head.role == role)

    def tails: Iterator[Stack[T]] = {
        val buf = new mutable.ArrayBuffer[Stack[T]]
        foreach { buf += _ }
        buf.toIterator
    }

    def concat(right: Stack[T]): Stack[T] =
        this ++ right

    def ++(right: Stack[T]): Stack[T] = this match {
        case Node(head, mk, left) => Node(head, mk, left ++ right)
        case Leaf(_, _)           => right
    }

    def +:(stk: Stackable[T]): Stack[T] =
        stk.toStack(this)

    def prepend(stk: Stackable[T]): Stack[T] =
        stk +: this

    override def toString = {
        val elems = tails map {
            case Node(head, mk, _) => s"Node(role=${head.role}, description=${head.description})"
            case Leaf(head, t)     => s"Leaf(role=${head.role}, description=${head.description})"
        }
        elems mkString "\n"
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
        def apply[T](_role: Stack.Role, t: T): Leaf[T] = {
            val head = new Stack.Head {
                val role = _role
                val description = _role.toString()
                val parameters = Nil
            }
            Leaf(head, t)
        }
    }

    trait Param[P] {
        def default: P
    }

    object Param {
        def apply[T](t: => T): Param[T] = new Param[T] {
            lazy val default = t
        }
    }

    trait Params extends Iterable[(Param[_], Any)] {
        def apply[P: Param]: P
        def contains[P: Param]: Boolean
        def iterator: Iterator[(Param[_], Any)]
        def +[P: Param](p: P): Params
        def ++(ps: Params): Params =
            addAll(ps)
        def addAll(ps: Params): Params

    }

    object Params {
        private case class Prms(map: Map[Param[_], Any]) extends Params {
            def apply[P](implicit param: Param[P]): P =
                map.get(param) match {
                    case Some(v) => v.asInstanceOf[P]
                    case None    => param.default
                }
            def contains[P](implicit param: Param[P]): Boolean =
                map.contains(param)
            def iterator: Iterator[(Param[_], Any)] =
                map.iterator
            def +[P](p: P)(implicit param: Param[P]): Params =
                copy(map + (param -> p))
            def allAll(ps: Params): Params =
                copy(map ++ ps.iterator)
        }
        val empty: Params = Prms(Map.empty)
    }

    trait Parameterized[+T] {
        def params: Stack.Params
        def configured[P: Stack.Param](p: P): T =
            withParams(params + p)
        def configured[P](psp: (P, Stack.Param[P])): T = {
            val (p, sp) = psp
            configured(p)(sp)
        }
        def withParams(ps: Stack.Params): T
    }

    trait Transformer {
        def apply[Req, Rep](stack: Stack[ServiceFactory[Req, Rep]]): Stack[ServiceFactory[Req, Rep]]
    }

    trait Transformable[+T] {
        def transformed(t: Transformer): T
    }

    abstract class Module[T] extends Stackable[T] {
        def make(params: Params, next: Stack[T]): Stack[T]
        def toStack(next: Stack[T]) =
            Node(this, (prms, next) => make(prms, next), next)
    }

    abstract class Module0[T] extends Stackable[T] {
        final val parameters: Seq[Stack.Param[_]] = Nil
        def make(next: T): T
        def toStack(next: Stack[T]) =
            Node(this, (prms, next) => Leaf(this, make(next.make(prms))), next)
    }
}

trait Stackable[T] extends Stack.Head {
    def toStack(next: Stack[T]): Stack[T]
}

trait CanStackFrom[-From, To] {
    def toStackable(role: Stack.Role, el: From): Stackable[To]
}

object CanStackFrom {
    implicit def fromFun[T]: CanStackFrom[T => T, T] =
        new CanStackFrom[T => T, T] {
            def toStackable(r: Stack.Role, fn: T => T): Stackable[T] = {
                new Stack.Module0[T] {
                    val role = r
                    val description = r.name
                    def make(next: T) = fn(next)
                }
            }
        }
}

class StackBuilder[T](init: Stack[T]) {
    def this(role: Stack.Role, end: T) = this(Stack.Leaf(role, end))

    private[this] var stack = init

    def push[U](role: Stack.Role, el: U)(implicit csf: CanStackFrom[U, T]): this.type = {
        stack = csf.toStackable(role, el) +: stack
        this
    }

    def push(module: Stackable[T]): this.type = {
        stack = module +: stack
        this
    }

    def result: Stack[T] = stack

    def make(params: Stack.Params): T = result.make(params)

    override def toString = s"Builder($stack)"

}