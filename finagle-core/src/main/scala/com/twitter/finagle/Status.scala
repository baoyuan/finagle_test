package com.twitter.finagle

import com.twitter.util.{ Await, Future }

sealed class Status

object Status {
    class ClosedException extends Exception("Status was closed; expected Open")

    implicit val StatusOrdering: Ordering[Status] = Ordering.by({
        case Open    => 3
        case Busy(_) => 2
        case Closed  => 1
    })

    def worst(left: Status, right: Status): Status =
        (left, right) match {
            case (Busy(f1), Busy(f2)) => Busy(f1.join(f2).unit)
            case (left, right)        => StatusOrdering.min(left, right)
        }

    def best(left: Status, right: Status): Status =
        (left, right) match {
            case (Busy(f1), Busy(f2)) => Busy(f1.or(f2))
            case (left, right)        => StatusOrdering.max(left, right)
        }

    def worstOf[T](ts: Iterable[T], status: T => Status): Status =
        ts.foldLeft(Open: Status)((a, e) => worst(a, status(e)))

    def bestOf[T](ts: Iterable[T], status: T => Status): Status =
        ts.foldLeft(Closed: Status)((a, e) => best(a, status(e)))

    def whenOpen(get: => Status): Future[Unit] =
        get match {
            case Open    => Future.Done
            case Busy(p) => p before whenOpen(get)
            case Closed  => Future.exception(new ClosedException)
        }

    def awaitOpen(get: => Status): Unit =
        Await.result(whenOpen(get))

    case object Open extends Status

    case class Busy(until: Future[Unit]) extends Status

    case object Closed extends Status
}