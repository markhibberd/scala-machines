// Copyright   :  (C) 2012 Rúnar Bjarnason, Paul Chiusano, Dan Doel, Edward Kmett
// License     :  BSD-style (see the file LICENSE)

package com.clarifi.machines

import scalaz._
import scalaz.syntax.arrow._
import Scalaz._
import \/._

object Machine {

  /**
   * The output of machines can be folded and mapped over.
   */
  implicit def machineFunctor[K]:
      Functor[({type λ[+α] = Machine[K, α]})#λ] with
      Foldable[({type λ[+α] = Machine[K, α]})#λ] =
    new Functor[({type λ[+α] = Machine[K, α]})#λ] with
        Foldable[({type λ[+α] = Machine[K, α]})#λ] {
      def map[A, B](m: Machine[K, A])(f: A => B): Machine[K, B] = m outmap f
      def foldMap[A, B](m: Machine[K, A])(f: A => B)(implicit M: Monoid[B]) = m foldMap f
      def foldRight[A, B](m: Machine[K, A], z: => B)(f: (A, => B) => B): B = m.foldRight(z)(f)
    }

  import Plan._

  /** Processes form a category. */
  implicit object ProcessCategory extends Category[Process] {
    def id[A]: Process[A, A] = (await[A] flatMap emit) repeatedly

    def compose[A, B, C](m: Process[B, C], n: Process[A, B]): Process[A, C] =
      n andThen m
  }

  /** A machine that just relays its input. */
  def pass[K, O](h: Handle[K, O]): Machine[K, O] =
    awaits(h) flatMap { x => emit(x) } repeatedly

  /** A stopped machine that never emits output nor awaits input. */
  def stopped: Machine[Nothing, Nothing] = Stop

  /**
   * A machine that emits the individual elements of input lists.
   */
  def flattened[F[_]:Foldable, K, I](h: Handle[K, F[I]]): Machine[K, I] =
    awaits(h) flatMap (is => traversePlan_(is)(emit)) repeatedly

  /**
   * Evaluate a single step of the machine.
   */
  def step[M[+_]:Monad, K, O, A](m: Machine[K, O],
                                 feed: K => M[Option[Any]],
                                 z: O => M[A]): (M[A], Machine[K, O]) \/ M[Machine[K, O]] = m match {
    case Return(a) => a /* _|_ this is not possible, as a is of type Nothing */
    case Stop => right(Stop.pure[M])
    case Emit(o, next) => left(z(o) -> next())
    case Await(k, s, f) => right(feed(s).map(_.map(k).getOrElse(f())))
  }

}
