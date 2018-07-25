package io.underscore.validation

import Sizeable._

import scala.language.higherKinds
import scala.language.implicitConversions

trait Emptyable[A] {
  def isEmpty(a: A): Boolean
}

object Emptyable extends EmptyableInstances {
  def apply[A](implicit emptyable: Emptyable[A]): Emptyable[A] =
    emptyable

  def pure[A](func: A => Boolean): Emptyable[A] =
    new Emptyable[A] {
      def isEmpty(a: A): Boolean =
        func(a)
    }

  implicit class EmptyableOps[A: Emptyable](a: A) {
    def isEmpty: Boolean = implicitly[Emptyable[A]].isEmpty(a)
  }

}

trait EmptyableInstances extends LowPriorityEmptyableInstances {
  implicit def emptyableSizeable[A: Sizeable]: Emptyable[A] =
    Emptyable.pure(_.size == 0)
}

trait LowPriorityEmptyableInstances {

  import cats.Monoid

  implicit def emptyableMonoid[A : Monoid]: Emptyable[A] =
    Emptyable.pure(_ == implicitly[Monoid[A]].empty)
}
