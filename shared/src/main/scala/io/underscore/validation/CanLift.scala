package io.underscore.validation

import cats._
import scala.language.higherKinds

sealed trait CanLift[F[_], G[_], R[_]] {
  val evMonadR: Monad[R]
  def liftF: F ~> R
  def liftG: G ~> R
}

trait CanLiftLowPriorityImplicits {
  implicit def monadTransform[F[_] : Monad]: Id ~> F = new (Id ~> F) {
    override def apply[A](a: Id[A]): F[A] = Monad[F].pure(a)
  }
}

trait CanLiftMidPriorityImplicits extends CanLiftLowPriorityImplicits {
  implicit def idTransform[F[_] : Monad]: F ~> F = new (F ~> F) {
    override def apply[A](fa: F[A]): F[A] = fa
  }

  implicit def CanLiftToT[F[_] : Monad, G[_] : Monad](implicit transform: G ~> F): CanLift[F, G, F] =
    new CanLift[F, G, F] {
      override val evMonadR = implicitly[Monad[F]]
      override def liftF: F ~> F = idTransform
      override def liftG: G ~> F = transform
    }
}

trait CanLiftImplicits extends CanLiftMidPriorityImplicits {
  implicit def CanLiftToG[F[_] : Monad, G[_] : Monad](implicit transform: F ~> G): CanLift[F, G, G] =
    new CanLift[F, G, G] {
      override val evMonadR = implicitly[Monad[G]]
      override def liftF: F ~> G = transform
      override def liftG: G ~> G = idTransform
    }

  implicit class CanLiftOps[F[_], A](fa: F[A]) {
    def liftTo[G[_]](implicit transform: F ~> G): G[A] = transform(fa)
  }
}