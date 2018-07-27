package io.underscore.validation

import cats._
import scala.language.higherKinds

trait NaturalTransformationLowPriorityImplicits {
  implicit def ApplicativeTransform[F[_] : Applicative]: Id ~> F = new (Id ~> F) {
    override def apply[A](a: Id[A]): F[A] = Applicative[F].pure(a)
  }
}

trait NaturalTransformationImplicits extends NaturalTransformationLowPriorityImplicits {
  implicit def idTransform[F[_] : Applicative]: F ~> F = new (F ~> F) {
    override def apply[A](fa: F[A]): F[A] = fa
  }

  implicit class LiftableOps[F[_], A](fa: F[A]) {
    def liftTo[G[_]](implicit transform: F ~> G): G[A] = transform(fa)
  }

}

trait CanLift[F[_], G[_], R[_]] {
  val evApplicativeR: Applicative[R]
  def liftF: F ~> R
  def liftG: G ~> R
}

trait CanLiftMidPriorityImplicits extends NaturalTransformationImplicits {
  implicit def CanLiftToT[F[_] : Applicative, G[_] : Applicative](implicit transform: G ~> F): CanLift[F, G, F] =
    new CanLift[F, G, F] {
      override val evApplicativeR = implicitly[Applicative[F]]
      override def liftF: F ~> F = idTransform
      override def liftG: G ~> F = transform
    }
}

trait CanLiftImplicits extends CanLiftMidPriorityImplicits {
  implicit def CanLiftToG[F[_] : Applicative, G[_] : Applicative](implicit transform: F ~> G): CanLift[F, G, G] =
    new CanLift[F, G, G] {
      override val evApplicativeR = implicitly[Applicative[G]]
      override def liftF: F ~> G = transform
      override def liftG: G ~> G = idTransform
    }

  // I can't make this general approach work. It seems that the problem is requiring the implicit evidence for Applicative[R],
  // which forces the compiler to match some arbitrary R instead of leaving it free to determine after the transforms.
  // Compiling any "and" operation results in "diverging implicit expansion for type cats.kernel.Order[A]"
//  implicit def canLiftToR[F[_] : Applicative, G[_] : Applicative, R[_] : Applicative](implicit transformF: F ~> R, transformG: G ~> R): CanLift[F, G, R] =
//    new CanLift[F, G, R] {
//      override val evApplicativeR = implicitly[Applicative[R]]
//      override def liftF: F ~> R = transformF
//      override def liftG: G ~> R = transformG
//    }
}