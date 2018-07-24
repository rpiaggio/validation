package io.underscore.validation

import cats._
import scala.language.higherKinds

sealed trait NaturalTransformation[F[_], G[_], R[_]] // R = Result

case class DirectTransformation[F[_], G[_]](transform: F ~> G) extends NaturalTransformation[F, G, G]

case class ReverseTransformation[F[_], G[_]](transform: G ~> F) extends NaturalTransformation[F, G, F]

trait ReverseTransformationImplicits {
  implicit def reverseTransform[T[_] : Monad, S[_] : Monad](implicit direct: NaturalTransformation[S, T, T]): NaturalTransformation[T, S, T] =
    ReverseTransformation(direct.asInstanceOf[DirectTransformation[S, T]].transform)
}

trait NaturalTransformationLowPriorityImplicits extends ReverseTransformationImplicits {
  implicit def monadTransform[T[_] : Monad]: NaturalTransformation[Id, T, T] = DirectTransformation(new (Id ~> T) {
    override def apply[A](a: Id[A]): T[A] = Monad[T].pure(a)
  })

  implicit def monadTransformWrap[T[_] : Monad, S[_] : Monad](implicit arrow: T ~> S): NaturalTransformation[T, S, S] =
    DirectTransformation(arrow)
}

trait NaturalTransformationImplicits extends NaturalTransformationLowPriorityImplicits {
  implicit def identityTransform[T[_]]: NaturalTransformation[T, T, T] = DirectTransformation(new (T ~> T) {
    override def apply[A](fa: T[A]): T[A] = fa
  })

  implicit class NaturalTransformationOps[F[_], A](fa: F[A]) {
    def liftTo[G[_]](implicit transform: F ~> G): G[A] = transform(fa)
  }
}
