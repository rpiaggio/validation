package io.underscore.validation

import scala.language.higherKinds
import scala.language.implicitConversions

trait Sizeable[A] {
  def size(a: A): Long
}

object Sizeable extends SizeableInstances {
  def apply[A](implicit sizeable: Sizeable[A]): Sizeable[A] =
    sizeable

  def pure[A](func: A => Long): Sizeable[A] =
    new Sizeable[A] {
      def size(a: A): Long =
        func(a)
    }

  implicit class SizeableOps[A: Sizeable](a: A) {
    def size: Long = implicitly[Sizeable[A]].size(a)
  }
}

trait SizeableInstances extends LowPrioritySizeableInstances {
  implicit val sizeableString: Sizeable[String] =
    Sizeable.pure(_.length.toLong)
}

trait LowPrioritySizeableInstances {

  import cats.Foldable
  import cats.implicits._

  implicit def sizeableFoldable[F[_] : Foldable, A]: Sizeable[F[A]] =
    Sizeable.pure(_.size)

  implicit def sizeableSeq[A]: Sizeable[Seq[A]] =
    Sizeable.pure(_.length.toLong)
}
