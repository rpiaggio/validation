package io.underscore

import cats._
import cats.implicits._

import scala.language.implicitConversions
import scala.language.higherKinds

package object validation extends Validators with ValidationResultImplicits with ValidationPathImplicits with NaturalTransformationImplicits {

  type ValidatorId[A] = Validator[A, Id]

  implicit class ValidatableOps[A, F[_] : Monad](in: A) {
    def validate(implicit validator: Validator[A, F]): F[Validated[A]] = (validator apply in).map(_ withValue in)
  }

  implicit def functionToValidator[A, F[_] : Monad](func: A => F[Seq[ValidationResult]]): Validator[A, F] =
    Validator[A, F] { in => func(in) }
}
