package io.underscore.validation

import cats._
import cats.implicits._

import scala.language.experimental.macros
import scala.language.higherKinds

abstract class Validator[A, F[_] : Monad] extends (A => F[Seq[ValidationResult]]) {
  def ~[G[_] : Monad, R[_]](that: Validator[A, G])(implicit transformation: NaturalTransformation[F, G, R]): Validator[A, R] =
    this and that

  private def andSame(that: Validator[A, F]): Validator[A, F] =
    Validator[A, F] { in =>
      for {
        vthis <- this (in)
        vthat <- that(in)
      } yield {
        vthis ++ vthat
      }
    }

  private def liftWith[G[_] : Monad](transform: F ~> G): Validator[A, G] = Validator[A, G] { in => this (in).liftTo[G](transform) }

  def and[G[_] : Monad, R[_]](that: Validator[A, G])(implicit transformation: NaturalTransformation[F, G, R]): Validator[A, R] = transformation match {
    case DirectTransformation(transform) => (this.liftWith(transform.asInstanceOf[F ~> G]) andSame that).asInstanceOf[Validator[A, R]]
    case ReverseTransformation(transform) => (this andSame that.liftWith[F](transform.asInstanceOf[G ~> F])).asInstanceOf[Validator[A, R]]
  }

  def andPrefix[G[_] : Monad, R[_]](fields: String*)(that: Validator[A, G])(implicit transformation: NaturalTransformation[F, G, R]): Validator[A, R] =
    this and that.prefix(fields: _*)

  def seq: Validator[Seq[A], F] =
    Validator[Seq[A], F] { seq =>
      seq.toList.zipWithIndex.traverse { case (elem, i) => this (elem).map(_.prefix(i)) }.map(_.flatten)
    }

  def contramap[B](func: B => A): Validator[B, F] =
    Validator[B, F] { in => this (func(in)) }

  def prefix[B: ValidationPathPrefix](prefixes: B*): Validator[A, F] =
    Validator[A, F] { in => this (in).map(_.prefix(prefixes: _*)) }

  def field[B, G[_] : Monad, R[_]](field: String, accessor: A => B)(validator: Validator[B, G])(implicit transformation: NaturalTransformation[F, G, R]): Validator[A, R] =
    this and (validator contramap accessor prefix field)

  def field[B, G[_] : Monad, R[_]](accessor: A => B)(validator: Validator[B, G])(implicit transformation: NaturalTransformation[F, G, R]): Validator[A, R] =
  macro ValidationMacros.field[A, B, F, G, R]

  def fieldWith[B, G[_] : Monad, R[_]](field: String, accessor: A => B)(validatorBuilder: A => Validator[B, G])(implicit transformation: NaturalTransformation[F, G, R]): Validator[A, R] =
    this and Validator[A, G] { value =>
      val validator = validatorBuilder(value) contramap accessor prefix field
      validator(value)
    }

  def fieldWith[B, G[_] : Monad, R[_]](accessor: A => B)(validatorBuilder: A => Validator[B, G])(implicit transformation: NaturalTransformation[F, G, R]): Validator[A, R] =
  macro ValidationMacros.fieldWith[A, B, F, G, R]

  def seqField[B, G[_] : Monad, R[_]](field: String, accessor: A => Seq[B])(validator: Validator[B, G])(implicit transformation: NaturalTransformation[F, G, R]): Validator[A, R] =
    this and (validator.seq contramap accessor prefix field)

  def seqField[B, G[_] : Monad, R[_]](accessor: A => Seq[B])(validator: Validator[B, G])(implicit transformation: NaturalTransformation[F, G, R]): Validator[A, R] =
  macro ValidationMacros.seqField[A, B, F, G, R]

  def seqFieldWith[B, G[_] : Monad, R[_]](field: String, accessor: A => Seq[B])(validatorBuilder: A => Validator[B, G])(implicit transformation: NaturalTransformation[F, G, R]): Validator[A, R] =
    this and Validator[A, G] { value =>
      val validator = validatorBuilder(value).seq contramap accessor prefix field
      validator(value)
    }

  def seqFieldWith[B, G[_] : Monad, R[_]](accessor: A => Seq[B])(validatorBuilder: A => Validator[B, G])(implicit transformation: NaturalTransformation[F, G, R]): Validator[A, R] =
  macro ValidationMacros.seqFieldWith[A, B, F, G, R]
}

object Validator {
  def apply[A, F[_] : Monad](func: A => F[Seq[ValidationResult]]): Validator[A, F] = new Validator[A, F] {
    def apply(in: A) = func(in)
  }
}
