package io.underscore.validation

import cats._
import cats.implicits._

import scala.language.experimental.macros
import scala.language.higherKinds

abstract class Validator[A, F[_] : Applicative] extends (A => F[Seq[ValidationResult]]) {
  def ~[G[_] : Applicative, R[_]](that: Validator[A, G])(implicit canLift: CanLift[F, G, R]): Validator[A, R] =
    this and that


  def liftWith[G[_] : Applicative](transform: F ~> G): Validator[A, G] = Validator[A, G] { in => this (in).liftTo[G](transform) }

  def liftTo[G[_] : Applicative](implicit transform: F ~> G): Validator[A, G] = liftWith(transform)

  private def andSame(that: Validator[A, F]): Validator[A, F] =
    Validator[A, F] { in =>
//      (this(in), that(in)).map2(_ ++ _) // What import do we need for this syntax???
      Apply[F].map2(this (in), that(in))(_ ++ _)
    }

  def and[G[_] : Applicative, R[_]](that: Validator[A, G])(implicit canLift: CanLift[F, G, R]): Validator[A, R] = {
    this.liftWith(canLift.liftF)(canLift.evApplicativeR) andSame that.liftWith(canLift.liftG)(canLift.evApplicativeR)
  }

  //  To correctly implement andThen, we need a Monad version of CanLift.
  //  However, there hardly seems to be a point, since all validations will be run anyway.
  //
  //  private def andThenSame(that: Validator[A, F])(implicit ev: Monad[F]): Validator[A, F] =
  //    Validator[A, F] { in =>
  //      for {thisResult <- this (in); thatResult <- that(in)} yield thisResult ++ thatResult
  //    }
  //
  //  def andThen[G[_] : Applicative, R[_] : Monad](that: Validator[A, G])(implicit canLift: CanLift[F, G, R]): Validator[A, R] = {
  //    this.liftWith(canLift.liftF)(canLift.evApplicativeR) andThenSame that.liftWith(canLift.liftG)(canLift.evApplicativeR)
  //  }

  def andPrefix[G[_] : Applicative, R[_]](fields: String*)(that: Validator[A, G])(implicit canLift: CanLift[F, G, R]): Validator[A, R] =
    this and that.prefix(fields: _*)

  def seq: Validator[Seq[A], F] =
    Validator[Seq[A], F] { seq =>
      seq.toList.zipWithIndex.traverse { case (elem, i) => this (elem).map(_.prefix(i)) }.map(_.flatten)
    }

  def contramap[B](func: B => A): Validator[B, F] =
    Validator[B, F] { in => this (func(in)) }

  def prefix[B: ValidationPathPrefix](prefixes: B*): Validator[A, F] =
    Validator[A, F] { in => this (in).map(_.prefix(prefixes: _*)) }

  def field[B, G[_] : Applicative, R[_]](field: String, accessor: A => B)(validator: Validator[B, G])(implicit canLift: CanLift[F, G, R]): Validator[A, R] =
    this and (validator contramap accessor prefix field)

  def field[B, G[_] : Applicative, R[_]](accessor: A => B)(validator: Validator[B, G])(implicit canLift: CanLift[F, G, R]): Validator[A, R] =
  macro ValidationMacros.field[A, B, F, G, R]

  def fieldImplicit[B, G[_] : Applicative, R[_]](field: String, accessor: A => B)(implicit validator: Validator[B, G], canLift: CanLift[F, G, R]): Validator[A, R] =
    this.field(field, accessor)(validator)

  def fieldImplicit[B, G[_] : Applicative, R[_]](accessor: A => B)(implicit validator: Validator[B, G], canLift: CanLift[F, G, R]): Validator[A, R] =
  macro ValidationMacros.fieldImplicit[A, B, F, G, R]

  def fieldWith[B, G[_] : Applicative, R[_]](field: String, accessor: A => B)(validatorBuilder: A => Validator[B, G])(implicit canLift: CanLift[F, G, R]): Validator[A, R] =
    this and Validator[A, G] { value =>
      val validator = validatorBuilder(value) contramap accessor prefix field
      validator(value)
    }

  def fieldWith[B, G[_] : Applicative, R[_]](accessor: A => B)(validatorBuilder: A => Validator[B, G])(implicit canLift: CanLift[F, G, R]): Validator[A, R] =
  macro ValidationMacros.fieldWith[A, B, F, G, R]


  def fieldWithImplicit[B, G[_] : Applicative, R[_]](field: String, accessor: A => B)(implicit validatorBuilder: A => Validator[B, G], canLift: CanLift[F, G, R]): Validator[A, R] =
    this.fieldWith(field, accessor)(validatorBuilder)

  def fieldWithImplicit[B, G[_] : Applicative, R[_]](accessor: A => B)(implicit validatorBuilder: A => Validator[B, G], canLift: CanLift[F, G, R]): Validator[A, R] =
  macro ValidationMacros.fieldWithImplicit[A, B, F, G, R]

  def seqField[B, G[_] : Applicative, R[_]](field: String, accessor: A => Seq[B])(validator: Validator[B, G])(implicit canLift: CanLift[F, G, R]): Validator[A, R] =
    this and (validator.seq contramap accessor prefix field)

  def seqField[B, G[_] : Applicative, R[_]](accessor: A => Seq[B])(validator: Validator[B, G])(implicit canLift: CanLift[F, G, R]): Validator[A, R] =
  macro ValidationMacros.seqField[A, B, F, G, R]


  def seqFieldImplicit[B, G[_] : Applicative, R[_]](field: String, accessor: A => Seq[B])(implicit validator: Validator[B, G], canLift: CanLift[F, G, R]): Validator[A, R] =
    this.seqField(field, accessor)(validator)

  def seqFieldImplicit[B, G[_] : Applicative, R[_]](accessor: A => Seq[B])(implicit validator: Validator[B, G], canLift: CanLift[F, G, R]): Validator[A, R] =
  macro ValidationMacros.seqFieldImplicit[A, B, F, G, R]

  def seqFieldWith[B, G[_] : Applicative, R[_]](field: String, accessor: A => Seq[B])(validatorBuilder: A => Validator[B, G])(implicit canLift: CanLift[F, G, R]): Validator[A, R] =
    this and Validator[A, G] { value =>
      val validator = validatorBuilder(value).seq contramap accessor prefix field
      validator(value)
    }

  def seqFieldWith[B, G[_] : Applicative, R[_]](accessor: A => Seq[B])(validatorBuilder: A => Validator[B, G])(implicit canLift: CanLift[F, G, R]): Validator[A, R] =
  macro ValidationMacros.seqFieldWith[A, B, F, G, R]

  def seqFieldWithImplicit[B, G[_] : Applicative, R[_]](field: String, accessor: A => Seq[B])(implicit validatorBuilder: A => Validator[B, G], canLift: CanLift[F, G, R]): Validator[A, R] =
    this.seqFieldWith(field, accessor)(validatorBuilder)

  def seqFieldWithImplicit[B, G[_] : Applicative, R[_]](accessor: A => Seq[B])(implicit validatorBuilder: A => Validator[B, G], canLift: CanLift[F, G, R]): Validator[A, R] =
  macro ValidationMacros.seqFieldWithImplicit[A, B, F, G, R]
}

object Validator {
  def apply[A] = Validator[A, Id] {
    _ => pass
  }

  def apply[A](func: A => Seq[ValidationResult]): Validator[A, Id] = apply[A, Id](func)

  def apply[A, F[_] : Applicative](func: A => F[Seq[ValidationResult]]): Validator[A, F] = new Validator[A, F] {
    def apply(in: A) = func(in)
  }
}
