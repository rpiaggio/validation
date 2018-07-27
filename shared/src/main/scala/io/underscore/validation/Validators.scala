package io.underscore.validation

import cats.{Id, _}
import cats.implicits._

import Emptyable._
import Sizeable._

import scala.util.matching.Regex
import scala.math.Ordering
import scala.language.higherKinds
import scala.language.implicitConversions

object Validators extends Validators

trait Validators {

  implicit private def liftResult[F[_] : Monad](result: Seq[ValidationResult]): F[Seq[ValidationResult]] =
    implicitly[Monad[F]].pure(result)

  def pass: Seq[ValidationResult] =
    Seq.empty

  def fail(msg: => String): Seq[ValidationResult] =
    Seq(ValidationError(msg))

  def warn(msg: => String): Seq[ValidationResult] =
    Seq(ValidationWarning(msg))

  def valid[A]: Validator[A, Id] = Validator[A]

  def validate[A]: Validator[A, Id] = Validator[A]

  class TestGenerator[A] protected[validation](msg: => String) {
    def apply[F[_] : Monad](func: A => F[Boolean]): Validator[A, F] =
      Validator[A, F] { in => func(in).map { valid => if (valid) pass else fail(msg) } }

    def apply(func: A => Boolean): Validator[A, Id] = apply[Id](func)
  }

  def test[A](msg: => String = "") = new TestGenerator[A](msg)

  def warn[A, F[_] : Monad](rule: Validator[A, F]): Validator[A, F] =
    Validator[A, F] { in => rule(in).map(_.toWarnings) }

  def optional[A, F[_] : Monad](rule: Validator[A, F]): Validator[Option[A], F] =
    Validator[Option[A], F] { in => in map rule getOrElse liftResult(pass)(implicitly[Monad[F]]) }

  def required[A, F[_] : Monad](rule: Validator[A, F]): Validator[Option[A], F] =
    required("Value is required", rule)

  def required[A, F[_] : Monad](msg: => String, rule: Validator[A, F]): Validator[Option[A], F] =
    Validator[Option[A], F] { in => in map rule getOrElse liftResult(fail(msg))(implicitly[Monad[F]]) }

  def eql[A](comp: A): Validator[A, Id] =
    eql(comp, s"Must be $comp")

  def eql[A](comp: A, msg: => String): Validator[A, Id] =
    Validator[A, Id] { in => if (in == comp) pass else fail(msg) }

  def neq[A](comp: A): Validator[A, Id] =
    neq(comp, s"Must not be $comp")

  def neq[A](comp: A, msg: => String): Validator[A, Id] =
    Validator[A, Id] { in => if (in == comp) fail(msg) else pass }

  def lt[A](comp: A)(implicit order: Ordering[_ >: A]): Validator[A, Id] =
    lt(comp, s"Must be less than $comp")

  def lt[A](comp: A, msg: => String)(implicit order: Ordering[_ >: A]): Validator[A, Id] =
    Validator[A, Id] { in => if (order.lt(in, comp)) pass else fail(msg) }

  def lte[A](comp: A)(implicit order: Ordering[_ >: A]): Validator[A, Id] =
    lte(comp, s"Must be $comp or less")

  def lte[A](comp: A, msg: => String)(implicit order: Ordering[_ >: A]): Validator[A, Id] =
    Validator[A, Id] { in => if (order.lteq(in, comp)) pass else fail(msg) }

  def gt[A](comp: A)(implicit order: Ordering[_ >: A]): Validator[A, Id] =
    gt(comp, s"Must be greater than $comp")

  def gt[A](comp: A, msg: => String)(implicit order: Ordering[_ >: A]): Validator[A, Id] =
    Validator[A, Id] { in => if (order.gt(in, comp)) pass else fail(msg) }

  def gte[A](comp: A)(implicit order: Ordering[_ >: A]): Validator[A, Id] =
    gte(comp, s"Must be $comp or higher")

  def gte[A](comp: A, msg: => String)(implicit order: Ordering[_ >: A]): Validator[A, Id] =
    Validator[A, Id] { in => if (order.gteq(in, comp)) pass else fail(msg) }

  def nonEmpty[E : Emptyable]: Validator[E, Id] =
    nonEmpty(s"Must not be empty")

  def nonEmpty[E : Emptyable](msg: => String): Validator[E, Id] =
    Validator[E, Id] { in => if (in.isEmpty) fail(msg) else pass }

  def lengthLt[E : Sizeable](comp: Int): Validator[E, Id] =
    lengthLt(comp, s"Length must be less than $comp")

  def lengthLt[E : Sizeable](comp: Int, msg: => String): Validator[E, Id] =
    Validator[E, Id] { in => if (in.size < comp) pass else fail(msg) }

  def lengthLte[E : Sizeable](comp: Int): Validator[E, Id] =
    lengthLte(comp, s"Length must be at most $comp")

  def lengthLte[E : Sizeable](comp: Int, msg: => String): Validator[E, Id] =
    Validator[E, Id] { in => if (in.size <= comp) pass else fail(msg) }

  def lengthGt[E : Sizeable](comp: Int): Validator[E, Id] =
    lengthGt(comp, s"Length must be more than $comp")

  def lengthGt[E : Sizeable](comp: Int, msg: => String): Validator[E, Id] =
    Validator[E, Id] { in => if (in.size > comp) pass else fail(msg) }

  def lengthGte[E : Sizeable](comp: Int): Validator[E, Id] =
    lengthGte(comp, s"Length must be at least $comp")

  def lengthGte[E : Sizeable](comp: Int, msg: => String): Validator[E, Id] =
    Validator[E, Id] { in => if (in.size >= comp) pass else fail(msg) }

  def matchesRegex(regex: Regex, msg: => String): Validator[String, Id] =
    Validator[String, Id] { in => if (regex.findFirstIn(in).isDefined) pass else fail(msg) }

  def notContainedIn[A](values: => Seq[A]): Validator[A, Id] =
    notContainedIn(values, s"Must not be one of the values ${values.mkString(", ")}")

  def notContainedIn[A](values: => Seq[A], msg: => String): Validator[A, Id] =
    Validator[A, Id] { in => if (values contains in) fail(msg) else pass }

  def containedIn[A](values: => Seq[A]): Validator[A, Id] =
    containedIn(values, s"Must be one of the values ${values.mkString(", ")}")

  def containedIn[A](values: => Seq[A], msg: => String): Validator[A, Id] =
    Validator[A, Id] { in => if (values contains in) pass else fail(msg) }
}
