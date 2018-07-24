package io.underscore.validation

import cats.implicits._
import org.specs2.mutable._

import scala.collection.immutable.HashSet
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

class MonadicValidatorSpec extends Specification {

  val db = HashSet("raul@example.com", "contributor@example.com")
  def existsInDBFuture(email: String): Future[Boolean] = Future.successful(db.contains(email))

  val isEmail = matchesRegex("^[^@]+@[^@]+$".r, "Must be an email")
  def isUnique(implicit ec: ExecutionContext) = validate[String]("Email is already registered") { email => existsInDBFuture(email).map(!_) }

  "Future unique email" >> { implicit ec: ExecutionContext =>
    val validator = isEmail and isUnique
    Await.result(validator("dave@example.com"), Duration.Inf) mustEqual pass
    Await.result(validator("raul@example.com"), Duration.Inf) mustEqual fail("Email is already registered")
    Await.result(validator("raul@"), Duration.Inf)            mustEqual fail("Must be an email")
  }

  "Future unique email (validator reversed)" >> { implicit ec: ExecutionContext =>
    val validator = isUnique and isEmail
    Await.result(validator("dave@example.com"), Duration.Inf) mustEqual pass
    Await.result(validator("raul@example.com"), Duration.Inf) mustEqual fail("Email is already registered")
    Await.result(validator("raul@"), Duration.Inf)            mustEqual fail("Must be an email")
  }
}
