package io.underscore.validation

import cats.implicits._
import org.specs2.mutable._

import scala.collection.immutable.HashSet
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

class MonadicValidatorSpec extends Specification {

  val db = HashSet("raul@example.com", "contributor@example.com")
  def existsInDB(email: String): Future[Boolean] = Future.successful(db.contains(email))

  val isEmail = matchesRegex("^[^@]+@[^@]+$".r, "Must be an email")

  "Future unique email" >> { implicit ec: ExecutionContext =>
    val isUnique = validate[String]("Email is already registered") { email => existsInDB(email).map(!_) }
    val validator = isEmail and isUnique
    Await.result(validator("dave@example.com"), Duration.Inf) mustEqual pass
    Await.result(validator("raul@example.com"), Duration.Inf) mustEqual fail("Email is already registered")
    Await.result(validator("raul@"), Duration.Inf)            mustEqual fail("Must be an email")
  }
}
