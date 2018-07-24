package io.underscore.validation

import cats.implicits._
import monix.eval.Task
import org.specs2.mutable._

import scala.collection.immutable.HashSet
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import monix.execution.Scheduler.Implicits.global

class MonadicValidatorSpec extends Specification {

  val db = HashSet("raul@example.com", "contributor@example.com")
  def existsInDBFuture(email: String): Future[Boolean] = Future.successful(db.contains(email))
  def existsInDBTask(email: String): Task[Boolean] = Task(db.contains(email))

  val isEmail = matchesRegex("^[^@]+@[^@]+$".r, "Must be an email")
  def isUniqueFuture(implicit ec: ExecutionContext) = validate[String]("Email is already registered") { email => existsInDBFuture(email).map(!_) }
  val isUniqueTask = validate[String]("Email is already registered") { email => existsInDBTask(email).map(!_) }

  "Future unique email" >> { implicit ec: ExecutionContext =>
    val validator = isEmail and isUniqueFuture
    Await.result(validator("dave@example.com"), Duration.Inf) mustEqual pass
    Await.result(validator("raul@example.com"), Duration.Inf) mustEqual fail("Email is already registered")
    Await.result(validator("raul@"), Duration.Inf)            mustEqual fail("Must be an email")
  }

  "Future unique email (validator reversed)" >> { implicit ec: ExecutionContext =>
    val validator = isUniqueFuture and isEmail
    Await.result(validator("dave@example.com"), Duration.Inf) mustEqual pass
    Await.result(validator("raul@example.com"), Duration.Inf) mustEqual fail("Email is already registered")
    Await.result(validator("raul@"), Duration.Inf)            mustEqual fail("Must be an email")
  }

  "Task unique email" >> { implicit ec: ExecutionContext =>
    val validator = isEmail and isUniqueTask
    validator("dave@example.com").runSyncUnsafe(Duration.Inf) mustEqual pass
    validator("raul@example.com").runSyncUnsafe(Duration.Inf) mustEqual fail("Email is already registered")
    validator("raul@").runSyncUnsafe(Duration.Inf)            mustEqual fail("Must be an email")
  }

  "Task unique email (validator reversed)" >> { implicit ec: ExecutionContext =>
    val validator = isUniqueTask and isEmail
    validator("dave@example.com").runSyncUnsafe(Duration.Inf) mustEqual pass
    validator("raul@example.com").runSyncUnsafe(Duration.Inf) mustEqual fail("Email is already registered")
    validator("raul@").runSyncUnsafe(Duration.Inf)            mustEqual fail("Must be an email")
  }
}
