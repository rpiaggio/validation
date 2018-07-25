package io.underscore.validation

import cats._
import cats.effect.IO
import cats.implicits._
import monix.eval.Task
import org.specs2.mutable._

import scala.collection.immutable.HashSet
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import monix.execution.Scheduler.Implicits.global

import scala.language.higherKinds
import scala.language.postfixOps

class MonadicValidatorSpec extends Specification {

  val db = HashSet("raul@example.com", "contributor@example.com")
  def existsInDBFuture(email: String): Future[Boolean] = Future.successful(db.contains(email))
  def existsInDBTask(email: String): Task[Boolean] = Task(db.contains(email))
  def existsInDBIO(email: String): IO[Boolean] = IO(db.contains(email))

  val nonEmptyString = nonEmpty[String]
  val nonEmptyStringIO = nonEmptyString.liftTo[IO]
  val isEmail = matchesRegex("^[^@]+@[^@]+$".r, "Must be an email")
  val isEmailFuture = isEmail.liftTo[Future]
  def isUniqueFuture(implicit ec: ExecutionContext) = test[String]("Email is already registered") { email => existsInDBFuture(email).map(!_) }
  val isUniqueTask = test[String]("Email is already registered") { email => existsInDBTask(email).map(!_) }
  val isUniqueIO = test[String]("Email is already registered") { email => existsInDBIO(email).map(!_) }

  val futureToTask: Future ~> Task = new (Future ~> Task) {
    override def apply[A](f: Future[A]): Task[A] = Task.deferFuture(f)
  }

  def taskToFuture(implicit ec: ExecutionContext): Task ~> Future = new (Task ~> Future) {
    override def apply[A](t: Task[A]): Future[A] = t.runAsync
  }

  val ioToFuture: IO ~> Future = new (IO ~> Future) {
    override def apply[A](i: IO[A]): Future[A] = i.unsafeToFuture
  }

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

  "Task unique email" >> {
    val validator = isEmail and isUniqueTask
    validator("dave@example.com").runSyncUnsafe(Duration.Inf) mustEqual pass
    validator("raul@example.com").runSyncUnsafe(Duration.Inf) mustEqual fail("Email is already registered")
    validator("raul@").runSyncUnsafe(Duration.Inf)            mustEqual fail("Must be an email")
  }

  "Id, Future and Task unique non-empty email (as Task)" >> { implicit ec: ExecutionContext =>
    implicit val transformation: Future ~> Task = futureToTask

    val validator = nonEmptyString and isUniqueTask and isEmailFuture
    validator("dave@example.com").runSyncUnsafe(Duration.Inf) mustEqual pass
    validator("raul@example.com").runSyncUnsafe(Duration.Inf) mustEqual fail("Email is already registered")
    validator("raul@").runSyncUnsafe(Duration.Inf)            mustEqual fail("Must be an email")
    validator("").runSyncUnsafe(Duration.Inf).map(_.message)  must contain("Must be an email", "Must not be empty")
  }

  "Id, Future and Task unique non-empty email (as Future)" >> { implicit ec: ExecutionContext =>
    implicit val transformation: Task ~> Future = taskToFuture

    val validator = isUniqueTask and isEmailFuture and nonEmptyString
    Await.result(validator("dave@example.com"), Duration.Inf) mustEqual pass
    Await.result(validator("raul@example.com"), Duration.Inf) mustEqual fail("Email is already registered")
    Await.result(validator("raul@"), Duration.Inf)            mustEqual fail("Must be an email")
    Await.result(validator(""), Duration.Inf).map(_.message)  must contain("Must be an email", "Must not be empty")
  }

  "IO, Future and Task unique non-empty email (as Future)" >> { implicit ec: ExecutionContext =>
    implicit val transformation1: Task ~> Future = taskToFuture
    implicit val transformation2: IO ~> Future = ioToFuture

    val validator = isUniqueTask and isEmailFuture and nonEmptyStringIO
    Await.result(validator("dave@example.com"), Duration.Inf) mustEqual pass
    Await.result(validator("raul@example.com"), Duration.Inf) mustEqual fail("Email is already registered")
    Await.result(validator("raul@"), Duration.Inf)            mustEqual fail("Must be an email")
    Await.result(validator(""), Duration.Inf).map(_.message)  must contain("Must be an email", "Must not be empty")
  }
}
