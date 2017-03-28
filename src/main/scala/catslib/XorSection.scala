/*
 * scala-exercises - exercises-cats
 * Copyright (C) 2015-2016 47 Degrees, LLC. <http://www.47deg.com>
 */

package catslib

import org.scalatest._

import cats.data.Xor

object XorStyle {
  def parse(s: String): Xor[NumberFormatException, Int] =
    if (s.matches("-?[0-9]+")) Xor.right(s.toInt)
    else Xor.left(new NumberFormatException(s"${s} is not a valid integer."))

  def reciprocal(i: Int): Xor[IllegalArgumentException, Double] =
    if (i == 0) Xor.left(new IllegalArgumentException("Cannot take reciprocal of 0."))
    else Xor.right(1.0 / i)

  def stringify(d: Double): String = d.toString

  def magic(s: String): Xor[Exception, String] =
    parse(s).flatMap(reciprocal).map(stringify)
}

object XorStyleWithAdts {
  sealed abstract class Error
  final case class NotANumber(string: String) extends Error
  final case object NoZeroReciprocal          extends Error

  def parse(s: String): Xor[Error, Int] =
    if (s.matches("-?[0-9]+")) Xor.right(s.toInt)
    else Xor.left(NotANumber(s))

  def reciprocal(i: Int): Xor[Error, Double] =
    if (i == 0) Xor.left(NoZeroReciprocal)
    else Xor.right(1.0 / i)

  def stringify(d: Double): String = d.toString

  def magic(s: String): Xor[Error, String] =
    parse(s).flatMap(reciprocal).map(stringify)
}

/** In day-to-day programming, it is fairly common to find ourselves writing functions that
 * can fail. For instance, querying a service may result in a connection issue, or some
 * unexpected JSON response.
 *
 * To communicate these errors it has become common practice to throw exceptions. However,
 * exceptions are not tracked in any way, shape, or form by the Scala compiler. To see
 * what kind of exceptions (if any) a function may throw, we have to dig through the source code.
 * Then to handle these exceptions, we have to make sure we catch them at the call site.
 * This all becomes even more unwieldy when we try to compose exception-throwing procedures.
 *
 * {{{
 * val throwsSomeStuff: Int => Double = ???
 *
 * val throwsOtherThings: Double => String = ???
 *
 * val moreThrowing: String => List[Char] = ???
 *
 * val magic = throwsSomeStuff.andThen(throwsOtherThings).andThen(moreThrowing)
 * }}}
 *
 * Assume we happily throw exceptions in our code. Looking at the types, any of those functions
 * can throw any number of exceptions, we don't know. When we compose, exceptions from any of
 * the constituent functions can be thrown. Moreover, they may throw the same kind of exception
 * (e.g. `IllegalArgumentException`) and thus it gets tricky tracking exactly where that
 * exception came from.
 *
 * How then do we communicate an error? By making it explicit in the data type we return.
 *
 * =`Xor` vs `Validated`=
 *
 * In general, `Validated` is used to accumulate errors, while `Xor` is used to short-circuit a computation upon the first error. For more information, see the `Validated` vs `Xor` section of the `Validated` documentation.
 *
 * =Why not `Either`=
 *
 * `Xor` is very similar to `scala.util.Either` - in fact, they are *isomorphic* (that is,
 * any `Either` value can be rewritten as an `Xor` value, and vice versa).
 *
 * {{{
 * sealed abstract class Xor[+A, +B]
 *
 * object Xor {
 * final case class Left[+A](a: A) extends Xor[A, Nothing]
 * final case class Right[+B](b: B) extends Xor[Nothing, B]
 * }
 * }}}
 *
 * Just like `Either`, it has two type parameters. Instances of `Xor` either hold a value
 * of one type parameter, or the other. Why then does it exist at all?
 *
 * Taking a look at `Either`, we notice it lacks `flatMap` and `map` methods. In order to map
 * over an `Either[A, B]` value, we have to state which side we want to map over. For example,
 * if we want to map `Either[A, B]` to `Either[A, C]` we would need to map over the right side.
 * This can be accomplished by using the `Either#right` method, which returns a `RightProjection`
 * instance. `RightProjection` does have `flatMap` and `map` on it, which acts on the right side
 * and ignores the left - this property is referred to as "right-bias."
 *
 * {{{
 * val e1: Either[String, Int] = Right(5)
 * e1.right.map(_ + 1)
 *
 * val e2: Either[String, Int] = Left("hello")
 * e2.right.map(_ + 1)
 * }}}
 *
 * Note the return types are themselves back to `Either`, so if we want to make more calls to
 * `flatMap` or `map` then we again must call `right` or `left`.
 *
 * @param name xor
 */
object XorSection extends FlatSpec with Matchers with org.scalaexercises.definitions.Section {

  /** More often than not we want to just bias towards one side and call it a day - by convention,
   * the right side is most often chosen. This is the primary difference between `Xor` and `Either` -
   * `Xor` is right-biased. `Xor` also has some more convenient methods on it, but the most
   * crucial one is the right-biased being built-in.
   *
   */
  def xorMapRightBias(res0: String Xor Int, res1: String Xor Int) = {
    import cats.data.Xor

    val right: String Xor Int = Xor.right(5)
    right.map(_ + 1) should be(res0)

    val left: String Xor Int = Xor.left("Something went wrong")
    left.map(_ + 1) should be(res1)
  }

  /** Because `Xor` is right-biased, it is possible to define a `Monad` instance for it. You
   * could also define one for `Either` but due to how it's encoded it may seem strange to fix a
   * bias direction despite it intending to be flexible in that regard. The `Monad` instance for
   * `Xor` is consistent with the behavior of the data type itself, whereas the one for `Either`
   * would only introduce bias when `Either` is used in a generic context (a function abstracted
   * over `M[_] : Monad`).
   *
   * Since we only ever want the computation to continue in the case of `Xor.Right` (as captured
   * by the right-bias nature), we fix the left type parameter and leave the right one free.
   *
   * {{{
   * import cats.Monad
   *
   * implicit def xorMonad[Err]: Monad[Xor[Err, ?]] =
   * new Monad[Xor[Err, ?]] {
   *  def flatMap[A, B](fa: Xor[Err, A])(f: A => Xor[Err, B]): Xor[Err, B] =
   *    fa.flatMap(f)
   *
   *  def pure[A](x: A): Xor[Err, A] = Xor.right(x)
   * }
   * }}}
   *
   * So the `flatMap` method is right-biased:
   *
   */
  def xorMonad(res0: String Xor Int, res1: String Xor Int) = {
    import cats.data.Xor

    val right: String Xor Int = Xor.right(5)
    right.flatMap(x ⇒ Xor.right(x + 1)) should be(res0)

    val left: String Xor Int = Xor.left("Something went wrong")
    left.flatMap(x ⇒ Xor.right(x + 1)) should be(res1)
  }

  /** = Using `Xor` instead of exceptions =
   *
   * As a running example, we will have a series of functions that will parse a string into an integer,
   * take the reciprocal, and then turn the reciprocal into a string.
   *
   * In exception-throwing code, we would have something like this:
   *
   * {{{
   * object ExceptionStyle {
   * def parse(s: String): Int =
   *  if (s.matches("-?[0-9]+")) s.toInt
   *  else throw new NumberFormatException(s"${s} is not a valid integer.")
   *
   * def reciprocal(i: Int): Double =
   *  if (i == 0) throw new IllegalArgumentException("Cannot take reciprocal of 0.")
   *  else 1.0 / i
   *
   * def stringify(d: Double): String = d.toString
   *
   * }
   * }}}
   *
   * Instead, let's make the fact that some of our functions can fail explicit in the return type.
   *
   * {{{
   * object XorStyle {
   * def parse(s: String): Xor[NumberFormatException, Int] =
   *  if (s.matches("-?[0-9]+")) Xor.right(s.toInt)
   *  else Xor.left(new NumberFormatException(s"${s} is not a valid integer."))
   *
   * def reciprocal(i: Int): Xor[IllegalArgumentException, Double] =
   *  if (i == 0) Xor.left(new IllegalArgumentException("Cannot take reciprocal of 0."))
   *  else Xor.right(1.0 / i)
   *
   * def stringify(d: Double): String = d.toString
   *
   * def magic(s: String): Xor[Exception, String] =
   *  parse(s).flatMap(reciprocal).map(stringify)
   * }
   * }}}
   *
   * Do these calls return a `Xor.Right` value?
   *
   */
  def xorStyleParse(res0: Boolean, res1: Boolean) = {
    XorStyle.parse("Not a number").isRight should be(res0)
    XorStyle.parse("2").isRight should be(res1)
  }

  /** Now, using combinators like `flatMap` and `map`, we can compose our functions together. Will the following incantations return a `Xor.Right` value?
   *
   */
  def xorComposition(res0: Boolean, res1: Boolean, res2: Boolean) = {
    import XorStyle._

    magic("0").isRight should be(res0)
    magic("1").isRight should be(res1)
    magic("Not a number").isRight should be(res2)
  }

  /** With the composite function that we actually care about, we can pass in strings and then pattern
   * match on the exception. Because `Xor` is a sealed type (often referred to as an algebraic data type,
   * or ADT), the compiler will complain if we do not check both the `Left` and `Right` case.
   *
   * In the following exercise we pattern-match on every case the `Xor` returned by `magic` can be in.
   * If we leave out any of those clauses the compiler will yell at us, as it should. However,
   * note the `Xor.Left(_)` clause - the compiler will complain if we leave that out because it knows
   * that given the type `Xor[Exception, String]`, there can be inhabitants of `Xor.Left` that are not
   * `NumberFormatException` or `IllegalArgumentException`. However, we "know" by inspection of the source
   * that those will be the only exceptions thrown, so it seems strange to have to account for other exceptions.
   * This implies that there is still room to improve.
   *
   */
  def xorExceptions(res0: String) = {
    import XorStyle._

    val result = magic("2") match {
      case Xor.Left(_: NumberFormatException)    ⇒ "Not a number!"
      case Xor.Left(_: IllegalArgumentException) ⇒ "Can't take reciprocal of 0!"
      case Xor.Left(_)                           ⇒ "Unknown error"
      case Xor.Right(result)                     ⇒ s"Got reciprocal: ${result}"
    }
    result should be(res0)
  }

  /** Instead of using exceptions as our error value, let's instead enumerate explicitly the things that
   * can go wrong in our program.
   *
   * {{{
   * object XorStyleWithAdts {
   * sealed abstract class Error
   * final case class NotANumber(string: String) extends Error
   * final case object NoZeroReciprocal extends Error
   *
   * def parse(s: String): Xor[Error, Int] =
   *  if (s.matches("-?[0-9]+")) Xor.right(s.toInt)
   *  else Xor.left(NotANumber(s))
   *
   * def reciprocal(i: Int): Xor[Error, Double] =
   *  if (i == 0) Xor.left(NoZeroReciprocal)
   *  else Xor.right(1.0 / i)
   *
   * def stringify(d: Double): String = d.toString
   *
   * def magic(s: String): Xor[Error, String] =
   *  parse(s).flatMap(reciprocal).map(stringify)
   * }
   * }}}
   *
   * For our little module, we enumerate any and all errors that can occur. Then, instead of using
   * exception classes as error values, we use one of the enumerated cases. Now when we pattern
   * match, we get much nicer matching. Moreover, since `Error` is `sealed`, no outside code can
   * add additional subtypes which we might fail to handle.
   *
   */
  def xorErrorsAsAdts(res0: String) = {
    import XorStyleWithAdts._

    val result = magic("2") match {
      case Xor.Left(NotANumber(_))    ⇒ "Not a number!"
      case Xor.Left(NoZeroReciprocal) ⇒ "Can't take reciprocal of 0!"
      case Xor.Right(result)          ⇒ s"Got reciprocal: ${result}"
    }
    result should be(res0)
  }

  /** = Xor in the small, Xor in the large =
   *
   * Once you start using `Xor` for all your error-handling, you may quickly run into an issue where
   * you need to call into two separate modules which give back separate kinds of errors.
   *
   * {{{
   * sealed abstract class DatabaseError
   * trait DatabaseValue
   *
   * object Database {
   * def databaseThings(): Xor[DatabaseError, DatabaseValue] = ???
   * }
   *
   * sealed abstract class ServiceError
   * trait ServiceValue
   *
   * object Service {
   * def serviceThings(v: DatabaseValue): Xor[ServiceError, ServiceValue] = ???
   * }
   * }}}
   *
   * Let's say we have an application that wants to do database things, and then take database
   * values and do service things. Glancing at the types, it looks like `flatMap` will do it.
   *
   * {{{
   * def doApp = Database.databaseThings().flatMap(Service.serviceThings)
   * }}}
   *
   * This doesn't work! Well, it does, but it gives us `Xor[Object, ServiceValue]` which isn't
   * particularly useful for us. Now if we inspect the `Left`s, we have no clue what it could be.
   * The reason this occurs is because the first type parameter in the two `Xor`s are different -
   * `databaseThings()` can give us a `DatabaseError` whereas `serviceThings()` can give us a
   * `ServiceError`: two completely unrelated types. Recall that the type parameters of `Xor`
   * are covariant, so when it sees an `Xor[E1, A1]` and an `Xor[E2, A2]`, it will happily try
   * to unify the `E1` and `E2` in a `flatMap` call - in our case, the closest common supertype is
   * `Object`, leaving us with practically no type information to use in our pattern match.
   *
   * == Solution 1: Application-wide errors ==
   *
   * So clearly in order for us to easily compose `Xor` values, the left type parameter must be the same.
   * We may then be tempted to make our entire application share an error data type.
   *
   * {{{
   * sealed abstract class AppError
   * final case object DatabaseError1 extends AppError
   * final case object DatabaseError2 extends AppError
   * final case object ServiceError1 extends AppError
   * final case object ServiceError2 extends AppError
   *
   * trait DatabaseValue
   *
   * object Database {
   * def databaseThings(): Xor[AppError, DatabaseValue] = ???
   * }
   *
   * object Service {
   * def serviceThings(v: DatabaseValue): Xor[AppError, ServiceValue] = ???
   * }
   *
   * def doApp = Database.databaseThings().flatMap(Service.serviceThings)
   * }}}
   *
   * This certainly works, or at least it compiles. But consider the case where another module wants to just use
   * `Database`, and gets an `Xor[AppError, DatabaseValue]` back. Should it want to inspect the errors, it
   * must inspect **all** the `AppError` cases, even though it was only intended for `Database` to use
   * `DatabaseError1` or `DatabaseError2`.
   *
   * == Solution 2: ADTs all the way down ==
   *
   * Instead of lumping all our errors into one big ADT, we can instead keep them local to each module, and have
   * an application-wide error ADT that wraps each error ADT we need.
   *
   * {{{
   * sealed abstract class DatabaseError
   * trait DatabaseValue
   *
   * object Database {
   * def databaseThings(): Xor[DatabaseError, DatabaseValue] = ???
   * }
   *
   * sealed abstract class ServiceError
   * trait ServiceValue
   *
   * object Service {
   * def serviceThings(v: DatabaseValue): Xor[ServiceError, ServiceValue] = ???
   * }
   *
   * sealed abstract class AppError
   * object AppError {
   * final case class Database(error: DatabaseError) extends AppError
   * final case class Service(error: ServiceError) extends AppError
   * }
   * }}}
   *
   * Now in our outer application, we can wrap/lift each module-specific error into `AppError` and then
   * call our combinators as usual. `Xor` provides a convenient method to assist with this, called `Xor.leftMap` -
   * it can be thought of as the same as `map`, but for the `Left` side.
   *
   * {{{
   * def doApp: Xor[AppError, ServiceValue] =
   * Database.databaseThings().leftMap(AppError.Database).
   * flatMap(dv => Service.serviceThings(dv).leftMap(AppError.Service))
   * }}}
   *
   * Hurrah! Each module only cares about its own errors as it should be, and more composite modules have their
   * own error ADT that encapsulates each constituent module's error ADT. Doing this also allows us to take action
   * on entire classes of errors instead of having to pattern match on each individual one.
   *
   * {{{
   * def awesome =
   * doApp match {
   *  case Xor.Left(AppError.Database(_)) => "something in the database went wrong"
   *  case Xor.Left(AppError.Service(_))  => "something in the service went wrong"
   *  case Xor.Right(_)                   => "everything is alright!"
   * }
   * }}}
   *
   * Let's review the `leftMap` and `map` methods:
   *
   */
  def xorInTheLarge(res0: String Xor Int, res1: String Xor Int, res2: String Xor Int) = {
    val right: String Xor Int = Xor.Right(41)
    right.map(_ + 1) should be(res0)

    val left: String Xor Int = Xor.Left("Hello")
    left.map(_ + 1) should be(res1)
    left.leftMap(_.reverse) should be(res2)
  }

  /** There will inevitably come a time when your nice `Xor` code will have to interact with exception-throwing
   * code. Handling such situations is easy enough.
   *
   * {{{
   * val xor: Xor[NumberFormatException, Int] =
   * try {
   *  Xor.right("abc".toInt)
   * } catch {
   *  case nfe: NumberFormatException => Xor.left(nfe)
   * }
   * }}}
   *
   * However, this can get tedious quickly. `Xor` provides a `catchOnly` method on its companion object
   * that allows you to pass it a function, along with the type of exception you want to catch, and does the
   * above for you.
   *
   * {{{
   * val xor: Xor[NumberFormatException, Int] =
   * Xor.catchOnly[NumberFormatException]("abc".toInt)
   * }}}
   *
   * If you want to catch all (non-fatal) throwables, you can use `catchNonFatal`.
   *
   *
   */
  def xorWithExceptions(res0: Boolean, res1: Boolean) = {
    Xor.catchOnly[NumberFormatException]("abc".toInt).isRight should be(res0)

    Xor.catchNonFatal(1 / 0).isLeft should be(res1)
  }

  /** = Additional syntax =
   *
   * For using Xor's syntax on arbitrary data types, you can import `cats.implicits._`. This will
   * make possible to use the `left` and `right` methods:
   *
   * {{{
   * import cats.implicits._
   *
   * val right: Xor[String, Int] = 7.right[String]
   *
   * val left: Xor[String, Int] = "hello 🐈s".left[Int]
   * }}}
   *
   * These method promote values to the `Xor` data type:
   *
   */
  def xorSyntax(res0: String Xor Int) = {
    import cats.implicits._

    val right: Xor[String, Int] = 42.right[String]
    right should be(res0)
  }
}
