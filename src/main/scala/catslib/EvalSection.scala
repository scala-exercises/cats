/*
 * Copyright 2016-2020 47 Degrees Open Source <https://www.47deg.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package catslib

import cats._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Eval is a data type for controlling synchronous evaluation.
 * Its implementation is designed to provide stack-safety at all times using a technique called trampolining.
 * There are two different factors that play into evaluation: memoization and laziness.
 * Memoized evaluation evaluates an expression only once and then remembers (memoizes) that value.
 * Lazy evaluation refers to when the expression is evaluated.
 * We talk about eager evaluation if the expression is immediately evaluated when defined and about lazy evaluation if the expression is evaluated when it’s first used.
 * For example, in Scala, a lazy val is both lazy and memoized, a method definition def is lazy, but not memoized, since the body will be evaluated on every call.
 * A normal val evaluates eagerly and also memoizes the result.
 * Eval is able to express all of these evaluation strategies and allows us to chain computations using its Monad instance.
 *
 * @param name Eval
 */
object EvalSection extends AnyFlatSpec with Matchers with org.scalaexercises.definitions.Section {

  /**
   * = Eval.now =
   *
   * First of the strategies is eager evaluation, we can construct an Eval eagerly using Eval.now:
   *
   * {{{
   * import cats.Eval
   * // import cats.Eval
   *
   * import cats.implicits._
   * // import cats.implicits._
   *
   * val eager = Eval.now {
   *   println("Running expensive calculation...")
   *   1 + 2 * 3
   * }
   * // Running expensive calculation...
   * // eager: cats.Eval[Int] = Now(7)
   * }}}
   *
   * We can run the computation using the given evaluation strategy anytime by using the value method.
   * eager.value
   * // res0: Int = 7
   */
  def nowEval(res0: List[Int]) = {
    //given
    val eagerEval = Eval.now {
      println("This is eagerly evaluated")
      1 :: 2 :: 3 :: Nil
    }

    //when/then
    eagerEval.value shouldBe (res0)
  }

  /**
   * = Eval.later =
   *
   * If we want lazy evaluation, we can use Eval.later
   * In this case
   *
   * {{{
   * val lazyEval = Eval.later {
   *   println("Running expensive calculation...")
   *   1 + 2 * 3
   * }
   * // lazyEval: cats.Eval[Int] = cats.Later@6c2b03e9
   *
   * lazyEval.value
   * // Running expensive calculation...
   * // res1: Int = 7
   *
   * lazyEval.value
   * // res2: Int = 7
   * }}}
   *
   * Notice that “Running expensive calculation” is printed only once, since the value was memoized internally.
   * Meaning also that the resulted operation was only computed once.
   * Eval.later is different to using a lazy val in a few different ways.
   * First, it allows the runtime to perform garbage collection of the thunk after evaluation, leading to more memory being freed earlier.
   * Secondly, when lazy vals are evaluated, in order to preserve thread-safety, the Scala compiler will lock the whole surrounding class, whereas Eval will only lock itself.
   */
  def laterEval(res0: List[Int], res1: Int) = {
    //given
    val n       = 2
    var counter = 0
    val lazyEval = Eval.later {
      println("This is lazily evaluated with caching")
      counter = counter + 1
      (1 to n)
    }

    //when/then
    List.fill(n)("").foreach(_ => lazyEval.value)
    lazyEval.value shouldBe res0
    counter shouldBe res1
  }

  /**
   * = Eval.always =
   *
   * If we want lazy evaluation, but without memoization akin to Function0, we can use Eval.always
   * Here we can see, that the expression is evaluated every time we call .value.
   * {{{
   * val alwaysEval = Eval.always(println("Always evaluated"))
   * //Always evaluated
   * alwaysEval.eval
   * //Always evaluated
   * alwaysEval.eval
   * //Always evaluated
   * alwaysEval.eval
   * }}}
   */
  def alwaysEval(res0: Int, res1: List[Int], res2: Int) = {
    //given
    val n       = 4
    var counter = 0
    val alwaysEval = Eval.always {
      println("This is lazyly evaluated without caching")
      counter = counter + 1
      (1 to n)
    }

    //when/then
    List.fill(n)("").foreach(_ => alwaysEval.value)
    counter shouldBe res0
    alwaysEval.value shouldBe res1
    counter shouldBe res2
  }

  /**
   * = Eval.defer =
   *
   * Defer a computation which produces an Eval[A] value
   * This is useful when you want to delay execution of an expression which produces an Eval[A] value. Like .flatMap, it is stack-safe.
   * Because Eval guarantees stack-safety, we can chain a lot of computations together using flatMap without fear of blowing up the stack.
   */
  def deferEval(res0: List[Int]) = {
    //given
    val list = List.fill(3)(0)

    //when
    val deferedEval: Eval[List[Int]] = Eval.now(list).flatMap(e => Eval.defer(Eval.later(e)))

    //then
    Eval.defer(deferedEval).value shouldBe res0
  }

}
