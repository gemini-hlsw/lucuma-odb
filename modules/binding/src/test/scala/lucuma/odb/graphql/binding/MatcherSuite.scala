// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import cats.data.Ior
import cats.data.NonEmptyChain
import cats.syntax.all.*
import grackle.Problem
import grackle.Query.Binding
import grackle.Result
import grackle.Value
import grackle.Value.*
import io.circe.syntax.*
import lucuma.odb.data.OdbError
import lucuma.odb.graphql.binding.Matcher.asValidation
import munit.FunSuite

class MatcherSuite extends FunSuite {

  case class Pair(a: Int, b: Int)

  val PairBinding: Matcher[Pair] =
    ObjectFieldsBinding.rmap {
      case List(
        IntBinding("a", ra),
        IntBinding("b", rb)
      ) => (ra, rb).parMapN(Pair.apply)
    }

  val NestedBinding: Matcher[Pair] =
    ObjectFieldsBinding.rmap {
      case List(PairBinding("inner", rInner)) => rInner
    }

  def obj(fields: (String, Value)*): Value =
    ObjectValue(fields.toList)

  def messages[A](r: Result[A]): List[String] =
    r.toProblems.toList.map(_.message)

  test("valid input gives Success") {
    val r = PairBinding.validate(Binding("input", obj("a" -> IntValue(1), "b" -> IntValue(2))))
    assertEquals(r, Result(Pair(1, 2)))
  }

  test("rmap keeps the problems of all invalid fields") {
    val r = PairBinding.validate(Binding("input", obj("a" -> StringValue("x"), "b" -> NullValue)))
    assertEquals(
      messages(r),
      List(
        "Argument 'input.a' is invalid: expected Int, found StringValue(x)",
        "Argument 'input.b' is invalid: Int cannot be null"
      )
    )
  }

  test("nested objects give one problem for each invalid leaf, with the full path") {
    val r = NestedBinding.validate(Binding("input", obj("inner" -> obj("a" -> NullValue, "b" -> NullValue))))
    assertEquals(
      messages(r),
      List(
        "Argument 'input.inner.a' is invalid: Int cannot be null",
        "Argument 'input.inner.b' is invalid: Int cannot be null"
      )
    )
  }

  test("List keeps the problems of all invalid elements") {
    val r = IntBinding.List.validate(Binding("ns", ListValue(List(IntValue(1), NullValue, StringValue("x")))))
    assertEquals(
      messages(r),
      List(
        "Argument 'ns' is invalid: at index 1: Int cannot be null",
        "Argument 'ns' is invalid: at index 2: expected Int, found StringValue(x)"
      )
    )
  }

  test("or keeps the problems of both when both sides fail") {
    val r = IntBinding.or(StringBinding).validate(Binding("v", NullValue))
    assertEquals(messages(r), List("Argument 'v' is invalid: Int cannot be null", "Argument 'v' is invalid: String cannot be null"))
  }

  test("or keeps problems of both sides") {
    val LenientBinding: Matcher[(Option[Int], Option[Int])] =
      ObjectFieldsBinding.rmap {
        case List(
          IntBinding.Option("a", ra),
          IntBinding.Option("b", rb)
        ) => (ra, rb).parTupled
      }
    val r = PairBinding.or(LenientBinding)
      .validate(Binding("v", obj("a" -> StringValue("x"), "b" -> AbsentValue)))
    assertEquals(messages(r), List("Argument 'v.a' is invalid: expected Int, found StringValue(x)", "Argument 'v.b' is invalid: Int is not optional"))
  }

  test("or gives Both when both sides succeed") {
    val r = IntBinding.or(LongBinding).validate(Binding("v", IntValue(1)))
    assertEquals(r, Result(Ior.Both(1, 1L)))
  }

  test("orElse keeps the problems of both sides") {
    val r = IntBinding.orElse(StringBinding).validate(Binding("v", NullValue))
    assertEquals(
      messages(r),
      List(
        "Argument 'v' is invalid: Int cannot be null",
        "Argument 'v' is invalid: String cannot be null"
      )
    )
  }

  test("orElse dedupes identical problems when both sides fail the same way") {
    val r = IntBinding.orElse(IntBinding).validate(Binding("v", NullValue))
    assertEquals(messages(r), List("Argument 'v' is invalid: Int cannot be null"))
  }

  test("orElse uses the second side when the first side fails") {
    val r = IntBinding.orElse(StringBinding).validate(Binding("v", StringValue("s")))
    assertEquals(r, Result("s".asRight[Int]))
  }

  test("emap turns Left into an invalid_argument problem") {
    val r   = IntBinding.emap(n => s"bad $n".asLeft[Int]).validate(Binding("n", IntValue(1)))
    val msg = "Argument 'n' is invalid: bad 1"
    assertEquals(messages(r), List(msg))
    assertEquals(
      r.toProblems.headOption.flatMap(_.extensions).flatMap(_(OdbError.Key)),
      Some((OdbError.InvalidArgument(Some(msg)): OdbError).asJson)
    )
  }

  test("asValidation tags untagged problems and keeps tagged problems") {
    val tagged = Matcher.validationProblem("already tagged")
    val r      = Result.Failure(NonEmptyChain(Problem("plain"), tagged)).asValidation
    assertEquals(r.toProblems.toList, List(Matcher.validationProblem("plain"), tagged))
  }

  test("rmap with no matching case is a validation failure") {
    val r = IntBinding.rmap { case 0 => Result(0) }.validate(Binding("n", IntValue(1)))
    assertEquals(messages(r), List("Argument 'n' is invalid: rmap: unhandled case; no match for IntValue(1)"))
  }

  test("Warning keeps its value and its problems unchanged") {
    val warning = Problem("careful")
    val r = IntBinding.rmap { case n => Result.warning(warning, n) }
      .validate(Binding("n", IntValue(1)))
    assertEquals(r, Result.warning(warning, 1))
  }

  test("rmap passes InternalError through unchanged") {
    val boom = new RuntimeException("boom")
    val r    = IntBinding.rmap { case _ => Result.internalError[Int](boom) }.validate(Binding("n", IntValue(1)))
    assertEquals(r, Result.internalError[Int](boom))
  }

}
