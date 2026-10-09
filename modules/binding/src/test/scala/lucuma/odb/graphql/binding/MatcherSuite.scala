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

  val IntOrStringBinding: Matcher[Int | String] =
    OneOfBinding("n" -> IntBinding, "s" -> StringBinding)

  val AtMostIntOrStringBinding: Matcher[Option[Int | String]] =
    AtMostOneBinding("n" -> IntBinding, "s" -> StringBinding)

  test("OneOfBinding gives the value of the one field that is set") {
    val r = IntOrStringBinding.validate(Binding("v", obj("s" -> StringValue("x"), "n" -> AbsentValue)))
    assertEquals(r, Result[Int | String]("x"))
  }

  test("OneOfBinding fails when no field is set") {
    val r = IntOrStringBinding.validate(Binding("v", obj("n" -> NullValue, "s" -> AbsentValue)))
    assertEquals(messages(r), List("Argument 'v' is invalid: Expected exactly one of n, s"))
  }

  test("OneOfBinding fails when more than one field is set") {
    val r = IntOrStringBinding.validate(Binding("v", obj("n" -> IntValue(1), "s" -> StringValue("x"))))
    assertEquals(messages(r), List("Argument 'v' is invalid: Expected exactly one of n, s"))
  }

  test("OneOfBinding keeps the problems of all invalid fields") {
    val r = IntOrStringBinding.validate(Binding("v", obj("n" -> StringValue("x"), "s" -> IntValue(1))))
    assertEquals(
      messages(r),
      List(
        "Argument 'v.n' is invalid: expected Int, found StringValue(x)",
        "Argument 'v.s' is invalid: expected String, found IntValue(1)"
      )
    )
  }

  test("OneOfBinding fails when the input has a field that is not listed") {
    val r = IntOrStringBinding.validate(Binding("v", obj("n" -> AbsentValue, "s" -> AbsentValue, "b" -> BooleanValue(true))))
    assertEquals(messages(r), List("Argument 'v' is invalid: Unhandled field(s) b; expected only n, s"))
  }

  test("OneOfBinding fails when a listed field is not in the input") {
    val r = IntOrStringBinding.validate(Binding("v", obj("n" -> IntValue(1))))
    assertEquals(messages(r), List("Argument 'v' is invalid: Missing field(s) s; expected n, s"))
  }

  test("OneOfBinding rejects duplicate field names") {
    intercept[IllegalArgumentException](OneOfBinding("n" -> IntBinding, "n" -> StringBinding))
  }

  test("AtMostOneBinding gives None when no field is set") {
    val r = AtMostIntOrStringBinding.validate(Binding("v", obj("n" -> AbsentValue, "s" -> NullValue)))
    assertEquals(r, Result(None))
  }

  test("AtMostOneBinding gives the value of the one field that is set") {
    val r = AtMostIntOrStringBinding.validate(Binding("v", obj("n" -> IntValue(1), "s" -> AbsentValue)))
    assertEquals(r, Result(Some(1)))
  }

  test("AtMostOneBinding fails when more than one field is set") {
    val r = AtMostIntOrStringBinding.validate(Binding("v", obj("n" -> IntValue(1), "s" -> StringValue("x"))))
    assertEquals(messages(r), List("Argument 'v' is invalid: Expected at most one of n, s"))
  }

  val IntOrStringOrDefaultBinding: Matcher[Int | String] =
    OneOfOrDefaultBinding[Int | String](0)("n" -> IntBinding, "s" -> StringBinding)

  test("OneOfOrDefaultBinding gives the default when no field is set") {
    val r = IntOrStringOrDefaultBinding.validate(Binding("v", obj("n" -> AbsentValue, "s" -> NullValue)))
    assertEquals(r, Result[Int | String](0))
  }

  test("OneOfOrDefaultBinding gives the value of the one field that is set") {
    val r = IntOrStringOrDefaultBinding.validate(Binding("v", obj("n" -> AbsentValue, "s" -> StringValue("x"))))
    assertEquals(r, Result[Int | String]("x"))
  }

  test("OneOfOrDefaultBinding fails when more than one field is set") {
    val r = IntOrStringOrDefaultBinding.validate(Binding("v", obj("n" -> IntValue(1), "s" -> StringValue("x"))))
    assertEquals(messages(r), List("Argument 'v' is invalid: Expected at most one of n, s"))
  }

  test("rmap passes InternalError through unchanged") {
    val boom = new RuntimeException("boom")
    val r    = IntBinding.rmap { case _ => Result.internalError[Int](boom) }.validate(Binding("n", IntValue(1)))
    assertEquals(r, Result.internalError[Int](boom))
  }

}
