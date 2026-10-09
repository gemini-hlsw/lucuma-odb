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
import grackle.Value.AbsentValue
import grackle.Value.NullValue
import grackle.syntax.*
import io.circe.JsonObject
import io.circe.syntax.*
import lucuma.odb.data
import lucuma.odb.data.OdbError

trait Matcher[A] { outer =>

  import Matcher.mapProblems

  def validate(v: Value): Result[A]

  final def validate(b: Binding): Result[A] =
    // Collapse nested argument errors into one dotted path: `Argument 'a.b' is invalid: ...`
    validate(b.value).mapProblems { p =>
      val msg =
        if p.message.startsWith("Argument '") then s"Argument '${b.name}.${p.message.stripPrefix("Argument '")}"
        else s"Argument '${b.name}' is invalid: ${p.message}"
      Matcher.validationProblem(msg, p.some)
    }

  final def map[B](f: A => B): Matcher[B] = v =>
    outer.validate(v).map(f)

  final def emap[B](f: A => Either[String, B]): Matcher[B] = v =>
    outer.validate(v).flatMap(a => f(a).fold(Result.failure, _.success))

  final def rmap[B](f: PartialFunction[A, Result[B]]): Matcher[B] = v =>
    outer.validate(v).flatMap { a =>
      f.applyOrElse(a, _ => Result.failure(s"rmap: unhandled case; no match for $v"))
    }

  def unapply(b: Binding): Some[(String, Result[A])] =
    Some((b.name, validate(b)))

  final def unapply(kv: (String, Value)): Some[(String, Result[A])] =
    unapply(Binding(kv._1, kv._2))

  lazy val Nullable: Matcher[data.Nullable[A]] = {
    case NullValue   => data.Nullable.Null.success
    case AbsentValue => data.Nullable.Absent.success
    case other       => outer.validate(other).map(data.Nullable.NonNull(_))
  }

  /** A matcher that disallows `NullValue` and treats `AbsentValue` as `None` */
  lazy val NonNullable: Matcher[Option[A]] = {
    case NullValue   => Result.failure("cannot be null")
    case AbsentValue => None.success
    case other       => outer.validate(other).map(Some(_))
  }

  /** A matcher that treats `NullValue` and `AbsentValue` as `None` */
  lazy val Option: Matcher[Option[A]] =
    Nullable.map(_.toOption)

  /** A matcher that matches a list of `A` */
  lazy val List: Matcher[List[A]] =
    ListBinding.validate(_).flatMap { vs =>
      vs.zipWithIndex.parTraverse { case (v, n) =>
        outer.validate(v).mapProblems(p => p.copy(message = s"at index $n: ${p.message}"))
      }
    }

  /**
   * If this matcher fails, try `other`. If both fail, keep the problems of both, without duplicates.
   * `InternalError`s are propagated and `other` is not tried.
   */
  def orElse[B](other: Matcher[B]): Matcher[Either[A, B]] = v =>
    outer.validate(v).map(_.asLeft[B]) match
      case Result.Failure(ps1) =>
        other.validate(v).map(_.asRight[A]) match
          case Result.Failure(ps2) => Matcher.bothFailed(ps1, ps2)
          case rb                  => rb
      case ra                  => ra

  /**
   * Match this or `other`, or both. 
   * `InternalError`s on either side are not validation failures,
   * so it propagates even if the other side succeeds.
   */
  def or[B](other: Matcher[B]): Matcher[Ior[A, B]] = v =>
    (outer.validate(v), other.validate(v)) match
      case (Result.Failure(ps1), Result.Failure(ps2)) => Matcher.bothFailed(ps1, ps2)
      case (ra, Result.Failure(_))                  => ra.map(Ior.Left(_))
      case (Result.Failure(_), rb)                  => rb.map(Ior.Right(_))
      case (ra, rb)                                 => (ra, rb).parMapN(Ior.Both(_, _))

}

object Matcher:

  // N.B. in order to avoid adding a dependency on Grackle in odb-schema we need to duplicate
  // the OdbError "Problem" encoding from OdbErrorExtensions. Luckily it's very simple.
  def validationProblem(msg: String, source: Option[Problem] = None): Problem =
    val e =  OdbError.InvalidArgument(Some(msg))
    Problem(e.message, source.foldMap(_.locations), source.foldMap(_.path), Some(JsonObject(OdbError.Key -> e.asJson)))

  def validationFailure(msg: String, source: Option[Problem] = None): Result[Nothing] =
    Result.failure(validationProblem(msg, source))

  /** A `Failure` with the problems of both sides, without problems that have the same message. */
  private def bothFailed(ps1: NonEmptyChain[Problem], ps2: NonEmptyChain[Problem]): Result[Nothing] =
    Result.Failure((ps1 ++ ps2).distinctBy(_.message))

  extension [A](r: Result[A])
    /**
     * Turn each `Problem` without extensions into an `InvalidArgument` validation problem.
     * Use this for validation that runs outside of `validate(Binding)`, such as `Edit.toCreate`.
     */
    def asValidation: Result[A] =
      r.mapProblems(p => if p.extensions.isEmpty then validationProblem(p.message, p.some) else p)

    /** Map each `Problem` in a `Failure`. `Success`, `Warning` and `InternalError` do not change. */
    private[binding] def mapProblems(f: Problem => Problem): Result[A] =
      r match
        case Result.Failure(ps) => Result.Failure(ps.map(f))
        case other              => other
