// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import cats.data.Ior
import cats.syntax.either.*
import cats.syntax.traverse.*
import grackle.Query.Binding
import grackle.Result
import grackle.Value
import grackle.Value.AbsentValue
import grackle.Value.NullValue
import lucuma.odb.data

trait Matcher[A] { outer =>

  def validate(v: Value): Either[String, A]

  final def validate(b: Binding): Result[A] =
    validate(b.value) match {
      case Left(error)  =>
        // We want to compress the paths together and the easy way is to munge the string.
        // I apologize, there is certainly a better way to do it but this works for now.
        val msg  = s"Argument '${b.name}' is invalid: $error"
        val msg0 = msg.replaceAll("' is invalid: Argument '", ".")
        Result.failure(msg0)
      case Right(value) => Result(value)
    }

  final def map[B](f: A => B): Matcher[B] = v => outer.validate(v).map(f)

  final def emap[B](f: A => Either[String, B]): Matcher[B] = v => outer.validate(v).flatMap(f)

  final def rmap[B](f: PartialFunction[A, Result[B]]): Matcher[B] = v =>
    outer.validate(v).flatMap { a =>
      f.lift(a) match {
        case Some(r) => r.toEither.leftMap(_.fold(_.getMessage, _.head.message))
        case None    => Left(s"rmap: unhandled case; no match for $v") // todo: this sucks
      }
    } // only preserves the first problem, rats

  def unapply(b: Binding): Some[(String, Result[A])] =
    Some((b.name, validate(b)))

  final def unapply(kv: (String, Value)): Some[(String, Result[A])] =
    unapply(Binding(kv._1, kv._2))

  lazy val Nullable: Matcher[data.Nullable[A]] = {
    case NullValue   => Right(data.Nullable.Null)
    case AbsentValue => Right(data.Nullable.Absent)
    case other       => outer.validate(other).map(data.Nullable.NonNull(_))
  }

  /** A matcher that disallows `NullValue` and treats `AbsentValue` as `None` */
  lazy val NonNullable: Matcher[Option[A]] = {
    case NullValue   => Left("cannot be null")
    case AbsentValue => Right(None)
    case other       => outer.validate(other).map(Some(_))
  }

  /** A matcher that treats `NullValue` and `AbsentValue` as `None` */
  lazy val Option: Matcher[Option[A]] =
    Nullable.map(_.toOption)

  /** A matcher that matches a list of `A` */
  lazy val List: Matcher[List[A]] =
    ListBinding.emap { vs =>
      // This fast-fails on the first invalid one, which is the best we can do
      vs.zipWithIndex.traverse { case (v, n) => validate(v).leftMap(s => s"at index $n: $s") }
    }

  /** If this matcher fails, try `other`. */
  def orElse[B](other: Matcher[B]): Matcher[Either[A, B]] = v =>
    outer.validate(v).map(_.asLeft).orElse(other.validate(v).map(_.asRight))

  /** Match this or `other`, or both. */
  def or[B](other: Matcher[B]): Matcher[Ior[A, B]] = v =>
    (outer.validate(v), other.validate(v)) match
      case (Left(s1), Left(s2)) => Left(s"$s1, $s2") // :-\
      case (Right(a), Left(_))  => Right(Ior.Left(a))
      case (Left(_), Right(b))  => Right(Ior.Right(b))
      case (Right(a), Right(b)) => Right(Ior.Both(a, b))

}
