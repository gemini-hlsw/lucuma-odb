// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.option.*
import grackle.Result
import lucuma.odb.graphql.binding.Matcher

def atMostOne[A](options: (Option[A], String)*): Result[Option[A]] =
  options.flatMap(_._1) match
    case Nil     => Result(none[A])
    case Seq(a) => Result(a.some)
    case _       => Matcher.validationFailure(s"Expected at most one of ${options.map(_._2).mkString(", ")}")

def oneOrDefault[A](default: A)(options: (Option[A], String)*): Result[A] =
  atMostOne(options*).map(_.getOrElse(default))

def oneOrFail[A](options: (Option[A], String)*): Result[A] =
  options.flatMap(_._1) match
    case Seq(a) => Result(a)
    case _       => Matcher.validationFailure(s"Expected exactly one of ${options.map(_._2).mkString(", ")}")
