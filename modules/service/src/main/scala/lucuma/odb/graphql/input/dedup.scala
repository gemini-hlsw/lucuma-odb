// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.Traverse
import cats.syntax.traverse.*
import grackle.Result
import grackle.syntax.*
import lucuma.odb.graphql.binding.Matcher

/**
 * Fails with a validation error if the list contains duplicates according to
 * the mapping function `f`, formatting the duplicate values with `toString` in
 * the error message.
 */
private[input] def mapDedup[F[_]: Traverse, A, B](
  name: String,
  in:   Result[F[List[A]]]
)(
  f:        A => B,
  toString: B => String
): Result[F[List[A]]] =
  in.flatMap: fas =>
    fas.traverse: as =>
      val bs   = as.map(f)
      val dups = bs.diff(bs.distinct)
      if dups.isEmpty then as.success
      else Matcher.validationFailure(s"duplicate '$name' specified: ${dups.map(toString).mkString(", ")}")

private[input] def dedup[F[_]: Traverse, A](
  name: String,
  in:   Result[F[List[A]]]
)(
  toString: A => String
): Result[F[List[A]]] =
  mapDedup(name, in)(identity, toString)