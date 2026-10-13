// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import cats.Order
import cats.syntax.parallel.*
import grackle.Predicate
import grackle.Predicate.True
import grackle.Result
import grackle.syntax.*
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.predicate.LeafPredicates

/**
 * Turns the paging arguments of a list query into a predicate on the paging
 * key. `OFFSET` (deprecated) and `OFFSET_AT` start the page at the given key,
 * `OFFSET_AFTER` after it, and at most one of the three may be given.
 */
object PagingOffset:

  val MoreThanOne: Result[Nothing] =
    Matcher.validationFailure("Only one of `OFFSET`, `OFFSET_AT` and `OFFSET_AFTER` may be specified.")

  def predicate[A: Order](
    key:          LeafPredicates[A],
    OFFSET:       Option[A],
    OFFSET_AT:    Option[A],
    OFFSET_AFTER: Option[A]
  ): Result[Predicate] =
    (OFFSET, OFFSET_AT, OFFSET_AFTER) match
      case (None, None, None)    => True.success
      case (Some(a), None, None) => key.gtEql(a).success
      case (None, Some(a), None) => key.gtEql(a).success
      case (None, None, Some(a)) => key.gt(a).success
      case _                     => MoreThanOne

  def predicate[A: Order](
    key:           LeafPredicates[A],
    rOFFSET:       Result[Option[A]],
    rOFFSET_AT:    Result[Option[A]],
    rOFFSET_AFTER: Result[Option[A]]
  ): Result[Predicate] =
    (rOFFSET, rOFFSET_AT, rOFFSET_AFTER).parTupled.flatMap(predicate(key, _, _, _))
