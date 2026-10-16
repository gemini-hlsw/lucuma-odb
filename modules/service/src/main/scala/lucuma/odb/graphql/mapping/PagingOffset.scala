// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import cats.Order
import cats.syntax.parallel.*
import grackle.Predicate
import grackle.Predicate.True
import grackle.Result
import lucuma.odb.graphql.input.oneOrDefault
import lucuma.odb.graphql.predicate.LeafPredicates

/**
 * Turns the paging arguments of a list query into a predicate on the paging
 * key. `OFFSET` (deprecated) and `OFFSET_AT` start the page at the given key,
 * `OFFSET_AFTER` after it, and at most one of the three may be given.
 */
object PagingOffset:

  /** As `predicate`, but the deprecated `OFFSET` pages by its own `offsetKey`. */
  def predicate[A: Order, B: Order](
    offsetKey:    LeafPredicates[B],
    key:          LeafPredicates[A],
    OFFSET:       Option[B],
    OFFSET_AT:    Option[A],
    OFFSET_AFTER: Option[A]
  ): Result[Predicate] =
    oneOrDefault[Predicate](True)(
      OFFSET.map(offsetKey.gtEql) -> "OFFSET",
      OFFSET_AT.map(key.gtEql)    -> "OFFSET_AT",
      OFFSET_AFTER.map(key.gt)    -> "OFFSET_AFTER"
    )

  def predicate[A: Order](
    key:          LeafPredicates[A],
    OFFSET:       Option[A],
    OFFSET_AT:    Option[A],
    OFFSET_AFTER: Option[A]
  ): Result[Predicate] =
    predicate(key, key, OFFSET, OFFSET_AT, OFFSET_AFTER)

  def predicate[A: Order](
    key:           LeafPredicates[A],
    rOFFSET:       Result[Option[A]],
    rOFFSET_AT:    Result[Option[A]],
    rOFFSET_AFTER: Result[Option[A]]
  ): Result[Predicate] =
    (rOFFSET, rOFFSET_AT, rOFFSET_AFTER).parFlatMapN(predicate(key, _, _, _))
