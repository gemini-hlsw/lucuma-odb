// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Parallel
import cats.data.EitherNec
import cats.effect.Concurrent
import cats.syntax.all.*
import lucuma.catalog.goa.GoaClient
import lucuma.catalog.goa.GoaParams
import lucuma.catalog.goa.GoaQueryError
import lucuma.catalog.goa.GoaSummaryRecord

/**
 * Executes the GOA queries.
 */
trait GoaQueryRunner[F[_]]:

  /**
   * Runs every query, yielding one record list per `params` in the same order,
   * or all of the errors the queries produced.
   */
  def run(params: List[GoaParams]): F[EitherNec[GoaQueryError, List[List[GoaSummaryRecord]]]]

object GoaQueryRunner:

  /** Queries GOA directly.  Queries within a group are independent, so they run in parallel. */
  def fromClient[F[_]: Concurrent: Parallel](client: GoaClient[F]): GoaQueryRunner[F] =
    new GoaQueryRunner[F]:
      override def run(params: List[GoaParams]): F[EitherNec[GoaQueryError, List[List[GoaSummaryRecord]]]] =
        params.parTraverse(client.query).map(_.parSequence)
