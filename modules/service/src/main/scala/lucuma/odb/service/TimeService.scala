// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.functor.*
import lucuma.core.enums.Site
import lucuma.core.model.ObservingNight
import lucuma.core.util.Timestamp
import skunk.*
import skunk.codec.temporal.timestamptz
import skunk.implicits.*

import Services.Syntax.*

/**
 * A service for database-time related queries.
 */
trait TimeService[F[_]]:

  /**
   * The database time associated with the current transaction.
   */
  def transactionTime(using Transaction[F]): F[Timestamp]

  /**
   * The observing night associated with the site and transaction time.
   */
  def currentObservingNight(site: Site)(using Transaction[F]): F[ObservingNight]

object TimeService:

  def instantiate[F[_]: Concurrent](using Services[F]): TimeService[F] =
    new TimeService[F]:

      override def transactionTime(using Transaction[F]): F[Timestamp] =
        session.unique(Statements.TransactionTime)

      override def currentObservingNight(site: Site)(using Transaction[F]): F[ObservingNight] =
        transactionTime.map(t => ObservingNight.fromSiteAndInstant(site, t.toInstant))

  object Statements:

    val TransactionTime: Query[Void, Timestamp] =
      sql"""SELECT CURRENT_TIMESTAMP(6)"""
        .query(timestamptz(6))
        .map(t => Timestamp.unsafeFromInstantTruncated(t.toInstant))