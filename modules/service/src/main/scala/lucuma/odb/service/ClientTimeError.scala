// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import skunk.exception.PostgresErrorException

/**
 * The client-time validation triggers on t_execution_event and t_visit raise a
 * dedicated SQLSTATE when a client-supplied event/visit time is outside the
 * accepted range.  Matching on that exact code (rather than "any raised
 * exception") guarantees we only translate *this* error into a typed OdbError and
 * let every other database error propagate untouched.
 *
 * Kept in sync with the `USING ERRCODE` clauses in the V1230 and V1231 migrations.
 */
object ClientTimeError:

  // Custom code; class 'OD' is not used by PostgreSQL, so nothing else raises it.
  val OutOfRangeSqlState: String = "ODB01"

  /** Matches only the client-time-out-of-range error raised by our triggers. */
  def unapply(t: Throwable): Boolean =
    t match
      case e: PostgresErrorException => e.code == OutOfRangeSqlState
      case _                         => false
