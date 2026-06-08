// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql

import grackle.Path
import grackle.skunk.SkunkMapping
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import resource.server.graphql.table.*

/**
 * Provides ObjectMappings for TimestampInterval and TimeSpan at arbitrary paths.
 *
 * A TimestampInterval has no single stored column for duration; it is derived from the two
 * timestamp columns (start, end) that already appear in the row.
 */
trait TimestampIntervalMapping[F[_]] extends TelescopeNightTimelineTable[F]:
  this: SkunkMapping[F] =>

  /**
   * Build ObjectMappings for a TimestampInterval field at `path`, plus the nested TimeSpan at
   * `path / "duration"`. `startCol` and `endCol` are both columns in the same table row; `keyCol`
   * is used as the Grackle key.
   */
  protected def timestampIntervalMappings(
    path:     Path,
    startCol: ColumnRef,
    endCol:   ColumnRef,
    keyCol:   ColumnRef
  ): List[TypeMapping] =
    List(
      ObjectMapping(path)(
        SqlField("_key", keyCol, key = true, hidden = true),
        SqlField("start", startCol),
        SqlField("end", endCol),
        SqlObject("duration")
      ),
      ObjectMapping(path / "duration")(
        SqlField("_key", keyCol, key = true, hidden = true),
        SqlField("_start", startCol, hidden = true),
        SqlField("_end", endCol, hidden = true),
        CursorField[Long](
          "microseconds",
          c =>
            for {
              s <- c.fieldAs[Timestamp]("_start")
              e <- c.fieldAs[Timestamp]("_end")
            } yield TimeSpan.between(s, e).toMicroseconds,
          List("_start", "_end")
        ),
        CursorField[BigDecimal](
          "milliseconds",
          c =>
            for {
              s <- c.fieldAs[Timestamp]("_start")
              e <- c.fieldAs[Timestamp]("_end")
            } yield TimeSpan.FromMilliseconds.reverseGet(TimeSpan.between(s, e)),
          List("_start", "_end")
        ),
        CursorField[BigDecimal](
          "seconds",
          c =>
            for {
              s <- c.fieldAs[Timestamp]("_start")
              e <- c.fieldAs[Timestamp]("_end")
            } yield TimeSpan.FromSeconds.reverseGet(TimeSpan.between(s, e)),
          List("_start", "_end")
        ),
        CursorField[BigDecimal](
          "minutes",
          c =>
            for {
              s <- c.fieldAs[Timestamp]("_start")
              e <- c.fieldAs[Timestamp]("_end")
            } yield TimeSpan.FromMinutes.reverseGet(TimeSpan.between(s, e)),
          List("_start", "_end")
        ),
        CursorField[BigDecimal](
          "hours",
          c =>
            for {
              s <- c.fieldAs[Timestamp]("_start")
              e <- c.fieldAs[Timestamp]("_end")
            } yield TimeSpan.FromHours.reverseGet(TimeSpan.between(s, e)),
          List("_start", "_end")
        ),
        CursorField[String](
          "iso",
          c =>
            for {
              s <- c.fieldAs[Timestamp]("_start")
              e <- c.fieldAs[Timestamp]("_end")
            } yield TimeSpan.FromString.reverseGet(TimeSpan.between(s, e)),
          List("_start", "_end")
        )
      )
    )
