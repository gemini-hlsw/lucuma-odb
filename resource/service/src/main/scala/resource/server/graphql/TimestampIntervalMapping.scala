// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql

import cats.syntax.all.*
import grackle.Cursor
import grackle.Result
import grackle.skunk.SkunkMapping
import io.circe.syntax.*
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.json.time.query.given

/**
 * Provides the `interval` field of a block.
 *
 * A TimestampInterval has no single stored column; it is derived from the two timestamp columns
 * (_start, _end) that already appear in the row. The JSON comes from the shared
 * `Encoder[TimestampInterval]`, so the block queries and the night projection produce one shape.
 */
trait TimestampIntervalMapping[F[_]] extends BaseMapping[F]:
  this: SkunkMapping[F] =>

  protected val ClipStartKey = "clipStart"
  protected val ClipEndKey   = "clipEnd"

  /**
   * The row's [start, end) trimmed to the requested window when the elaborator put
   * ClipStartKey/ClipEndKey in the Env; the stored interval otherwise. Overlap filtering guarantees
   * clipped start < clipped end.
   */
  private def clipped(c: Cursor): Result[TimestampInterval] =
    for
      s <- c.fieldAs[Timestamp]("_start")
      e <- c.fieldAs[Timestamp]("_end")
    yield TimestampInterval.between(
      c.env[Timestamp](ClipStartKey).filter(_ > s).getOrElse(s),
      c.env[Timestamp](ClipEndKey).filter(_ < e).getOrElse(e)
    )

  /** The `interval` field of a block, built from its hidden `_start` and `_end` fields. */
  protected val intervalField: CursorFieldJson =
    CursorFieldJson("interval", c => clipped(c).map(_.asJson), List("_start", "_end"))
