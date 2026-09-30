// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.graphql

import cats.syntax.all.*
import grackle.Cursor
import grackle.Result
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import io.circe.syntax.*
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.json.time.query.given
import resource.server.graphql.table.ResourceBlockTables

/**
 * Provides the `interval` field of a block.
 *
 * A TimestampInterval has no single stored column; it is derived from the two timestamp columns
 * (_start, _end) that already appear in the row. The JSON comes from the shared
 * `Encoder[TimestampInterval]`, so the block queries and the night projection produce one shape.
 */
trait TimestampIntervalMapping[F[_]] extends ResourceBlockTables[F]:
  this: SkunkMapping[F] =>

  protected val ClipStartKey = "clipStart"
  protected val ClipEndKey   = "clipEnd"

  /** The hidden block fields that the interval elaborator orders and clips by. */
  protected val IdField    = "_id"
  protected val StartField = "_start"
  protected val EndField   = "_end"

  /**
   * The row's [start, end) trimmed to the requested window when the elaborator put
   * ClipStartKey/ClipEndKey in the Env; the stored interval otherwise. Overlap filtering guarantees
   * that the window and the row intersect.
   */
  private def clipped(c: Cursor): Result[TimestampInterval] =
    for
      s <- c.fieldAs[Timestamp](StartField)
      e <- c.fieldAs[Timestamp](EndField)
    yield
      val stored = TimestampInterval.between(s, e)
      (c.env[Timestamp](ClipStartKey), c.env[Timestamp](ClipEndKey))
        .mapN(TimestampInterval.between)
        .flatMap(stored.intersection)
        .getOrElse(stored)

  /**
   * A block type's mapping: the fields every block shares (the hidden key, site, the hidden
   * interval bounds, the `interval` derived from them, and the note) plus the type's own.
   */
  protected def blockMapping(tpe: TypeRef, t: BlockTable)(own: FieldMapping*): ObjectMapping =
    ObjectMapping(tpe)(
      (List(
        SqlField(IdField, t.Id, key = true, hidden = true),
        SqlField("site", t.Site),
        SqlField(StartField, t.Start, hidden = true),
        SqlField(EndField, t.End, hidden = true),
        CursorFieldJson("interval", c => clipped(c).map(_.asJson), List(StartField, EndField)),
        SqlField("note", t.Note)
      ) ++ own)*
    )
