// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server

import cats.Monad
import cats.syntax.all.*
import grackle.Query
import io.circe.Json
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.enums.Site
import lucuma.core.model.ObservingNight
import lucuma.core.model.ProgramReference
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.json.time.query.given
import resource.server.Codecs.*
import skunk.Session
import skunk.implicits.*

/**
 * Builds the TelescopeNight JSON served by the telescopeNight and telescopeNights effect handlers.
 * Blocks are always clipped to the night.
 *
 * Grackle cannot serve this shape from the block mappings: a night matches a block by interval
 * overlap, and a Grackle `Join` compares columns for equality. The JSON here must therefore match
 * what the SQL-mapped block queries produce for the same types, so the same circe encoders are
 * used.
 */
object NightProjection:

  /** One stored block: its interval and its type-specific fields, encoded once when read. */
  final case class Block(interval: TimestampInterval, fields: List[(String, Json)])

  /**
   * One block list field of TelescopeNight and the table that serves it. The block table is aliased
   * as `b`; `joins` follows it in the FROM clause. `cols` are the type-specific columns, which
   * `decoder` turns into block fields.
   */
  final case class Kind(
    field:   String,
    table:   String,
    joins:   String,
    cols:    String,
    decoder: skunk.Decoder[List[(String, Json)]]
  ):
    /**
     * The blocks of this table that overlap [start, end) at a site, ordered by start.
     *
     * The overlap is written against the generated `c_interval` column, which the GiST index behind
     * each table's exclusion constraint serves.
     */
    val query: skunk.Query[(Site, Timestamp, Timestamp), Block] =
      sql"""
        select b.c_start, b.c_end, #$cols
        from #$table b #$joins
        where b.c_site = $site
          and b.c_interval && tsrange($core_timestamp, $core_timestamp, '[)')
        order by b.c_start, b.c_id
      """.query((timestamp_interval *: decoder).to[Block])

  /**
   * The spans of every block of the given tables that overlaps [start, end) at a site, merged into
   * disjoint intervals, for when only `dataAvailable` needs those tables.
   *
   * One query serves every table: each branch of the union reads its table's GiST index, and
   * `range_agg` merges the intervals, so contiguous blocks come back as one row however many of
   * them there are. The window is a one-row CTE so the parameters are bound once.
   */
  private def spansQuery(
    kinds: List[Kind]
  ): skunk.Query[(Site, Timestamp, Timestamp), TimestampInterval] =
    val branches = kinds
      .map(k =>
        s"select b.c_interval from ${k.table} b, w where b.c_site = w.c_site and b.c_interval && w.c_window"
      )
      .mkString(" union all ")
    sql"""
      with w as (select $site as c_site, tsrange($core_timestamp, $core_timestamp, '[)') as c_window)
      select lower(r), upper(r)
      from unnest((select range_agg(u.c_interval) from (#$branches) u)) as r
    """.query(timestamp_interval)

  private val note = text_nonempty.opt

  private val Kinds: List[Kind] =
    List(
      Kind(
        "telescopeAvailability",
        "t_telescope_availability_block",
        "",
        "b.c_availability, b.c_port, b.c_reason, b.c_note",
        (telescope_availability *: int4_pos.opt *: note *: note).map { (a, p, r, n) =>
          List("availability" -> a.asJson,
               "port"         -> p.asJson,
               "reason"       -> r.asJson,
               "note"         -> n.asJson
          )
        }
      ),
      Kind(
        "tooSupport",
        "t_too_support_block",
        "",
        "b.c_too_support, b.c_note",
        (too_support *: note).map { (t, n) =>
          List("tooSupport" -> t.asJson, "note" -> n.asJson)
        }
      ),
      Kind(
        "telescopeMode",
        "t_telescope_mode_block",
        "",
        "b.c_mode, b.c_program_references, b.c_partner, b.c_note",
        (telescope_mode_type *: program_reference_array *: partner.opt *: note).map {
          (m, refs, p, n) =>
            List(
              "mode"              -> m.asJson,
              "programReferences" -> refs.map(ProgramReference.fromString.reverseGet).asJson,
              "partner"           -> p.asJson,
              "note"              -> n.asJson
            )
        }
      ),
      Kind(
        "instrumentAvailability",
        "t_instrument_availability_block",
        "",
        "b.c_instrument, b.c_published_name, b.c_place, b.c_port, b.c_usage, b.c_note",
        (resource_instrument *: text_nonempty *: instrument_place *: int4_pos.opt *: resource_usage *: note)
          .map { (i, name, place, port, u, n) =>
            List(
              "instrument"    -> i.asJson,
              "publishedName" -> name.asJson,
              "location"      -> Json.obj("place" -> place.asJson, "port" -> port.asJson),
              "usage"         -> u.asJson,
              "note"          -> n.asJson
            )
          }
      ),
      Kind(
        "subsystems",
        "t_telescope_subsystem_block",
        "",
        "b.c_subsystem, b.c_usage, b.c_power_source, b.c_note",
        (telescope_subsystem *: resource_usage *: power_source.opt *: note).map { (s, u, p, n) =>
          List("subsystem"   -> s.asJson,
               "usage"       -> u.asJson,
               "powerSource" -> p.asJson,
               "note"        -> n.asJson
          )
        }
      ),
      Kind(
        "components",
        "t_instrument_component_block",
        "join t_instrument_component c on c.c_id = b.c_component_id",
        """b.c_usage, b.c_location, b.c_note,
           c.c_id, c.c_instrument, c.c_component_type, c.c_code, c.c_name, c.c_barcode, c.c_aliases, c.c_existence""",
        (resource_usage *: component_location *: note *: ComponentCatalog.componentRowCodec).map {
          (u, l, n, c) =>
            List("usage"     -> u.asJson,
                 "location"  -> l.asJson,
                 "note"      -> n.asJson,
                 "component" -> c.asJson
            )
        }
      )
    )

  /**
   * What a client asked for. A block table is read in full only when its list was selected, and
   * read for its intervals alone when only `dataAvailable` needs it.
   */
  final case class Selection(rows: Set[String], dataAvailable: Boolean):
    def needsRows(field:  String): Boolean = rows(field)
    def needsSpans(field: String): Boolean = dataAvailable && !rows(field)

  object Selection:
    /** Reads the selection from the client's query over one TelescopeNight. */
    def fromQuery(night: Query): Selection =
      Selection(
        Kinds.map(_.field).filter(Query.hasField(night, _)).toSet,
        Query.hasField(night, "dataAvailable")
      )

  /**
   * The rows a night's JSON needs: the blocks of each selected list by field name, and the
   * intervals of the tables that only `dataAvailable` needs.
   */
  final case class NightRows(blocks: Map[String, List[Block]], spans: List[TimestampInterval])

  /**
   * Fetches every block the selection needs that overlaps [start, end) at the site, on the given
   * session. The queries run one after another, and each is quick.
   */
  def allRows[F[_]: Monad](
    session: Session[F],
    st:      Site,
    start:   Timestamp,
    end:     Timestamp,
    sel:     Selection
  ): F[NightRows] =
    val rowKinds  = Kinds.filter(k => sel.needsRows(k.field))
    val spanKinds = Kinds.filter(k => sel.needsSpans(k.field))

    def fetch[A](q: skunk.Query[(Site, Timestamp, Timestamp), A]): F[List[A]] =
      session.execute(q)((st, start, end))

    (
      rowKinds.traverse(k => fetch(k.query).tupleLeft(k.field)),
      if spanKinds.isEmpty then List.empty[TimestampInterval].pure[F]
      else fetch(spansQuery(spanKinds))
    ).mapN((blocks, spans) => NightRows(blocks.toMap, spans))

  /** The night's [start, end). */
  def nightSpan(night: ObservingNight): grackle.Result[TimestampInterval] =
    val i = night.interval
    (Timestamp.fromInstantTruncated(i.lower), Timestamp.fromInstantTruncated(i.upper))
      .mapN(TimestampInterval.between)
      .fold(
        OdbError
          .InvalidArgument("Observing night out of the supported timestamp range.".some)
          .asFailure
      )(grackle.Result(_))

  /**
   * For each span, the items that overlap it, in input order. The spans must be ascending and
   * disjoint, and the items sorted by start. One pass over the items then serves every span: an
   * item becomes active when a span reaches its start, and is dropped once a span starts at or
   * after its end. The work is linear in spans plus items plus overlaps.
   */
  private def overlapping[A](spans: List[TimestampInterval], items: List[A])(
    interval: A => TimestampInterval
  ): List[List[A]] =
    spans
      .foldLeft((items, Vector.empty[A], List.empty[List[A]])) {
        case ((pending, active, acc), span) =>
          val (starting, rest) = pending.span(a => interval(a).start < span.end)
          val live             = (active ++ starting).filter(a => interval(a).end > span.start)
          (rest, live, live.toList :: acc)
      }
      ._3
      .reverse

  /**
   * The JSON of each night, in order. The nights must be consecutive and ascending, each paired
   * with its span. The block rows arrive sorted by start, and `range_agg` returns the spans of the
   * `dataAvailable` tables in ascending order.
   */
  def nightsJson(
    st:     Site,
    nights: List[(ObservingNight, TimestampInterval)],
    rows:   NightRows,
    sel:    Selection
  ): List[Json] =
    val spans = nights.map(_._2)

    val listsByNight: List[List[(String, List[Block])]] =
      sel.rows.toList
        .map(field =>
          overlapping(spans, rows.blocks.getOrElse(field, Nil))(_.interval).map(field -> _)
        )
        .transpose
        .padTo(spans.length, Nil)

    val spansByNight: List[Boolean] =
      overlapping(spans, rows.spans)(identity).map(_.nonEmpty)

    nights.zip(listsByNight).zip(spansByNight).map { case (((night, span), lists), anySpan) =>
      nightJson(st, night, span, lists, anySpan, sel)
    }

  /** One night's JSON, from the blocks of each selected list that overlap it. */
  private def nightJson(
    st:      Site,
    night:   ObservingNight,
    span:    TimestampInterval,
    lists:   List[(String, List[Block])],
    anySpan: Boolean,
    sel:     Selection
  ): Json =
    // Each block trimmed to the night.
    def trimmed(bs: List[Block]): List[Json] =
      bs.flatMap { b =>
        b.interval
          .intersection(span)
          .map(i => Json.obj((("site" -> st.asJson) :: ("interval" -> i.asJson) :: b.fields)*))
      }

    val jsonLists = lists.map((field, bs) => field -> trimmed(bs))

    val dataAvailable =
      Option.when(sel.dataAvailable)(
        "dataAvailable" -> (jsonLists.exists(_._2.nonEmpty) || anySpan).asJson
      )

    Json.obj(
      (List(
        "site"           -> st.asJson,
        "observingNight" -> night.toLocalDate.asJson,
        "interval"       -> span.asJson
      ) ++ dataAvailable ++ jsonLists.map((f, js) => f -> Json.fromValues(js)))*
    )
