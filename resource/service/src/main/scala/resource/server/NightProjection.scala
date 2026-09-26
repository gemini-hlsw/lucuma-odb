// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server

import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Query
import io.circe.Json
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.enums.Partner
import lucuma.core.enums.Site
import lucuma.core.model.ObservingNight
import lucuma.core.model.ProgramReference
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.json.time.query.given
import resource.model.ComponentLocation
import resource.model.InstrumentPlace
import resource.model.PowerSource
import resource.model.ResourceInstrument
import resource.model.ResourceUsage
import resource.model.TelescopeAvailability
import resource.model.TelescopeModeType
import resource.model.TelescopeSubsystem
import resource.model.TooSupport
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

  final case class TooRow(
    start:      Timestamp,
    end:        Timestamp,
    tooSupport: TooSupport,
    note:       Option[NonEmptyString]
  )
  final case class AvailRow(
    start:        Timestamp,
    end:          Timestamp,
    availability: TelescopeAvailability,
    port:         Option[PosInt],
    reason:       Option[NonEmptyString],
    note:         Option[NonEmptyString]
  )
  final case class ModeRow(
    start:             Timestamp,
    end:               Timestamp,
    mode:              TelescopeModeType,
    programReferences: List[ProgramReference],
    partner:           Option[Partner],
    note:              Option[NonEmptyString]
  )
  final case class InstRow(
    start:         Timestamp,
    end:           Timestamp,
    instrument:    ResourceInstrument,
    publishedName: NonEmptyString,
    place:         InstrumentPlace,
    port:          Option[PosInt],
    usage:         ResourceUsage,
    note:          Option[NonEmptyString]
  )
  final case class SubsRow(
    start:       Timestamp,
    end:         Timestamp,
    subsystem:   TelescopeSubsystem,
    usage:       ResourceUsage,
    powerSource: Option[PowerSource],
    note:        Option[NonEmptyString]
  )
  final case class CompRow(
    start:     Timestamp,
    end:       Timestamp,
    usage:     ResourceUsage,
    location:  ComponentLocation,
    note:      Option[NonEmptyString],
    component: ComponentCatalog.ComponentRow
  )

  /** The block list fields of TelescopeNight. */
  private val BlockFields =
    List("telescopeAvailability",
         "tooSupport",
         "telescopeMode",
         "instrumentAvailability",
         "subsystems",
         "components"
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
        BlockFields.filter(Query.hasField(night, _)).toSet,
        Query.hasField(night, "dataAvailable")
      )

  /**
   * A query for the blocks of one table that overlap [start, end) at a site, ordered by start. The
   * `prefix` qualifies the block columns when the `from` clause joins more than one table.
   *
   * The overlap is written against the generated `c_interval` column, which the GiST index behind
   * each table's exclusion constraint serves.
   */
  private def blockQuery[A](
    cols:   String,
    from:   String,
    dec:    skunk.Decoder[A],
    prefix: String = ""
  ): skunk.Query[(Site, Timestamp, Timestamp), A] =
    sql"""
      select #$cols
      from #$from
      where #${prefix}c_site = $site
        and #${prefix}c_interval && tsrange($core_timestamp, $core_timestamp, '[)')
      order by #${prefix}c_start, #${prefix}c_id
    """.query(dec)

  private val spanDecoder: skunk.Decoder[(Timestamp, Timestamp)] =
    core_timestamp *: core_timestamp

  /** The intervals alone, for a table that only `dataAvailable` needs. */
  private def spanQuery(from: String, prefix: String) =
    blockQuery(s"${prefix}c_start, ${prefix}c_end", from, spanDecoder, prefix)

  private val tooQuery =
    blockQuery(
      "c_start, c_end, c_too_support, c_note",
      "t_too_support_block",
      (core_timestamp *: core_timestamp *: too_support *: text_nonempty.opt).to[TooRow]
    )

  private val availQuery =
    blockQuery(
      "c_start, c_end, c_availability, c_port, c_reason, c_note",
      "t_telescope_availability_block",
      (core_timestamp *: core_timestamp *: telescope_availability *: int4_pos.opt *: text_nonempty.opt *: text_nonempty.opt)
        .to[AvailRow]
    )

  private val modeQuery =
    blockQuery(
      "c_start, c_end, c_mode, c_program_references, c_partner, c_note",
      "t_telescope_mode_block",
      (core_timestamp *: core_timestamp *: telescope_mode_type *: program_reference_array *: partner.opt *: text_nonempty.opt)
        .to[ModeRow]
    )

  private val instQuery =
    blockQuery(
      "c_start, c_end, c_instrument, c_published_name, c_place, c_port, c_usage, c_note",
      "t_instrument_availability_block",
      (core_timestamp *: core_timestamp *: resource_instrument *: text_nonempty *: instrument_place *: int4_pos.opt *: resource_usage *: text_nonempty.opt)
        .to[InstRow]
    )

  private val subsQuery =
    blockQuery(
      "c_start, c_end, c_subsystem, c_usage, c_power_source, c_note",
      "t_telescope_subsystem_block",
      (core_timestamp *: core_timestamp *: telescope_subsystem *: resource_usage *: power_source.opt *: text_nonempty.opt)
        .to[SubsRow]
    )

  private val CompFrom =
    "t_instrument_component_block b join t_instrument_component c on c.c_id = b.c_component_id"

  private val compsQuery =
    blockQuery(
      """b.c_start, b.c_end, b.c_usage, b.c_location, b.c_note,
         c.c_id, c.c_instrument, c.c_component_type, c.c_code, c.c_name, c.c_barcode, c.c_aliases, c.c_existence""",
      CompFrom,
      (core_timestamp *: core_timestamp *: resource_usage *: component_location *: text_nonempty.opt *: ComponentCatalog.componentRowCodec)
        .to[CompRow],
      "b."
    )

  private def fetch[F[_]: Concurrent, A](
    s:     Session[F],
    q:     skunk.Query[(Site, Timestamp, Timestamp), A],
    st:    Site,
    start: Timestamp,
    end:   Timestamp
  ): F[List[A]] =
    s.prepare(q).flatMap(_.stream((st, start, end), 512).compile.toList)

  /**
   * The rows a night's JSON needs. A list is empty when the client did not select it. `spans` holds
   * the intervals of the tables that only `dataAvailable` needs.
   */
  final case class NightRows(
    avail: List[AvailRow],
    too:   List[TooRow],
    mode:  List[ModeRow],
    inst:  List[InstRow],
    subs:  List[SubsRow],
    comps: List[CompRow],
    spans: List[(Timestamp, Timestamp)]
  )

  /**
   * Fetches every block the selection needs that overlaps [start, end) at the site. The queries
   * share one session, so they run one after the other.
   */
  def allRows[F[_]: Concurrent](
    s:     Session[F],
    st:    Site,
    start: Timestamp,
    end:   Timestamp,
    sel:   Selection
  ): F[NightRows] =

    def rowsOf[A](field: String, q: skunk.Query[(Site, Timestamp, Timestamp), A]): F[List[A]] =
      if sel.needsRows(field) then fetch(s, q, st, start, end) else List.empty[A].pure[F]

    def spansOf(field: String, from: String, prefix: String = ""): F[List[(Timestamp, Timestamp)]] =
      if sel.needsSpans(field) then fetch(s, spanQuery(from, prefix), st, start, end)
      else List.empty[(Timestamp, Timestamp)].pure[F]

    (rowsOf("telescopeAvailability", availQuery),
     rowsOf("tooSupport", tooQuery),
     rowsOf("telescopeMode", modeQuery),
     rowsOf("instrumentAvailability", instQuery),
     rowsOf("subsystems", subsQuery),
     rowsOf("components", compsQuery),
     List(
       spansOf("telescopeAvailability", "t_telescope_availability_block"),
       spansOf("tooSupport", "t_too_support_block"),
       spansOf("telescopeMode", "t_telescope_mode_block"),
       spansOf("instrumentAvailability", "t_instrument_availability_block"),
       spansOf("subsystems", "t_telescope_subsystem_block"),
       spansOf("components", CompFrom, "b.")
     ).sequence.map(_.flatten)
    ).mapN(NightRows.apply)

  /** The night's [start, end). */
  def nightSpan(night: ObservingNight): grackle.Result[TimestampInterval] =
    val i = night.interval
    (Timestamp.fromInstantTruncated(i.lower), Timestamp.fromInstantTruncated(i.upper)) match
      case (Some(lo), Some(hi)) => grackle.Result(TimestampInterval.between(lo, hi))
      case _                    =>
        OdbError
          .InvalidArgument("Observing night out of the supported timestamp range.".some)
          .asFailure

  def nightJson(
    st:    Site,
    night: ObservingNight,
    span:  TimestampInterval,
    rows:  NightRows,
    sel:   Selection
  ): Json =
    def overlaps(s: Timestamp, e: Timestamp): Boolean =
      TimestampInterval.between(s, e).intersects(span)

    val availN = rows.avail.filter(r => overlaps(r.start, r.end))
    val tooN   = rows.too.filter(r => overlaps(r.start, r.end))
    val modeN  = rows.mode.filter(r => overlaps(r.start, r.end))
    val instN  = rows.inst.filter(r => overlaps(r.start, r.end))
    val subsN  = rows.subs.filter(r => overlaps(r.start, r.end))
    val compsN = rows.comps.filter(r => overlaps(r.start, r.end))

    // A component may back many blocks across the range, so encode each one once.
    val compJson =
      compsN.view.map(_.component).map(c => c.id -> ComponentCatalog.componentJson(c)).toMap

    // The block is trimmed to the night. Overlap filtering guarantees that the two
    // bounds stay in order, so this is the intersection of the two intervals.
    def block(s: Timestamp, e: Timestamp)(fields: (String, Json)*): Json =
      val clipped =
        TimestampInterval.between(if s > span.start then s else span.start,
                                  if e < span.end then e else span.end
        )
      Json.obj((("site" -> st.asJson) +: ("interval" -> clipped.asJson) +: fields)*)

    def list(field: String, values: List[Json]): Option[(String, Json)] =
      Option.when(sel.needsRows(field))(field -> Json.fromValues(values))

    val dataAvailable =
      Option.when(sel.dataAvailable)(
        "dataAvailable" -> (availN.nonEmpty || tooN.nonEmpty || modeN.nonEmpty ||
          instN.nonEmpty || subsN.nonEmpty || compsN.nonEmpty ||
          rows.spans.exists(overlaps)).asJson
      )

    Json.obj(
      List(
        ("site"           -> st.asJson).some,
        ("observingNight" -> night.toLocalDate.asJson).some,
        ("interval"       -> span.asJson).some,
        dataAvailable,
        list(
          "telescopeAvailability",
          availN.map(r =>
            block(r.start, r.end)(
              "note"         -> r.note.asJson,
              "availability" -> r.availability.asJson,
              "port"         -> r.port.asJson,
              "reason"       -> r.reason.asJson
            )
          )
        ),
        list("tooSupport",
             tooN.map(r =>
               block(r.start, r.end)(
                 "note"       -> r.note.asJson,
                 "tooSupport" -> r.tooSupport.asJson
               )
             )
        ),
        list(
          "telescopeMode",
          modeN.map(r =>
            block(r.start, r.end)(
              "note"              -> r.note.asJson,
              "mode"              -> r.mode.asJson,
              "programReferences" -> r.programReferences
                .map(ProgramReference.fromString.reverseGet)
                .asJson,
              "partner"           -> r.partner.asJson
            )
          )
        ),
        list(
          "instrumentAvailability",
          instN.map(r =>
            block(r.start, r.end)(
              "note"          -> r.note.asJson,
              "instrument"    -> r.instrument.asJson,
              "publishedName" -> r.publishedName.asJson,
              "location"      -> Json.obj("place" -> r.place.asJson, "port" -> r.port.asJson),
              "usage"         -> r.usage.asJson
            )
          )
        ),
        list(
          "subsystems",
          subsN.map(r =>
            block(r.start, r.end)(
              "note"        -> r.note.asJson,
              "subsystem"   -> r.subsystem.asJson,
              "usage"       -> r.usage.asJson,
              "powerSource" -> r.powerSource.asJson
            )
          )
        ),
        list(
          "components",
          compsN.map(r =>
            block(r.start, r.end)(
              "note"      -> r.note.asJson,
              "component" -> compJson(r.component.id),
              "usage"     -> r.usage.asJson,
              "location"  -> r.location.asJson
            )
          )
        )
      ).flatten*
    )
