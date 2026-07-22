// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.catalog.goa.GoaObservationClass
import lucuma.catalog.goa.GoaObservationType
import lucuma.catalog.goa.GoaSummaryRecord
import lucuma.core.math.Coordinates
import lucuma.core.model.Observation
import lucuma.odb.data.ArchiveDuplication
import lucuma.odb.data.ArchiveSearchPointing
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.boolean.bool
import skunk.codec.numeric.float8
import skunk.codec.temporal.date
import skunk.codec.temporal.timestamp
import skunk.codec.text.text
import skunk.implicits.*

import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneOffset

/**
 * Storage for Archive Duplication Search snapshots.  Running the search itself
 * is the caller's job; this service only reads and replaces what was found.
 */
trait ArchiveDuplicationService[F[_]]:

  /** The stored snapshot, or an empty never-checked one if there is none. */
  def select(observationId: Observation.Id)(using Transaction[F]): F[ArchiveDuplication.Snapshot]

  /** The stored headline values, without the matches. */
  def selectSummary(observationId: Observation.Id)(using Transaction[F]): F[ArchiveDuplication.Summary]

  /**
   * Replaces any existing snapshot with this one.  There is no history: the
   * previous matches are discarded.
   */
  def store(
    observationId: Observation.Id,
    summary:        ArchiveDuplication.Summary,
    matches:       List[GoaSummaryRecord]
  )(using Transaction[F]): F[Unit]

  /**
   * Records that the most recent attempt failed, leaving any previously stored
   * matches and headline values in place so a GOA outage cannot destroy a good
   * snapshot.
   */
  def storeError(observationId: Observation.Id, message: NonEmptyString)(using Transaction[F]): F[Unit]

object ArchiveDuplicationService:

  def instantiate[F[_]: Concurrent](using Services[F]): ArchiveDuplicationService[F] =
    new ArchiveDuplicationService[F]:

      import Services.Syntax.*

      override def select(observationId: Observation.Id)(using Transaction[F]): F[ArchiveDuplication.Snapshot] =
        (selectSummary(observationId), session.execute(Statements.SelectMatches)(observationId))
          .mapN(ArchiveDuplication.Snapshot.apply)

      override def selectSummary(observationId: Observation.Id)(using Transaction[F]): F[ArchiveDuplication.Summary] =
        session
          .option(Statements.SelectSummary)(observationId)
          .map(_.getOrElse(ArchiveDuplication.Summary.NeverChecked))

      override def store(
        observationId: Observation.Id,
        summary:        ArchiveDuplication.Summary,
        matches:       List[GoaSummaryRecord]
      )(using Transaction[F]): F[Unit] =
        for
          _ <- session.execute(Statements.UpsertSummary)(observationId, summary)
          _ <- session.execute(Statements.DeleteMatches)(observationId)
          _ <- NonEmptyList.fromList(matches).traverse_ : nel =>
                 session.execute(Statements.insertMatches(nel))(observationId, nel)
        yield ()

      override def storeError(observationId: Observation.Id, message: NonEmptyString)(using Transaction[F]): F[Unit] =
        session.execute(Statements.UpsertError)(observationId, message).void

  object Statements:

    /** GOA reports UT datetimes as instants; the column is a plain UTC timestamp. */
    private val ut_datetime: Codec[Instant] =
      timestamp.imap(_.toInstant(ZoneOffset.UTC))(LocalDateTime.ofInstant(_, ZoneOffset.UTC))

    private val goa_observation_type: Codec[GoaObservationType] =
      text.imap(GoaObservationType.fromTag)(_.tag)

    private val goa_observation_class: Codec[GoaObservationClass] =
      text.imap(GoaObservationClass.fromTag)(_.tag)

    /** Columns of `t_archive_match`, in `GoaSummaryRecord` field order. */
    private val goa_match: Codec[GoaSummaryRecord] =
      (text                     *:  // c_file_name
       text.opt                 *:  // c_data_label
       right_ascension.opt      *:  // c_ra
       declination.opt          *:  // c_dec
       text                     *:  // c_instrument
       goa_observation_type     *:  // c_observation_type
       goa_observation_class.opt*:  // c_observation_class
       text.opt                 *:  // c_qa_state
       ut_datetime.opt          *:  // c_ut_datetime
       date.opt                 *:  // c_release_date
       text.opt                 *:  // c_goa_program_id
       text.opt                 *:  // c_goa_observation_id
       text.opt                 *:  // c_object_name
       time_span.opt            *:  // c_exposure
       text.opt                 *:  // c_disperser
       text.opt                 *:  // c_filter
       wavelength_pm.opt        *:  // c_wavelength
       float8.opt               *:  // c_airmass
       angle_µas.opt            *:  // c_azimuth
       angle_µas.opt                // c_elevation
      ).to[GoaSummaryRecord]

    /**
     * Summary columns, in the order the summary is selected and written.
     */
    private val archive_duplication_summary: Codec[ArchiveDuplication.Summary] =
      (archive_duplication_state *:
       int4_nonneg           *:
       bool                  *:
       core_timestamp.opt    *:
       text_nonempty.opt     *:
       right_ascension.opt   *:
       declination.opt       *:
       text_nonempty.opt     *:
       angle_µas.opt
      ).imap { (state, count, saturated, checkedAt, error, ra, dec, targetName, radius) =>
        val center = (ra, dec)
          .mapN((r, d) => ArchiveSearchPointing.Sidereal(Coordinates(r, d)))
          .orElse(targetName.map(ArchiveSearchPointing.NonSidereal(_)))
        ArchiveDuplication.Summary(
          state,
          count,
          saturated,
          checkedAt,
          error,
          ArchiveDuplication.SearchArea(center, radius)
        )
      } { h =>
        val (ra, dec, targetName) = h.searchArea.center match
          case Some(ArchiveSearchPointing.Sidereal(c))    => (c.ra.some, c.dec.some, none)
          case Some(ArchiveSearchPointing.NonSidereal(n)) => (none, none, n.some)
          case None                                 => (none, none, none)
        (h.state, h.matchCount, h.saturated, h.lastCheckedAt, h.error, ra, dec, targetName, h.searchArea.radius)
      }

    val SelectSummary: Query[Observation.Id, ArchiveDuplication.Summary] =
      sql"""
        SELECT
          c_state,
          c_match_count,
          c_saturated,
          c_last_checked_at,
          c_error,
          c_search_ra,
          c_search_dec,
          c_search_target,
          c_search_radius
        FROM v_archive_duplication
        WHERE c_observation_id = $observation_id
      """.query(archive_duplication_summary)

    val SelectMatches: Query[Observation.Id, GoaSummaryRecord] =
      sql"""
        SELECT
          c_file_name,
          c_data_label,
          c_ra,
          c_dec,
          c_instrument,
          c_observation_type,
          c_observation_class,
          c_qa_state,
          c_ut_datetime,
          c_release_date,
          c_goa_program_id,
          c_goa_observation_id,
          c_object_name,
          c_exposure,
          c_disperser,
          c_filter,
          c_wavelength,
          c_airmass,
          c_azimuth,
          c_elevation
        FROM t_archive_match
        WHERE c_observation_id = $observation_id
        ORDER BY c_file_name
      """.query(goa_match)

    val UpsertSummary: Command[(Observation.Id, ArchiveDuplication.Summary)] =
      sql"""
        INSERT INTO t_archive_duplication (
          c_observation_id,
          c_state,
          c_match_count,
          c_saturated,
          c_last_checked_at,
          c_error,
          c_search_ra,
          c_search_dec,
          c_search_target,
          c_search_radius
        ) VALUES ($observation_id, $archive_duplication_summary)
        ON CONFLICT (c_observation_id) DO UPDATE SET
          c_state           = EXCLUDED.c_state,
          c_match_count     = EXCLUDED.c_match_count,
          c_saturated       = EXCLUDED.c_saturated,
          c_last_checked_at = EXCLUDED.c_last_checked_at,
          c_error           = EXCLUDED.c_error,
          c_search_ra       = EXCLUDED.c_search_ra,
          c_search_dec      = EXCLUDED.c_search_dec,
          c_search_target   = EXCLUDED.c_search_target,
          c_search_radius   = EXCLUDED.c_search_radius
      """.command

    /**
     * Flags a failed attempt.  Only the state and message are touched, so a
     * previously good snapshot survives a GOA outage intact.
     */
    val UpsertError: Command[(Observation.Id, NonEmptyString)] =
      sql"""
        INSERT INTO t_archive_duplication (
          c_observation_id,
          c_state,
          c_error
        ) VALUES ($observation_id, 'error', $text_nonempty)
        ON CONFLICT (c_observation_id) DO UPDATE SET
          c_state = 'error',
          c_error = EXCLUDED.c_error
      """.command

    val DeleteMatches: Command[Observation.Id] =
      sql"""
        DELETE FROM t_archive_match
        WHERE c_observation_id = $observation_id
      """.command

    def insertMatches(
      matches: NonEmptyList[GoaSummaryRecord]
    ): Command[(Observation.Id, matches.type)] =
      sql"""
        INSERT INTO t_archive_match (
          c_observation_id,
          c_file_name,
          c_data_label,
          c_ra,
          c_dec,
          c_instrument,
          c_observation_type,
          c_observation_class,
          c_qa_state,
          c_ut_datetime,
          c_release_date,
          c_goa_program_id,
          c_goa_observation_id,
          c_object_name,
          c_exposure,
          c_disperser,
          c_filter,
          c_wavelength,
          c_airmass,
          c_azimuth,
          c_elevation
        ) VALUES ${(observation_id *: goa_match).values.list(matches.size)}
      """.command
         .contramap: (oid, ms) =>
           ms.toList.map((oid, _))
