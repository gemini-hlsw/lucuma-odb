// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Eq
import cats.derived.*
import cats.syntax.option.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.catalog.goa.GoaSummaryRecord
import lucuma.core.math.Angle
import lucuma.core.util.Enumerated
import lucuma.core.util.Timestamp

/**
 * The stored result of an Archive Duplication Search.
 */
object ArchiveDuplication:

  /** Archive per-query record cap. */
  val QueryLimit: Int = 500

  /**
   * The outcome of the most recent search attempt.
   *
   * `NotChecked` is the absence of an attempt and is never stored: it is what
   * an observation with no snapshot row reads as.  `NotApplicable` is an
   * attempt that concluded there is nothing to ask the archive.
   */
  enum State(val tag: String) derives Enumerated:
    case NotChecked    extends State("not_checked")
    case NotApplicable extends State("not_applicable")
    case Checked       extends State("checked")
    case Error         extends State("error")

  /**
   * The circle on the sky a search ran over: where it looked and how wide.
   * Stored alongside the results so a match is read against the search that
   * found it
   */
  final case class SearchArea(
    center: Option[ArchiveSearchPointing],
    radius: Option[Angle]
  ) derives Eq

  object SearchArea:
    val Empty: SearchArea = SearchArea(none, none)

  /**
   * A snapshot's headline values, without the matches they summarize.  Read
   * from `v_archive_duplication`, which counts `matchCount` over the matches
   * rather than storing it; the rest are columns of `t_archive_duplication`.
   * This is the storage side only — GraphQL serves the same values
   * independently, straight from the same view.
   */
  final case class Summary(
    state:         State,
    matchCount:    NonNegInt,
    saturated:     Boolean,
    lastCheckedAt: Option[Timestamp],
    error:         Option[NonEmptyString],
    searchArea:    SearchArea,
    queryUrls:     List[String]
  ) derives Eq

  object Summary:

    /** The summary of an observation that has never been searched. */
    val NotChecked: Summary =
      Summary(State.NotChecked, NonNegInt.unsafeFrom(0), false, none, none, SearchArea.Empty, Nil)

    /** The summary of a search that found nothing to ask the archive. */
    def notApplicable(at: Timestamp, searchArea: SearchArea): Summary =
      Summary(State.NotApplicable, NonNegInt.unsafeFrom(0), false, at.some, none, searchArea, Nil)

  /** The summary, together with the matched files it describes. */
  final case class Snapshot(
    summary:  Summary,
    matches: List[GoaSummaryRecord]
  )
