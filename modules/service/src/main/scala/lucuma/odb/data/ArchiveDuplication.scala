// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
   */
  enum State(val tag: String) derives Enumerated:
    case NotChecked extends State("not_checked")
    case Checked    extends State("checked")
    case Error      extends State("error")

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
   * One row of `t_archive_duplication`: a snapshot's headline values, without the
   * matches they summarize.  This is the storage side only — GraphQL serves the
   * same values independently, straight from `v_archive_duplication`.
   */
  final case class Summary(
    state:         State,
    matchCount:    NonNegInt,
    saturated:     Boolean,
    lastCheckedAt: Option[Timestamp],
    error:         Option[NonEmptyString],
    searchArea:    SearchArea
  ) derives Eq

  object Summary:

    /** The summary of an observation that has never been searched. */
    val NeverChecked: Summary =
      Summary(State.NotChecked, NonNegInt.unsafeFrom(0), false, none, none, SearchArea.Empty)

    def notChecked(at: Timestamp, searchArea: SearchArea): Summary =
      Summary(State.NotChecked, NonNegInt.unsafeFrom(0), false, at.some, none, searchArea)

  /** The summary, together with the matched files it describes. */
  final case class Snapshot(
    summary:  Summary,
    matches: List[GoaSummaryRecord]
  )
