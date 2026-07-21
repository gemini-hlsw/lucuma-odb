// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Eq
import cats.derived.*
import cats.syntax.option.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.catalog.goa.GoaSummaryRecord
import lucuma.core.math.Angle
import lucuma.core.util.Enumerated
import lucuma.core.util.Timestamp

/**
 * The stored result of an Archive Duplication Search.
 */
object GoaDuplication:

  /** GOA's hard per-query record cap.  A query that returns this many is a floor. */
  val QueryLimit: Int = 500

  /**
   * The outcome of the most recent search attempt.  `NotChecked` is advisory
   * rather than a failure: the observation is one GOA cannot be asked about.
   */
  enum State(val tag: String) derives Enumerated:
    case NotChecked extends State("not_checked")
    case Checked    extends State("checked")
    case Error      extends State("error")

  /**
   * What a search was run against.  Kept so the per-match angular distance is
   * derived from the search that produced the matches, not from the
   * observation as it stands now.
   */
  final case class Provenance(
    center: Option[GoaSearchCenter],
    radius: Option[Angle]
  ) derives Eq

  object Provenance:
    val Empty: Provenance = Provenance(none, none)

  /**
   * A snapshot as stored, without the matches.  This is what the proposal PDF
   * and the observation's GraphQL field read.
   */
  final case class Header(
    state:         State,
    matchCount:    NonNegInt,
    saturated:     Boolean,
    lastCheckedAt: Option[Timestamp],
    error:         Option[String],
    provenance:    Provenance
  ) derives Eq

  object Header:

    /** The header of an observation that has never been searched. */
    val NeverChecked: Header =
      Header(State.NotChecked, NonNegInt.unsafeFrom(0), false, none, none, Provenance.Empty)

    def notChecked(at: Timestamp, provenance: Provenance): Header =
      Header(State.NotChecked, NonNegInt.unsafeFrom(0), false, at.some, none, provenance)

  /** A header together with the matched files it summarizes. */
  final case class Snapshot(
    header:  Header,
    matches: List[GoaSummaryRecord]
  )

  object Snapshot:

    val NeverChecked: Snapshot =
      Snapshot(Header.NeverChecked, Nil)
