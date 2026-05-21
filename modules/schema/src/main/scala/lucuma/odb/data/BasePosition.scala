// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.data.NonEmptyList
import cats.syntax.foldable.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.math.Coordinates
import lucuma.core.model.Target
import lucuma.core.util.Enumerated

// TODO: move to lucuma-core
enum BasePositionType(val tag: String) derives Enumerated:
  case SingleTarget extends BasePositionType("SingleTarget")
  case Asterism     extends BasePositionType("Asterism")
  case ExplicitBase extends BasePositionType("ExplicitBase")

/**
 * The base position toward which the telescope must point.
 *
 * Invariant: at most one of `sidereal` / `nonsidereal` / `coordinates` is populated.
 * Opportunity targets have no meaningful base position; in that case the surrounding
 * code returns `None` rather than constructing a `BasePosition`.
 */
case class BasePosition(
  basePositionType: BasePositionType,
  name:             NonEmptyString,
  sidereal:         Option[Target.Sidereal],
  nonsidereal:      Option[Target.Nonsidereal],
  coordinates:      Option[Coordinates]
)

object BasePosition:

  // Max length for the name, limited by epics
  val MaxNameLength: Int = 48

  private val Ellipsis = "..."

  def truncate(s: NonEmptyString): NonEmptyString =
    if s.value.length <= MaxNameLength then s
    // truncated result is always non-empty
    else NonEmptyString.unsafeFrom(s.value.take(MaxNameLength - Ellipsis.length) + Ellipsis)

  /** Compose a display name from an asterism */
  def composeName(targets: NonEmptyList[Target]): NonEmptyString =
    // each target name is non empty thus the joined result is always non-empty
    truncate(NonEmptyString.unsafeFrom(targets.map(_.name.value).mkString_(", ")))
