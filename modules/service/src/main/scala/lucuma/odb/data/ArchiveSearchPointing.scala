// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.math.Coordinates

/**
 * The single point an Archive Duplication Search is run around.  GOA locates
 * sidereal pointings by coordinates and moving targets by object name.
 */
enum ArchiveSearchPointing derives Eq:
  case Sidereal(coordinates: Coordinates)
  case NonSidereal(targetName: NonEmptyString)
