// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb

import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension

// Coordinates for test fixtures: from an "HH:MM:SS +DD:MM:SS" literal, or from decimal
// degrees for values that are computed or read back out of a response.
object TestCoordinates:

  def coords(hmsDms: String): Coordinates =
    Coordinates.fromHmsDms.unsafeGet(hmsDms)

  def coords(raDeg: Double, decDeg: Double): Coordinates =
    Coordinates(
      RightAscension.fromDoubleDegrees(raDeg),
      Declination.fromDoubleDegrees(decDeg).getOrElse(sys.error(s"Invalid declination: $decDeg"))
    )
