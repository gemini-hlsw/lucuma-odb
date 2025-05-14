// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.syntax.order.*
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan

package object flamingos2:

  val FaintExposureFloor: TimeSpan  = 85.secondTimeSpan
  val MediumExposureFloor: TimeSpan = 21.secondTimeSpan

  extension (t: TimeSpan)
    def readMode: Flamingos2ReadMode =
      if t >= FaintExposureFloor then Flamingos2ReadMode.Faint
      else if t >= MediumExposureFloor then Flamingos2ReadMode.Medium
      else Flamingos2ReadMode.Bright
