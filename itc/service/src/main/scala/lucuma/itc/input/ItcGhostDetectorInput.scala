// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostReadMode
import lucuma.itc.ItcGhostDetector
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.ExposureTimeModeInput

object ItcGhostDetectorInput:
  val Binding: Matcher[ItcGhostDetector] =
    ObjectFieldsBinding.rmap {
      case List(
            ExposureTimeModeInput.TimeAndCount.Binding("timeAndCount", timeAndCount),
            GhostReadModeBinding("readMode", readMode),
            GhostBinningBinding("binning", binning)
          ) =>
        (timeAndCount, readMode, binning).parMapN(ItcGhostDetector.apply)
    }
