// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.model.sequence.ghost.GhostDetector
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.odb.graphql.binding.*

object GhostDetectorInput:

  val Binding: Matcher[GhostDetector] =
    ObjectFieldsBinding.rmap:
      case List(
        TimeSpanInput.Binding("exposureTime", rExposureTime),
        PosIntBinding("exposureCount", rExposureCount),
        GhostBinningBinding("binning", rBinning),
        GhostReadModeBinding("readMode", rReadMode)
      ) => (rExposureTime, rExposureCount, rBinning, rReadMode).parMapN:
        (exposureTime, exposureCount, binning, readMode) =>
          GhostDetector(exposureTime, exposureCount, binning, readMode)

object GhostDynamicInput:

  val Binding: Matcher[GhostDynamicConfig] =
    ObjectFieldsBinding.rmap:
      case List(
        GhostDetectorInput.Binding("red", rRed),
        GhostDetectorInput.Binding("blue", rBlue),
        GhostIfu1FiberAgitatorBinding("ifu1FiberAgitator", rIfu1),
        GhostIfu2FiberAgitatorBinding("ifu2FiberAgitator", rIfu2)
      ) => (rRed, rBlue, rIfu1, rIfu2).parMapN: (red, blue, ifu1, ifu2) =>
        GhostDynamicConfig(
          GhostDetector.Red(red),
          GhostDetector.Blue(blue),
          ifu1,
          ifu2
        )
