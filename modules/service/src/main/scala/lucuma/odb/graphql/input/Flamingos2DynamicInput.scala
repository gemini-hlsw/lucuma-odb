// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.model.sequence.f2.F2DynamicConfig
import lucuma.core.model.sequence.f2.F2FpuMask
import lucuma.odb.graphql.binding.*

object Flamingos2DynamicInput:

  val Binding: Matcher[F2DynamicConfig] =
    ObjectFieldsBinding.rmap:
      case List(
        TimeSpanInput.Binding("exposure", rExposure),
        F2DisperserBinding.Option("disperser", rDisperser),
        F2FilterBinding("filter", rFilter),
        F2ReadModeBinding("readMode", rReadMode),
        F2LyoutWheelBinding("lyotWheel", rLyotWheel),
        Flamingos2FpuMaskInput.Binding.Option("mask", rMask),
        F2ReadoutModeBinding.Option("readoutMode", rReadoutMode),
        F2ReadsBinding.Option("reads", rReads)
      ) => (rExposure, rDisperser, rFilter, rReadMode, rLyotWheel, rMask, rReadoutMode, rReads).parMapN: (exposure, disperser, filter, readMode, lyotWheel, mask, readoutMode, reads) =>
        F2DynamicConfig(
          exposure,
          disperser,
          filter,
          readMode,
          lyotWheel,
          mask.getOrElse(F2FpuMask.Imaging),
          readoutMode,
          reads
        )