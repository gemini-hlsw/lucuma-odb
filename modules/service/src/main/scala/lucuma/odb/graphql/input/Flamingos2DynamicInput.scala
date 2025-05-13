// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.odb.graphql.binding.*

object Flamingos2DynamicInput:

  val Binding: Matcher[Flamingos2DynamicConfig] =
    ObjectFieldsBinding.rmap:
      case List(
        TimeSpanInput.Binding("exposure", rExposure),
        Flamingos2DisperserBinding.Option("disperser", rDisperser),
        Flamingos2FilterBinding("filter", rFilter),
        Flamingos2ReadModeBinding("readMode", rReadMode),
        Flamingos2LyoutWheelBinding("lyotWheel", rLyotWheel),
        Flamingos2FpuMaskInput.Binding.Option("mask", rMask),
        Flamingos2ReadoutModeBinding.Option("readoutMode", rReadoutMode),
        Flamingos2ReadsBinding.Option("reads", rReads)
      ) => (rExposure, rDisperser, rFilter, rReadMode, rLyotWheel, rMask, rReadoutMode, rReads).parMapN: (exposure, disperser, filter, readMode, lyotWheel, mask, readoutMode, reads) =>
        Flamingos2DynamicConfig(
          exposure,
          disperser,
          filter,
          readMode,
          lyotWheel,
          mask.getOrElse(Flamingos2FpuMask.Imaging),
          readoutMode,
          reads
        )