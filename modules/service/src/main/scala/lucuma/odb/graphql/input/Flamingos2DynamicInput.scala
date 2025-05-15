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
        Flamingos2FpuMaskInput.Binding.Option("fpu", rFpu),
        Flamingos2DeckerBinding("decker", rDecker),
        Flamingos2ReadoutModeBinding("readoutMode", rReadoutMode),
        Flamingos2ReadsBinding("reads", rReads)
      ) => (rExposure, rDisperser, rFilter, rReadMode, rLyotWheel, rFpu, rDecker, rReadoutMode, rReads).parMapN: (exposure, disperser, filter, readMode, lyotWheel, fpu, decker, readoutMode, reads) =>
        Flamingos2DynamicConfig(
          exposure,
          disperser,
          filter,
          readMode,
          lyotWheel,
          fpu.getOrElse(Flamingos2FpuMask.Imaging),
          decker,
          readoutMode,
          reads
        )