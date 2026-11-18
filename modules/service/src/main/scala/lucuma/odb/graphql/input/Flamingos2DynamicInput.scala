// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.odb.graphql.binding.*

object Flamingos2DynamicInput:

  /** @see [[GmosCustomMaskInput.OptionalWidthBinding]] */
  val OptionalWidthBinding: Matcher[(Flamingos2DynamicConfig, Boolean)] =
    ObjectFieldsBinding.rmap:
      case List(
        TimeSpanInput.Binding("exposure", rExposure),
        Flamingos2DisperserBinding.Option("disperser", rDisperser),
        Flamingos2FilterBinding("filter", rFilter),
        Flamingos2ReadModeBinding("readMode", rReadMode),
        Flamingos2LyoutWheelBinding("lyotWheel", rLyotWheel),
        Flamingos2FpuMaskInput.OptionalWidthBinding.Option("fpu", rFpu),
        Flamingos2DeckerBinding("decker", rDecker),
        Flamingos2ReadoutModeBinding("readoutMode", rReadoutMode),
        Flamingos2ReadsBinding("reads", rReads),
        Flamingos2WindowCoverBinding.Option("windowCover", rWindowCover)
      ) => (rExposure, rDisperser, rFilter, rReadMode, rLyotWheel, rFpu, rDecker, rReadoutMode, rReads, rWindowCover).parMapN: (exposure, disperser, filter, readMode, lyotWheel, fpu, decker, readoutMode, reads, windowCover) =>
        (Flamingos2DynamicConfig(
          exposure,
          disperser,
          filter,
          readMode,
          lyotWheel,
          fpu.fold(Flamingos2FpuMask.Imaging)(_._1),
          decker,
          readoutMode,
          reads,
          windowCover
        ), fpu.exists(_._2))

  val Binding: Matcher[Flamingos2DynamicConfig] =
    OptionalWidthBinding.rmap:
      case (config, false) => Result(config)
      case (_, true)       => Result.failure(GmosCustomMaskInput.SlitWidthRequiredMessage)
