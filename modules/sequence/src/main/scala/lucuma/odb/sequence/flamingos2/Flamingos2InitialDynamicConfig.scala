// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.flamingos2

import cats.syntax.option.*
import lucuma.core.enums.Flamingos2Filter.H
import lucuma.core.enums.Flamingos2ReadMode.Bright
import lucuma.core.enums.Flamingos2LyotWheel.F16
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask.Imaging
import lucuma.core.util.TimeSpan

trait Flamingos2InitialDynamicConfig:

  /**
   * Starting point, default dynamic configuration for Flamingos 2.  This will
   * serve as the initial state in state computations that produce sequence
   * steps.
   */
  val initialConfig: Flamingos2DynamicConfig =
    Flamingos2DynamicConfig(
      exposure    = TimeSpan.Min,
      disperser   = none,
      filter      = H,
      readMode    = Bright,
      lyot        = F16,
      fpu         = Imaging,
      readoutMode = none,
      reads       = none
    )