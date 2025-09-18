// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.syntax

import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.SignalToNoise
import spire.implicits.*

import java.math.MathContext

extension (signalToNoise: SignalToNoise)
  def stepSignalToNoise(exposureCount: PosInt): Option[SignalToNoise] =
    SignalToNoise.FromBigDecimalRounding.getOption(
      BigDecimal(
        (signalToNoise.toBigDecimal * signalToNoise.toBigDecimal / exposureCount.value)
          .underlying()
      )
        .sqrt(MathContext.DECIMAL128)
    )
