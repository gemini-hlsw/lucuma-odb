// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

import lucuma.core.math.SignalToNoise

val SignalToNoiseBinding: Matcher[SignalToNoise] =
  BigDecimalBinding.emap { bd =>
    SignalToNoise.FromBigDecimalExact
      .getOption(bd)
      .toRight(
        s"Signal-to-noise out of range [${SignalToNoise.Min.toBigDecimal}, ${SignalToNoise.Max.toBigDecimal}] with scale 3: $bd"
      )
  }
