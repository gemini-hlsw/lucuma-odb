// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal.data

import cats.Eq
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GcalBaselineType
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.core.util.TimeSpan

case class SmartGcalValue[A](
  gcalConfig:       Gcal,
  baselineType:     GcalBaselineType,
  stepCount:        PosInt,
  instrumentConfig: A
)

object SmartGcalValue {

  case class LegacyInstrumentConfig(
    exposureTime: TimeSpan,
    coadds:       PosInt   = PosInt.unsafeFrom(1)
  )

  object LegacyInstrumentConfig {

    given Eq[LegacyInstrumentConfig] =
      Eq.by { a => (
        a.exposureTime,
        a.coadds.value
      )}

  }

  type Legacy = SmartGcalValue[LegacyInstrumentConfig]

  given [A](using Eq[A]): Eq[SmartGcalValue[A]] =
    Eq.by { a => (
      a.gcalConfig,
      a.baselineType,
      a.stepCount.value,
      a.instrumentConfig
    )}

}

