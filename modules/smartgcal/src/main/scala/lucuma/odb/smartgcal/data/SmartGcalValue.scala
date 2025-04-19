// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal.data

import cats.Eq
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GcalBaselineType
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.core.util.TimeSpan
import monocle.Focus
import monocle.Lens

case class SmartGcalValue[A](
  gcalConfig:       Gcal,
  baselineType:     GcalBaselineType,
  stepCount:        PosInt,
  instrumentConfig: A
)

object SmartGcalValue {

  def gcalConfig[A]: Lens[SmartGcalValue[A], Gcal] =
    Focus[SmartGcalValue[A]](_.gcalConfig)

  def baselineType[A]: Lens[SmartGcalValue[A], GcalBaselineType] =
    Focus[SmartGcalValue[A]](_.baselineType)

  def stepCount[A]: Lens[SmartGcalValue[A], PosInt] =
    Focus[SmartGcalValue[A]](_.stepCount)

  def instrumentConfig[A]: Lens[SmartGcalValue[A], A] =
    Focus[SmartGcalValue[A]](_.instrumentConfig)

  case class LegacyInstrumentConfig(
    exposureTime: TimeSpan,
    coadds:       PosInt   = PosInt.unsafeFrom(1)
  )

  object LegacyInstrumentConfig {

    val exposureTime: Lens[LegacyInstrumentConfig, TimeSpan] =
      Focus[LegacyInstrumentConfig](_.exposureTime)

    val coadds: Lens[LegacyInstrumentConfig, PosInt] =
      Focus[LegacyInstrumentConfig](_.coadds)

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

