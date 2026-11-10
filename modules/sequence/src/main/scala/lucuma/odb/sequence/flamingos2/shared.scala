// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.flamingos2

import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.MosPreImaging
import lucuma.core.math.SignalToNoise
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime

// Definitions that are shared across F2 modes.
/**
 * The signal-to-noise at which a target is classified as a Bright or Faint
 * acquisition: Bright when it is reached within
 * `Flamingos2AcquisitionMode.BrightExposureLimit`.
 */
val AcquisitionClassificationSignalToNoise: SignalToNoise =
  SignalToNoise.unsafeFromBigDecimalExact(BigDecimal(10))

val MinAcquisitionExposureTime: TimeSpan = 2.secondTimeSpan
val MaxAcquisitionExposureTime: TimeSpan = TimeSpan.Max  // TBD

/**
 * The read mode the ITC computed a result with. Every Flamingos 2 result carries one: the
 * ITC reports it, and the results frozen before it did were backfilled (V1357).
 */
def itcReadMode(time: IntegrationTime): Either[String, Flamingos2ReadMode] =
  time.flamingos2ReadMode.toRight("the ITC result carries no Flamingos 2 read mode")

/** Every Flamingos 2 mode shares the same static configuration. */
val Static: Flamingos2StaticConfig =
  Flamingos2StaticConfig(
    mosPreImaging           = MosPreImaging.IsNotMosPreImaging,
    useElectronicOffsetting = false
  )
