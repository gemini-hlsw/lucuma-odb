// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.syntax

import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.TimeSpan
import lucuma.odb.data.ExposureTimeModeType

trait ToExposureTimeModeOps:

  extension (etm: ExposureTimeMode)
    def modeType: ExposureTimeModeType =
      etm match
        case ExposureTimeMode.SignalToNoiseMode(_, _)   => ExposureTimeModeType.SignalToNoiseMode
        case ExposureTimeMode.TimeAndCountMode(_, _, _) => ExposureTimeModeType.TimeAndCountMode

    def signalToNoise: Option[SignalToNoise] =
      ExposureTimeMode
        .signalToNoise
        .andThen(ExposureTimeMode.SignalToNoiseMode.value)
        .getOption(etm)

    def exposureTime: Option[TimeSpan] =
      ExposureTimeMode
        .timeAndCount
        .andThen(ExposureTimeMode.TimeAndCountMode.time)
        .getOption(etm)

    def exposureCount: Option[PosInt] =
      ExposureTimeMode
        .timeAndCount
        .andThen(ExposureTimeMode.TimeAndCountMode.count)
        .getOption(etm)

  extension ($: ExposureTimeMode.type)
    def forAcquisition(at: Wavelength): ExposureTimeMode =
      ExposureTimeMode.SignalToNoiseMode(
        SignalToNoise.FromBigDecimalExact.getOption(10).get,
        at
      )

object exposureTimeMode extends ToExposureTimeModeOps