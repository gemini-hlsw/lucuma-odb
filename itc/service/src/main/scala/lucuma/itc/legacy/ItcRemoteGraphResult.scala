// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.data.NonEmptyChain
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN
import lucuma.core.math.Wavelength
import lucuma.itc.ItcGraphGroup
import lucuma.itc.ItcReadMode

case class GraphsRemoteResult(
  ccds:   NonEmptyChain[ItcRemoteCcd],
  groups: NonEmptyChain[ItcGraphGroup]
) {
  val maxTotalSNRatio: Double = ccds.map(_.totalSNRatio).maximum
  val maxWellDepth: Double    = ccds.map(_.wellDepth).maximum
  val maxPeakPixelFlux: Int   = ccds.map(_.peakPixelFlux).maximum.toInt
}

// One detector's result: the exposure time of a single exposure, the number of frames, the
// coadds per frame and, for the instruments whose recipe selects one, the read mode.
case class ExposureCalculation(
  exposureTime: Double,
  frameCount:   NonNegInt,
  coadds:       PosInt,
  readMode:     Option[ItcReadMode]
)

case class SignalToNoiseAt(
  wavelength: Wavelength,
  single:     SingleSN,
  total:      TotalSN
)

case class AllExposureCalculations(
  detectors:     NonEmptyChain[ExposureCalculation],
  selectedIndex: Int
)

case class IntegrationTimeRemoteResult(
  exposureCalculation: AllExposureCalculations,

  // In principle this should not be empty, but you could still use a wv outside the rang of a ccd. Rather than giving a zero SN, we just don't return anything.
  signalToNoiseAt: Option[SignalToNoiseAt],

  // CCD performance data from the legacy ITC
  ccds: NonEmptyChain[ItcRemoteCcd]
)

case class TimeAndGraphsRemoteResult(
  exposureCalculation: AllExposureCalculations,
  signalToNoiseAt:     Option[SignalToNoiseAt],
  ccds:                NonEmptyChain[ItcRemoteCcd],
  groups:              NonEmptyChain[ItcGraphGroup]
)
