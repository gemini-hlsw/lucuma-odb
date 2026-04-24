// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal.data

import cats.Eq
import cats.derived.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.ghost.GhostStaticConfig
import lucuma.core.util.TimeSpan

//Resolution Mode,Binning,Calibration Observe,Calibration Filter,Calibration Diffuser,Calibration Lamps,Calibration Shutter,Calibration Red Exposure Time,Calibration Red Count,Calibration Blue Exposure Time,Calibration Blue Count,Calibration Basecal,IFU1X,IFU1Y,IFU2X,IFU2Y,Slitviewer_exptime,Slitviewer binning,No. of slit viewer exposures
//,,,,,,,,,,,,,,,,
//Standard Resolution,1x1,1,GMOS balance,Visible,Quartz Halogen 100W,Closed,6.0s,5,6.0s,5,Day,-68.5,0,68.5,0,0.2s,2x2,150

object Ghost:

  case class SearchKey(
    resolutionMode: GhostResolutionMode,
    redBinning:     GhostBinning,
    blueBinning:    GhostBinning
  ) derives Eq:
    def format: String =
      val r  = s"resolutionMode: $resolutionMode"
      val rb = s"redBinning: $redBinning"
      val bb = s"blueBinning: $blueBinning"
      s"Ghost { $r, red: $rb, blue: $bb }"

  object SearchKey:

    def forConfig(
      s: GhostStaticConfig,
      d: GhostDynamicConfig
    ): SearchKey =
      SearchKey(s.resolutionMode, d.red.value.binning, d.blue.value.binning)

  case class GhostUpdate(
    redExposureTime:   TimeSpan,
    redExposureCount:  PosInt,
    blueExposureTime:  TimeSpan,
    blueExposureCount: PosInt,
    slitExposureTime:  TimeSpan
  )

  case class TableRow(
    key:   SearchKey,
    value: SmartGcalValue[GhostUpdate]
  )