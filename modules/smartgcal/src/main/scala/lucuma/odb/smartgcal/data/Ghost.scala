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
    blueExposureCount: PosInt
  )

  case class TableRow(
    key:   SearchKey,
    value: SmartGcalValue[GhostUpdate]
  )