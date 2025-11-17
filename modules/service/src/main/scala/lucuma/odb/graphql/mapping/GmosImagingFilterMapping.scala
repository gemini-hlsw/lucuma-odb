// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.*

trait GmosImagingFilterMapping[F[_]] extends GmosImagingFilterTable[F]
                                        with ExposureTimeModeView[F]:
  this: SkunkMapping[F] =>

    lazy val GmosNorthImagingFilterMapping: ObjectMapping =
      ObjectMapping(GmosNorthImagingFilterType)(
        SqlField("observationId",     GmosNorthImagingFilterTable.ObservationId, key = true, hidden = true),
        SqlField("filter",            GmosNorthImagingFilterTable.Filter, key = true),
        SqlField("version",           GmosNorthImagingFilterTable.Version, key = true, hidden = true),
        SqlObject("exposureTimeMode", Join(GmosNorthImagingFilterTable.ExposureTimeModeId, ExposureTimeModeView.Id))
      )

    lazy val GmosSouthImagingFilterMapping: ObjectMapping =
      ObjectMapping(GmosSouthImagingFilterType)(
        SqlField("observationId",     GmosSouthImagingFilterTable.ObservationId, key = true, hidden = true),
        SqlField("filter",            GmosSouthImagingFilterTable.Filter, key = true),
        SqlField("version",           GmosSouthImagingFilterTable.Version, key = true, hidden = true),
        SqlObject("exposureTimeMode", Join(GmosSouthImagingFilterTable.ExposureTimeModeId, ExposureTimeModeView.Id))
      )