// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping
import lucuma.core.enums.ObservingModeType
import lucuma.odb.syntax.observingModeType.*

import table.*

trait ObservingModeMapping[F[_]]
  extends ObservationView[F]
     with ExchangeView[F]
     with Flamingos2ImagingView[F]
     with GnirsImagingView[F]
     with Flamingos2LongSlitView[F]
     with GhostIfuView[F]
     with GmosImagingView[F]
     with GmosLongSlitView[F]
     with GnirsSpectroscopyView[F]
     with Igrins2LongSlitView[F]
     with VisitorTable[F] { this: SkunkMapping[F] =>

  lazy val ObservingModeMapping: ObjectMapping =
    ObjectMapping(ObservingModeType)(
      SqlField("synthetic_id", ObservationView.ObservingMode.SyntheticId, key = true, hidden = true),

      FieldRef[ObservingModeType]("mode").as("instrument", _.instrumentOption),
      SqlField("mode", ObservationView.ObservingMode.ObservingModeType),

      SqlObject("exchange",           Join(ObservationView.Id, ExchangeView.ObservationId)),
      SqlObject("flamingos2Imaging",  Join(ObservationView.Id, Flamingos2ImagingView.ObservationId)),
      SqlObject("gnirsImaging",       Join(ObservationView.Id, GnirsImagingView.ObservationId)),
      SqlObject("flamingos2LongSlit", Join(ObservationView.Id, Flamingos2LongSlitView.ObservationId)),
      SqlObject("ghostIfu",           Join(ObservationView.Id, GhostIfuView.ObservationId)),
      SqlObject("gmosNorthImaging",   Join(ObservationView.Id, GmosNorthImagingView.Common.ObservationId)),
      SqlObject("gmosNorthLongSlit",  Join(ObservationView.Id, GmosNorthLongSlitView.Common.ObservationId)),
      SqlObject("gmosSouthImaging",   Join(ObservationView.Id, GmosSouthImagingView.Common.ObservationId)),
      SqlObject("gmosSouthLongSlit",  Join(ObservationView.Id, GmosSouthLongSlitView.Common.ObservationId)),
      SqlObject("igrins2LongSlit",    Join(ObservationView.Id, Igrins2LongSlitView.ObservationId)),
      SqlObject("gnirsSpectroscopy", Join(ObservationView.Id, GnirsSpectroscopyView.ObservationId)),
      SqlObject("visitor",            Join(ObservationView.Id, VisitorTable.ObservationId))

    )
}