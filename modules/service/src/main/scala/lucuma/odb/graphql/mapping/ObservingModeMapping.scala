// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
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
     with GmosIfuView[F]
     with GmosMosView[F]
     with GnirsSpectroscopyView[F]
     with Flamingos2MosView[F]
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
      SqlObject("flamingos2Mos",      Join(ObservationView.Id, Flamingos2MosView.ObservationId)),
      SqlObject("ghostIfu",           Join(ObservationView.Id, GhostIfuView.ObservationId)),
      SqlObject("gmosNorthIfu",       Join(ObservationView.Id, GmosNorthIfuView.Common.ObservationId)),
      SqlObject("gmosNorthImaging",   Join(ObservationView.Id, GmosNorthImagingView.Common.ObservationId)),
      SqlObject("gmosNorthLongSlit",  Join(ObservationView.Id, GmosNorthLongSlitView.Common.ObservationId)),
      SqlObject("gmosNorthMos",       Join(ObservationView.Id, GmosNorthMosView.Common.ObservationId)),
      SqlObject("gmosSouthIfu",       Join(ObservationView.Id, GmosSouthIfuView.Common.ObservationId)),
      SqlObject("gmosSouthImaging",   Join(ObservationView.Id, GmosSouthImagingView.Common.ObservationId)),
      SqlObject("gmosSouthLongSlit",  Join(ObservationView.Id, GmosSouthLongSlitView.Common.ObservationId)),
      SqlObject("gmosSouthMos",       Join(ObservationView.Id, GmosSouthMosView.Common.ObservationId)),
      SqlObject("igrins2LongSlit",    Join(ObservationView.Id, Igrins2LongSlitView.ObservationId)),
      SqlObject("gnirsIfu",           Join(ObservationView.Id, GnirsSpectroscopyView.ObservationId)),
      SqlObject("gnirsLongSlit",      Join(ObservationView.Id, GnirsSpectroscopyView.ObservationId)),
      SqlObject("gnirsSpectroscopy",  Join(ObservationView.Id, GnirsSpectroscopyView.ObservationId)),
      SqlObject("visitor",            Join(ObservationView.Id, VisitorTable.ObservationId))

    )
}