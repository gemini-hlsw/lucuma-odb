// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping

import table.DaytimeCalibrationView

trait DaytimeCalibrationMapping[F[_]] extends DaytimeCalibrationView[F]:
  this: SkunkMapping[F] =>

  lazy val DaytimeCalibrationMapping =
    ObjectMapping(DaytimeCalibrationType)(
      SqlField("observationId", DaytimeCalibrationView.ObservationId, key = true, hidden = true),
      SqlField("mode", DaytimeCalibrationView.ObservingModeType),
      SqlField("instrument", DaytimeCalibrationView.Instrument)
    )
