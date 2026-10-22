// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.Query.Binding
import grackle.Query.OrderBy
import grackle.Query.OrderSelection
import grackle.Query.OrderSelections
import grackle.QueryCompiler.Elab
import grackle.TypeRef
import grackle.skunk.SkunkMapping

import table.DaytimeCalibrationSetView
import table.DaytimeCalibrationView

trait DaytimeCalibrationMapping[F[_]] extends DaytimeCalibrationView[F]
                                         with DaytimeCalibrationSetView[F]:
  this: SkunkMapping[F] =>

  lazy val DaytimeCalibrationMapping =
    ObjectMapping(DaytimeCalibrationType)(
      SqlField("observationId", DaytimeCalibrationView.ObservationId, key = true, hidden = true),
      SqlField("mode", DaytimeCalibrationView.ObservingModeType),
      SqlField("instrument", DaytimeCalibrationView.Instrument),
      SqlObject("calibrationSets", Join(DaytimeCalibrationView.ObservationId, DaytimeCalibrationSetView.ObservationId))
    )

  lazy val DaytimeCalibrationElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (DaytimeCalibrationType, "calibrationSets", Nil) =>
      Elab.transformChild: child =>
        OrderBy(OrderSelections(List(OrderSelection[Int](CalibrationSetType / "position"))), child)
