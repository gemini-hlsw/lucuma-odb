// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.syntax.functorFilter.*
import grackle.Cursor
import grackle.Result
import grackle.skunk.SkunkMapping
import lucuma.core.enums.GcalArc

import table.DaytimeCalibrationSetView
import table.GmosDynamicTables

trait CalibrationSetMapping[F[_]] extends DaytimeCalibrationSetView[F]
                                     with GmosDynamicTables[F]:
  this: SkunkMapping[F] =>

  lazy val CalibrationSetMapping: ObjectMapping =
    ObjectMapping(CalibrationSetType)(
      SqlField("id",        DaytimeCalibrationSetView.Id, key = true, hidden = true),
      SqlField("position",  DaytimeCalibrationSetView.Position, hidden = true),
      SqlField("kind",      DaytimeCalibrationSetView.Kind),
      SqlField("count",     DaytimeCalibrationSetView.Count),
      SqlObject("gcal"),
      SqlObject("gmosNorth", Join(DaytimeCalibrationSetView.Id, GmosNorthCalibrationSetupTable.Id)),
      SqlObject("gmosSouth", Join(DaytimeCalibrationSetView.Id, GmosSouthCalibrationSetupTable.Id))
    )

  lazy val CalibrationSetGcalMapping: ObjectMapping =
    def arc(c: Cursor, n: String, a: GcalArc): Result[Option[GcalArc]] =
      c.fieldAs[Boolean](n).map(b => Option.when(b)(a))

    ObjectMapping(CalibrationSetType / "gcal")(
      SqlField("id",        DaytimeCalibrationSetView.Gcal.SyntheticId, key = true, hidden = true),
      SqlField("continuum", DaytimeCalibrationSetView.Gcal.Continuum),
      SqlField("arArc",     DaytimeCalibrationSetView.Gcal.ArArc,   hidden = true),
      SqlField("cuarArc",   DaytimeCalibrationSetView.Gcal.CuarArc, hidden = true),
      SqlField("tharArc",   DaytimeCalibrationSetView.Gcal.TharArc, hidden = true),
      SqlField("xeArc",     DaytimeCalibrationSetView.Gcal.XeArc,   hidden = true),
      CursorField(
        "arcs",
        cursor =>
          for
            ar   <- arc(cursor, "arArc",   GcalArc.ArArc)
            cuar <- arc(cursor, "cuarArc", GcalArc.CuArArc)
            thar <- arc(cursor, "tharArc", GcalArc.ThArArc)
            xe   <- arc(cursor, "xeArc",   GcalArc.XeArc)
          yield List(ar, cuar, thar, xe).flattenOption,
        List("arArc", "cuarArc", "tharArc", "xeArc")
      ),
      SqlField("filter",    DaytimeCalibrationSetView.Gcal.Filter),
      SqlField("diffuser",  DaytimeCalibrationSetView.Gcal.Diffuser),
      SqlField("shutter",   DaytimeCalibrationSetView.Gcal.Shutter)
    )
