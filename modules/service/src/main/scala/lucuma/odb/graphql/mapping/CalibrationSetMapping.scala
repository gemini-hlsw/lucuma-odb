// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.effect.Concurrent
import cats.effect.Resource
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import grackle.Cursor
import grackle.Query.EffectHandler
import grackle.Result
import grackle.skunk.SkunkMapping
import grackle.syntax.*
import io.circe.syntax.*
import lucuma.core.enums.GcalArc
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.odb.logic.DaytimeCalibrationExpansion
import lucuma.odb.logic.SmartGcalImplementation
import lucuma.odb.service.Services

import Services.Syntax.*
import table.DaytimeCalibrationSetView
import table.Flamingos2DynamicTables
import table.GmosDynamicTables

trait CalibrationSetMapping[F[_]: Concurrent] extends DaytimeCalibrationSetView[F]
                                                 with Flamingos2DynamicTables[F]
                                                 with GmosDynamicTables[F]
                                                 with KeyValueEffectHandler[F]:
  this: SkunkMapping[F] =>

  def services(using User): Resource[F, Services[F]]

  lazy val CalibrationSetMapping: ObjectMapping =
    ObjectMapping(CalibrationSetType)(
      SqlField("id",            DaytimeCalibrationSetView.Id, key = true, hidden = true),
      SqlField("observationId", DaytimeCalibrationSetView.ObservationId, hidden = true),
      SqlField("position",      DaytimeCalibrationSetView.Position, hidden = true),
      SqlField("kind",          DaytimeCalibrationSetView.Kind),
      SqlField("count",         DaytimeCalibrationSetView.Count),
      EffectField("stepCount",  stepCountHandler, List("observationId", "position")),
      SqlObject("gcal"),
      SqlObject("gmosNorth", Join(DaytimeCalibrationSetView.Id, GmosNorthCalibrationSetupTable.Id)),
      SqlObject("gmosSouth", Join(DaytimeCalibrationSetView.Id, GmosSouthCalibrationSetupTable.Id)),
      SqlObject("flamingos2", Join(DaytimeCalibrationSetView.Id, Flamingos2CalibrationSetupTable.Id))
    )

  // A manual set's step count is its count; a smart set's is what the Smart
  // GCAL lookup returns, resolved per observation so one lookup pass serves
  // every row of it.
  private lazy val stepCountHandler: EffectHandler[F] =
    keyedBatchEffectHandler[(Observation.Id, Int), Option[PosInt]](
      c => (c.fieldAs[Observation.Id]("observationId"), c.fieldAs[Int]("position")).tupled,
      (keys, _) =>
        services.useTransactionally:
          val exp = SmartGcalImplementation.fromService(smartGcalService)
          daytimeCalibrationService.select(keys.map(_._1).distinct).flatMap: cfgs =>
            cfgs.toList.traverse: (oid, cfg) =>
              DaytimeCalibrationExpansion.setCounts(exp, oid, cfg).map: counts =>
                counts.zipWithIndex.map((n, i) => (oid, i) -> n.success)
            .map(_.flatten.toMap),
      (_, n) => n.map(_.value).asJson.success
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
