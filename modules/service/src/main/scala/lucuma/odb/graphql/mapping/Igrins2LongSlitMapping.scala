// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Cursor
import grackle.Query.Binding
import grackle.Query.Filter
import grackle.Query.Unique
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.sequence.igrins2.*
import lucuma.core.util.TimeSpan
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.*
import lucuma.odb.json.time.query.given

trait Igrins2LongSlitMapping[F[_]]
  extends Igrins2LongSlitView[F]
     with ExposureTimeModeMapping[F]
     with SlitTelescopeConfigsMapping[F]
     with Predicates[F] { this: SkunkMapping[F] =>

  lazy val Igrins2LongSlitMapping: ObjectMapping =
    ObjectMapping(Igrins2LongSlitType)(
      SqlField("observationId", Igrins2LongSlitView.ObservationId, key = true, hidden = true),

      SqlObject("exposureTimeMode", Join(Igrins2LongSlitView.ObservationId, ExposureTimeModeView.ObservationId)),

      // Hidden raw columns backing the SVC sub-config (served as a single JSON object below).
      SqlField("saveSvcImagesRaw",               Igrins2LongSlitView.SaveSVCImages, hidden = true),
      SqlField("explicitSvcExposureRaw",         Igrins2LongSlitView.SvcExposure, hidden = true),
      SqlField("explicitSvcTelescopeConfigsRaw", Igrins2LongSlitView.SvcTelescopeConfigs, hidden = true),

      // SVC (Slit-Viewing Camera) acquisition sub-config.
      // Resolves to null when SVC images are not saved, otherwise a JSON object carrying
      // the exposure and telescope dither positions.
      // Served as a single JSON value so the whole object is null when SVC is off.
      CursorFieldJson(
        "svc",
        svcConfigJson,
        List("saveSvcImagesRaw", "explicitSvcExposureRaw", "explicitSvcTelescopeConfigsRaw")
      ),

      SqlField("slitOffsetModeEffRaw", Igrins2LongSlitView.SlitOffsetModeEffective, hidden = true),
      SqlField("tcEffRaw",             Igrins2LongSlitView.TelescopeConfigsEffective, hidden = true),
      SqlField("slitOffsetModeDefRaw", Igrins2LongSlitView.SlitOffsetModeDefault,     hidden = true),
      SqlField("tcDefRaw",             Igrins2LongSlitView.TelescopeConfigsDefault,   hidden = true),
      SqlField("slitOffsetModeExpRaw", Igrins2LongSlitView.SlitOffsetMode,            hidden = true),
      SqlField("tcExpRaw",             Igrins2LongSlitView.TelescopeConfigs,          hidden = true),

      slitTelescopeConfigsField("telescopeConfigs",        "slitOffsetModeEffRaw", "tcEffRaw"),
      slitTelescopeConfigsField("defaultTelescopeConfigs", "slitOffsetModeDefRaw", "tcDefRaw"),
      explicitSlitTelescopeConfigsField("explicitTelescopeConfigs", "slitOffsetModeExpRaw", "tcExpRaw"),

      SqlJson("telluricType", Igrins2LongSlitView.TelluricType)

    )

  /** Builds the `svc` GraphQL object as JSON, or `Json.Null` when SVC images are not saved. */
  private def svcConfigJson(cursor: Cursor): Result[Json] =
    for
      save     <- cursor.field("saveSvcImagesRaw", None).flatMap(_.as[Boolean])
      exposure <- svcExplicitExposure(cursor)
      tcsRaw   <- svcExplicitTelescopeConfigs(cursor)
      tcsJson  <- tcsRaw.fold[Result[Json]](Result(Json.Null))(ifuTelescopeConfigsJson)
    yield
      if save then
        Json.obj(
          "exposure"                 -> exposure.fold(timeSpanJson(SvcDefaultExposure))(timeSpanJson),
          "defaultExposure"          -> timeSpanJson(SvcDefaultExposure),
          "explicitExposure"         -> exposure.fold(Json.Null)(timeSpanJson),
          "telescopeConfigs"         -> tcsRaw.fold(telescopeConfigsJson(SvcDefaultTelescopeConfigs))(_ => tcsJson),
          "defaultTelescopeConfigs"  -> telescopeConfigsJson(SvcDefaultTelescopeConfigs),
          "explicitTelescopeConfigs" -> tcsJson
        )
      else Json.Null

  private def svcExplicitExposure(cursor: Cursor): Result[Option[TimeSpan]] =
    cursor.field("explicitSvcExposureRaw", None).flatMap(_.as[Option[TimeSpan]])

  private def svcExplicitTelescopeConfigs(cursor: Cursor): Result[Option[String]] =
    cursor.field("explicitSvcTelescopeConfigsRaw", None).flatMap(_.as[Option[String]])

  private def timeSpanJson(ts: TimeSpan): Json = ts.asJson

  lazy val Igrins2LongSlitElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (Igrins2LongSlitType, "exposureTimeMode", Nil) =>
      Elab.transformChild: child =>
        Unique(
          Filter(
            Predicates.exposureTimeMode.role.eql(ExposureTimeModeRole.Science),
            child
          )
        )

}
