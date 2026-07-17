// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.syntax.all.*
import grackle.Query.Binding
import grackle.Query.Filter
import grackle.Query.Unique
import grackle.QueryCompiler.Elab
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.Flamingos2Decker
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2ReadoutMode
import lucuma.core.enums.SlitOffsetMode
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.format.telescopeConfigs.*
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.*
import lucuma.odb.json.offset.query.given

trait Flamingos2LongSlitMapping[F[_]]
  extends Flamingos2LongSlitView[F]
     with ExposureTimeModeMapping[F]
     with OptionalFieldMapping[F]
     with Predicates[F] { this: SkunkMapping[F] =>

  private def telescopeConfigJson(offset: Offset, guiding: StepGuideState): Json =
    Json.obj(
      "offset"  -> Json.obj("p" -> offset.p.asJson, "q" -> offset.q.asJson),
      "guiding" -> guiding.asJson
    )

  private def telescopeConfigAlongSlitJson(q: Offset.Q, guiding: StepGuideState): Json =
    Json.obj(
      "q"       -> q.asJson,
      "guiding" -> guiding.asJson
    )

  // Encodes a SlitTelescopeConfigs as the GraphQL SlitTelescopeConfigs shape:
  // { offsetMode, alongSlit, toSky } with exactly one of alongSlit / toSky populated.
  private def slitTelescopeConfigsJson(mode: SlitOffsetMode, json: String): Json =
    SlitTelescopeConfigsFormat.getOption((mode, json)).fold(Json.Null):
      case SlitTelescopeConfigs.AlongSlit(nel) =>
        Json.obj(
          "offsetMode" -> mode.asJson,
          "alongSlit"  -> nel.toList.map(c => telescopeConfigAlongSlitJson(c.offset, c.guiding)).asJson,
          "toSky"      -> Json.Null
        )
      case SlitTelescopeConfigs.ToSky(nel) =>
        Json.obj(
          "offsetMode" -> mode.asJson,
          "alongSlit"  -> Json.Null,
          "toSky"      -> nel.toList.map(tc => telescopeConfigJson(tc.offset, tc.guiding)).asJson
        )

  lazy val Flamingos2LongSlitAcquisitionMapping: ObjectMapping =
    ObjectMapping(Flamingos2LongSlitAcquisitionType)(
      SqlField("observationId", Flamingos2LongSlitView.ObservationId, key = true, hidden = true),

      explicitOrElseDefault[Flamingos2Filter]("filter", "explicitFilter", "defaultFilter"),
      SqlField("defaultFilter",  Flamingos2LongSlitView.AcquisitionFilterDefault),
      SqlField("explicitFilter", Flamingos2LongSlitView.AcquisitionFilter),

      SqlObject("exposureTimeMode", Join(Flamingos2LongSlitView.ObservationId, ExposureTimeModeView.ObservationId))
    )

  lazy val Flamingos2LongSlitMapping: ObjectMapping =
    ObjectMapping(Flamingos2LongSlitType)(

      SqlField("observationId", Flamingos2LongSlitView.ObservationId, key = true, hidden = true),

      SqlField("disperser", Flamingos2LongSlitView.Disperser),
      SqlField("filter",    Flamingos2LongSlitView.Filter),
      SqlField("fpu",       Flamingos2LongSlitView.Fpu),

      SqlObject("exposureTimeMode", Join(Flamingos2LongSlitView.ObservationId, ExposureTimeModeView.ObservationId)),

      SqlField("explicitReadMode", Flamingos2LongSlitView.ReadMode),
      SqlField("explicitReads", Flamingos2LongSlitView.Reads),

      explicitOrElseDefault[Flamingos2Decker]("decker", "explicitDecker", "defaultDecker"),
      SqlField("defaultDecker",  Flamingos2LongSlitView.DeckerDefault),
      SqlField("explicitDecker", Flamingos2LongSlitView.Decker),

      explicitOrElseDefault[Flamingos2ReadoutMode]("readoutMode", "explicitReadoutMode", "defaultReadoutMode"),
      SqlField("defaultReadoutMode",  Flamingos2LongSlitView.ReadoutModeDefault),
      SqlField("explicitReadoutMode", Flamingos2LongSlitView.ReadoutMode),

      // Raw columns (hidden) backing the telescope config cursor fields.
      SqlField("slitOffsetModeEffRaw", Flamingos2LongSlitView.SlitOffsetModeEffective,  hidden = true),
      SqlField("tcEffRaw",             Flamingos2LongSlitView.TelescopeConfigsEffective, hidden = true),
      SqlField("slitOffsetModeDefRaw", Flamingos2LongSlitView.SlitOffsetModeDefault,     hidden = true),
      SqlField("tcDefRaw",             Flamingos2LongSlitView.TelescopeConfigsDefault,   hidden = true),
      SqlField("slitOffsetModeExpRaw", Flamingos2LongSlitView.SlitOffsetMode,            hidden = true),
      SqlField("tcExpRaw",             Flamingos2LongSlitView.TelescopeConfigs,          hidden = true),

      CursorFieldJson("telescopeConfigs",
        cursor =>
          for
            modeEff <- cursor.field("slitOffsetModeEffRaw", None).flatMap(_.as[SlitOffsetMode])
            tcEff   <- cursor.field("tcEffRaw", None).flatMap(_.as[String])
          yield slitTelescopeConfigsJson(modeEff, tcEff),
        List("slitOffsetModeEffRaw", "tcEffRaw")
      ),
      CursorFieldJson("defaultTelescopeConfigs",
        cursor =>
          for
            modeDef <- cursor.field("slitOffsetModeDefRaw", None).flatMap(_.as[SlitOffsetMode])
            tcDef   <- cursor.field("tcDefRaw", None).flatMap(_.as[String])
          yield slitTelescopeConfigsJson(modeDef, tcDef),
        List("slitOffsetModeDefRaw", "tcDefRaw")
      ),
      CursorFieldJson("explicitTelescopeConfigs",
        cursor =>
          for
            modeOpt <- cursor.field("slitOffsetModeExpRaw", None).flatMap(_.as[Option[SlitOffsetMode]])
            jsonOpt <- cursor.field("tcExpRaw", None).flatMap(_.as[Option[String]])
          yield (modeOpt, jsonOpt).mapN(slitTelescopeConfigsJson).getOrElse(Json.Null),
        List("slitOffsetModeExpRaw", "tcExpRaw")
      ),

      SqlJson("telluricType", Flamingos2LongSlitView.TelluricType),

      SqlObject("acquisition"),

      SqlField("initialDisperser", Flamingos2LongSlitView.InitialDisperser),
      SqlField("initialFilter",    Flamingos2LongSlitView.InitialFilter),
      SqlField("initialFpu",       Flamingos2LongSlitView.InitialFpu),

    )

  lazy val Flamingos2LongSlitElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (Flamingos2LongSlitAcquisitionType, "exposureTimeMode", Nil) =>
      Elab.transformChild: child =>
        Unique(
          Filter(
            Predicates.exposureTimeMode.role.eql(ExposureTimeModeRole.Acquisition),
            child
          )
        )

    case (Flamingos2LongSlitType, "exposureTimeMode", Nil) =>
      Elab.transformChild: child =>
        Unique(
          Filter(
            Predicates.exposureTimeMode.role.eql(ExposureTimeModeRole.Science),
            child
          )
        )

}
