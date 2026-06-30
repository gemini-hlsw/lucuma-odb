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
import lucuma.core.enums.SlitOffsetMode
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.format.telescopeConfigs.*
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.*
import lucuma.odb.json.offset.query.given

trait GnirsSpectroscopyMapping[F[_]]
  extends GnirsSpectroscopyView[F]
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

  // Encodes a SlitTelescopeConfigs as the JSON shape expected for the GraphQL
  // SlitTelescopeConfigs type: { offsetMode, alongSlit, toSky } where exactly
  // one of alongSlit / toSky is non-null depending on the discriminant.
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

  lazy val GnirsSpectroscopyAcquisitionMapping: ObjectMapping =
    ObjectMapping(GnirsSpectroscopyAcquisitionType)(

      SqlField("observationId", GnirsSpectroscopyView.ObservationId, key = true, hidden = true),

      SqlField("explicitAcquisitionType", GnirsSpectroscopyView.AcqType),
      SqlField("coadds",    GnirsSpectroscopyView.AcqCoadds),

      // Acquisition filter: explicit override only. The effective/default filter is
      // determined in code (GnirsAcquisitionMode) at sequence-generation time.
      SqlField("explicitFilter", GnirsSpectroscopyView.AcqFilter),

      SqlField("acqSkyOffPRaw", GnirsSpectroscopyView.AcqSkyOffsetP, hidden = true),
      SqlField("acqSkyOffQRaw", GnirsSpectroscopyView.AcqSkyOffsetQ, hidden = true),

      CursorFieldJson("skyOffset",
        cursor =>
          for
            p <- cursor.field("acqSkyOffPRaw", None).flatMap(_.as[Option[Angle]])
            q <- cursor.field("acqSkyOffQRaw", None).flatMap(_.as[Option[Angle]])
          yield (p, q) match
            case (Some(pa), Some(qa)) =>
              Offset(Offset.P(pa), Offset.Q(qa)).asJson
            case _ => Json.Null,
        List("acqSkyOffPRaw", "acqSkyOffQRaw")
      ),

      SqlObject("exposureTimeMode", Join(GnirsSpectroscopyView.ObservationId, ExposureTimeModeView.ObservationId)),
    )

  lazy val GnirsSpectroscopyMapping: ObjectMapping =
    ObjectMapping(GnirsSpectroscopyType)(

      SqlField("observationId", GnirsSpectroscopyView.ObservationId, key = true, hidden = true),

      SqlObject("exposureTimeMode", Join(GnirsSpectroscopyView.ObservationId, ExposureTimeModeView.ObservationId)),

      // Grating: effective = COALESCE(explicit, initial)
      SqlField("grating",        GnirsSpectroscopyView.GratingEffective),
      SqlField("explicitGrating", GnirsSpectroscopyView.Grating),
      SqlField("initialGrating", GnirsSpectroscopyView.InitialGrating),

      // Prism: effective = COALESCE(explicit, initial)
      SqlField("prism",          GnirsSpectroscopyView.PrismEffective),
      SqlField("explicitPrism",  GnirsSpectroscopyView.Prism),
      SqlField("initialPrism",   GnirsSpectroscopyView.InitialPrism),

      // Central wavelength: required stored value + initial snapshot
      SqlObject("centralWavelength"),
      SqlObject("initialCentralWavelength"),

      // Camera + FPU + Filter + Wavelength
      SqlField("camera",        GnirsSpectroscopyView.Camera),
      SqlField("initialCamera", GnirsSpectroscopyView.InitialCamera),
      // FPU: exactly one of fpuSlit / fpuIfu is non-null (+ initial snapshots).
      SqlField("fpuSlit",        GnirsSpectroscopyView.FpuSlit),
      SqlField("fpuIfu",         GnirsSpectroscopyView.FpuIfu),
      SqlField("initialFpuSlit", GnirsSpectroscopyView.InitialFpuSlit),
      SqlField("initialFpuIfu",  GnirsSpectroscopyView.InitialFpuIfu),
      SqlField("filter",        GnirsSpectroscopyView.Filter),
      SqlField("initialFilter", GnirsSpectroscopyView.InitialFilter),
      SqlField("coadds",        GnirsSpectroscopyView.Coadds),

      // Decker: effective (DB-computed COALESCE), default, explicit
      SqlField("decker",         GnirsSpectroscopyView.DeckerEffective),
      SqlField("defaultDecker",  GnirsSpectroscopyView.DefaultDecker),
      SqlField("explicitDecker", GnirsSpectroscopyView.ExplicitDecker),

      // Read mode: explicit override only; when null the read mode is computed
      // from the exposure time at sequence-generation time (mirrors Flamingos2).
      SqlField("explicitReadMode", GnirsSpectroscopyView.ExplicitReadMode),

      // Well depth: effective (DB-computed COALESCE), default, explicit
      SqlField("wellDepth",         GnirsSpectroscopyView.WellDepthEffective),
      SqlField("defaultWellDepth",  GnirsSpectroscopyView.DefaultWellDepth),
      SqlField("explicitWellDepth", GnirsSpectroscopyView.ExplicitWellDepth),

      // Focus motor steps (null = best)
      SqlField("explicitFocusMotorSteps", GnirsSpectroscopyView.FocusMotorSteps),

      // Telescope configs raw columns (hidden, used for cursor fields)
      SqlField("slitOffsetModeEffRaw",  GnirsSpectroscopyView.SlitOffsetModeEffective,  hidden = true),
      SqlField("tcEffRaw",              GnirsSpectroscopyView.TelescopeConfigsEffective, hidden = true),
      SqlField("slitOffsetModeDefRaw",  GnirsSpectroscopyView.DefaultSlitOffsetMode,     hidden = true),
      SqlField("tcDefRaw",              GnirsSpectroscopyView.DefaultTelescopeConfigs,   hidden = true),
      SqlField("slitOffsetModeExpRaw",  GnirsSpectroscopyView.ExplicitSlitOffsetMode,    hidden = true),
      SqlField("tcExpRaw",              GnirsSpectroscopyView.ExplicitTelescopeConfigs,  hidden = true),

      // telescopeConfigs: effective SlitTelescopeConfigs = explicit coalesce default
      CursorFieldJson("telescopeConfigs",
        cursor =>
          for
            modeExp <- cursor.field("slitOffsetModeExpRaw", None).flatMap(_.as[Option[SlitOffsetMode]])
            tcExp   <- cursor.field("tcExpRaw", None).flatMap(_.as[Option[String]])
            modeDef <- cursor.field("slitOffsetModeDefRaw", None).flatMap(_.as[SlitOffsetMode])
            tcDef   <- cursor.field("tcDefRaw", None).flatMap(_.as[String])
          yield slitTelescopeConfigsJson(modeExp.getOrElse(modeDef), tcExp.getOrElse(tcDef)),
        List("slitOffsetModeExpRaw", "tcExpRaw", "slitOffsetModeDefRaw", "tcDefRaw")
      ),

      // defaultTelescopeConfigs: the pure default
      CursorFieldJson("defaultTelescopeConfigs",
        cursor =>
          for
            mode <- cursor.field("slitOffsetModeDefRaw", None).flatMap(_.as[SlitOffsetMode])
            json <- cursor.field("tcDefRaw", None).flatMap(_.as[String])
          yield slitTelescopeConfigsJson(mode, json),
        List("slitOffsetModeDefRaw", "tcDefRaw")
      ),

      // explicitTelescopeConfigs (nullable): present only when an explicit override is set
      CursorFieldJson("explicitTelescopeConfigs",
        cursor =>
          for
            modeOpt <- cursor.field("slitOffsetModeExpRaw", None).flatMap(_.as[Option[SlitOffsetMode]])
            jsonOpt <- cursor.field("tcExpRaw", None).flatMap(_.as[Option[String]])
          yield (modeOpt, jsonOpt).mapN(slitTelescopeConfigsJson).getOrElse(Json.Null),
        List("slitOffsetModeExpRaw", "tcExpRaw")
      ),

      // Acquisition sub-object
      SqlObject("acquisition"),

      // Telluric type (stored as jsonb)
      SqlJson("telluricType", GnirsSpectroscopyView.TelluricType),

    )

  lazy val GnirsSpectroscopyElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (GnirsSpectroscopyType, "exposureTimeMode", Nil) =>
      Elab.transformChild: child =>
        Unique(
          Filter(
            Predicates.exposureTimeMode.role.eql(ExposureTimeModeRole.Science),
            child
          )
        )

    case (GnirsSpectroscopyAcquisitionType, "exposureTimeMode", Nil) =>
      Elab.transformChild: child =>
        Unique(
          Filter(
            Predicates.exposureTimeMode.role.eql(ExposureTimeModeRole.Acquisition),
            child
          )
        )

}
