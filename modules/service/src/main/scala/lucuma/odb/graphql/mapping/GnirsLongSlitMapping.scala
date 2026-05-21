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
import lucuma.core.enums.GnirsDecker
import lucuma.core.enums.GnirsObsReadMode
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.enums.SlitOffsetMode
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.format.telescopeConfigs.*
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.*
import lucuma.odb.json.offset.query.given

trait GnirsLongSlitMapping[F[_]]
  extends GnirsLongSlitView[F]
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
  // SlitTelescopeConfigs type: { offsetMode, alongSlit, onSky } where exactly
  // one of alongSlit / onSky is non-null depending on the discriminant.
  private def slitTelescopeConfigsJson(mode: SlitOffsetMode, json: String): Json =
    SlitTelescopeConfigsFormat.getOption((mode, json)).fold(Json.Null):
      case lucuma.core.model.SlitTelescopeConfigs.AlongSlit(nel) =>
        Json.obj(
          "offsetMode" -> mode.asJson,
          "alongSlit"  -> nel.toList.map(c => telescopeConfigAlongSlitJson(c.offset, c.guiding)).asJson,
          "onSky"      -> Json.Null
        )
      case lucuma.core.model.SlitTelescopeConfigs.ToSky(nel) =>
        Json.obj(
          "offsetMode" -> mode.asJson,
          "alongSlit"  -> Json.Null,
          "onSky"      -> nel.toList.map(tc => telescopeConfigJson(tc.offset, tc.guiding)).asJson
        )

  lazy val GnirsLongSlitAcquisitionMapping: ObjectMapping =
    ObjectMapping(GnirsLongSlitAcquisitionType)(

      SqlField("observationId", GnirsLongSlitView.ObservationId, key = true, hidden = true),

      SqlField("readMode",  GnirsLongSlitView.AcqReadMode),
      SqlField("coadds",    GnirsLongSlitView.AcqCoadds),
      SqlField("filter",    GnirsLongSlitView.AcqFilter),

      SqlField("acqOffPRaw", GnirsLongSlitView.AcqOffsetP, hidden = true),
      SqlField("acqOffQRaw", GnirsLongSlitView.AcqOffsetQ, hidden = true),

      CursorFieldJson("offset",
        cursor =>
          for
            p <- cursor.field("acqOffPRaw", None).flatMap(_.as[Option[lucuma.core.math.Angle]])
            q <- cursor.field("acqOffQRaw", None).flatMap(_.as[Option[lucuma.core.math.Angle]])
          yield (p, q) match
            case (Some(pa), Some(qa)) =>
              Offset(Offset.P(pa), Offset.Q(qa)).asJson
            case _ => Json.Null,
        List("acqOffPRaw", "acqOffQRaw")
      ),

      SqlObject("exposureTime"),
      SqlField("exposureCount", GnirsLongSlitView.AcqExpCount),
      SqlObject("exposureAt"),
    )

  lazy val GnirsLongSlitMapping: ObjectMapping =
    ObjectMapping(GnirsLongSlitType)(

      SqlField("observationId", GnirsLongSlitView.ObservationId, key = true, hidden = true),

      SqlObject("exposureTimeMode", Join(GnirsLongSlitView.ObservationId, ExposureTimeModeView.ObservationId)),

      // Grating: effective = COALESCE(explicit, initial)
      SqlField("grating",        GnirsLongSlitView.GratingEffective),
      SqlField("explicitGrating", GnirsLongSlitView.Grating),
      SqlField("initialGrating", GnirsLongSlitView.InitialGrating),

      // Prism: effective = COALESCE(explicit, initial)
      SqlField("prism",          GnirsLongSlitView.PrismEffective),
      SqlField("explicitPrism",  GnirsLongSlitView.Prism),
      SqlField("initialPrism",   GnirsLongSlitView.InitialPrism),

      // Grating wavelength: explicit + default + effective (all Wavelength objects)
      SqlObject("gratingWavelength"),
      SqlObject("explicitGratingWavelength"),
      SqlObject("defaultGratingWavelength"),

      // Camera + FPU + Filter + Wavelength
      SqlField("camera",        GnirsLongSlitView.Camera),
      SqlField("initialCamera", GnirsLongSlitView.InitialCamera),
      SqlField("fpu",           GnirsLongSlitView.Fpu),
      SqlField("initialFpu",    GnirsLongSlitView.InitialFpu),
      SqlField("filter",        GnirsLongSlitView.Filter),
      SqlField("initialFilter", GnirsLongSlitView.InitialFilter),
      SqlObject("centralWavelength"),
      SqlField("coadds",        GnirsLongSlitView.Coadds),

      // Decker: explicit override + computed default + effective
      explicitOrElseDefault[GnirsDecker]("decker", "explicitDecker", "defaultDecker"),
      SqlField("explicitDecker", GnirsLongSlitView.ExplicitDecker),
      SqlField("defaultDecker",  GnirsLongSlitView.DefaultDecker),

      // Read mode: explicit + default + effective
      explicitOrElseDefault[GnirsObsReadMode]("readMode", "explicitReadMode", "defaultReadMode"),
      SqlField("explicitReadMode", GnirsLongSlitView.ExplicitReadMode),
      SqlField("defaultReadMode",  GnirsLongSlitView.DefaultReadMode),

      // Well depth: explicit + default + effective
      explicitOrElseDefault[GnirsWellDepth]("wellDepth", "explicitWellDepth", "defaultWellDepth"),
      SqlField("explicitWellDepth", GnirsLongSlitView.ExplicitWellDepth),
      SqlField("defaultWellDepth",  GnirsLongSlitView.DefaultWellDepth),

      // Focus motor steps (null = best)
      SqlField("explicitFocusMotorSteps", GnirsLongSlitView.FocusMotorSteps),

      // Telescope configs raw columns (hidden, used for cursor fields)
      SqlField("slitOffsetModeEffRaw",  GnirsLongSlitView.SlitOffsetModeEffective,  hidden = true),
      SqlField("tcEffRaw",              GnirsLongSlitView.TelescopeConfigsEffective, hidden = true),
      SqlField("slitOffsetModeDefRaw",  GnirsLongSlitView.DefaultSlitOffsetMode,     hidden = true),
      SqlField("tcDefRaw",              GnirsLongSlitView.DefaultTelescopeConfigs,   hidden = true),
      SqlField("slitOffsetModeExpRaw",  GnirsLongSlitView.ExplicitSlitOffsetMode,    hidden = true),
      SqlField("tcExpRaw",              GnirsLongSlitView.ExplicitTelescopeConfigs,  hidden = true),

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

    )

  lazy val GnirsLongSlitElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (GnirsLongSlitType, "exposureTimeMode", Nil) =>
      Elab.transformChild: child =>
        Unique(
          Filter(
            Predicates.exposureTimeMode.role.eql(ExposureTimeModeRole.Science),
            child
          )
        )

}
