// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Query.Binding
import grackle.Query.Filter
import grackle.Query.Unique
import grackle.QueryCompiler.Elab
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.*
import lucuma.odb.json.offset.query.given

trait GnirsSpectroscopyMapping[F[_]]
  extends GnirsSpectroscopyView[F]
     with ExposureTimeModeMapping[F]
     with OptionalFieldMapping[F]
     with SlitTelescopeConfigsMapping[F]
     with Predicates[F] { this: SkunkMapping[F] =>

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

      // Camera + Filter + Wavelength
      SqlField("camera",        GnirsSpectroscopyView.Camera),
      SqlField("initialCamera", GnirsSpectroscopyView.InitialCamera),
      // FPU + telescope configs are grouped, by variant, into the slit / ifu
      // sub-objects. Exactly one is non-null per row (discriminated by which FPU
      // column is set); see GnirsSlitMapping / GnirsIfuMapping.
      SqlObject("slit"),
      SqlObject("ifu"),
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

      // Acquisition sub-object
      SqlObject("acquisition"),

      // Telluric type (stored as jsonb)
      SqlJson("telluricType", GnirsSpectroscopyView.TelluricType),

    )

  // Long-slit variant. Keyed on c_fpu_slit (embedded): null for IFU rows, so the
  // whole object resolves to null on `GnirsSpectroscopy.slit` for IFU observations.
  lazy val GnirsSlitMapping: ObjectMapping =
    ObjectMapping(GnirsSlitType)(

      SqlField("observationId", GnirsSpectroscopyView.ObservationId, key = true, hidden = true),

      SqlField("fpu",        GnirsSpectroscopyView.FpuSlitConfig, key = true),
      SqlField("initialFpu", GnirsSpectroscopyView.InitialFpuSlitConfig),

      // Raw columns (hidden) backing the telescope config cursor fields.
      SqlField("slitOffsetModeEffRaw",  GnirsSpectroscopyView.SlitOffsetModeEffective,  hidden = true),
      SqlField("tcEffRaw",              GnirsSpectroscopyView.TelescopeConfigsEffective, hidden = true),
      SqlField("slitOffsetModeDefRaw",  GnirsSpectroscopyView.DefaultSlitOffsetMode,     hidden = true),
      SqlField("tcDefRaw",              GnirsSpectroscopyView.DefaultTelescopeConfigs,   hidden = true),
      SqlField("slitOffsetModeExpRaw",  GnirsSpectroscopyView.ExplicitSlitOffsetMode,    hidden = true),
      SqlField("tcExpRaw",              GnirsSpectroscopyView.ExplicitTelescopeConfigs,  hidden = true),

      // effective (explicit coalesce default) and default: the offset mode is always
      // present for a long slit row, so these never resolve to null here.
      slitTelescopeConfigsField("telescopeConfigs",        "slitOffsetModeEffRaw", "tcEffRaw"),
      slitTelescopeConfigsField("defaultTelescopeConfigs", "slitOffsetModeDefRaw", "tcDefRaw"),
      // explicit (nullable): present only when an explicit override is set.
      explicitSlitTelescopeConfigsField("explicitTelescopeConfigs", "slitOffsetModeExpRaw", "tcExpRaw"),
    )

  // IFU variant. Keyed on c_fpu_ifu (embedded): null for long slit rows, so the
  // whole object resolves to null on `GnirsSpectroscopy.ifu` for long slit observations.
  lazy val GnirsIfuMapping: ObjectMapping =
    ObjectMapping(GnirsIfuType)(

      SqlField("observationId", GnirsSpectroscopyView.ObservationId, key = true, hidden = true),

      SqlField("fpu",        GnirsSpectroscopyView.FpuIfuConfig, key = true),
      SqlField("initialFpu", GnirsSpectroscopyView.InitialFpuIfuConfig),

      // IFU configs are a single stored value (seeded at creation), a plain
      // [TelescopeConfig] with no slit offset mode — no default / explicit split.
      SqlField("tcRaw", GnirsSpectroscopyView.TelescopeConfigsEffective, hidden = true),
      CursorFieldJson("telescopeConfigs",
        cursor => cursor.field("tcRaw", None).flatMap(_.as[String]).flatMap(ifuTelescopeConfigsJson),
        List("tcRaw")
      ),
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
