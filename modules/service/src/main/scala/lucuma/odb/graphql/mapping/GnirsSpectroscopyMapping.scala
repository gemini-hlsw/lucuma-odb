// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Query.Binding
import grackle.Query.Filter
import grackle.Query.OrderBy
import grackle.Query.OrderSelection
import grackle.Query.OrderSelections
import grackle.Query.Unique
import grackle.QueryCompiler.Elab
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.data.ObservingModeRowVersion
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

      // The effective acquisition exposure time mode, and the explicit override.
      // Both read the same t_exposure_time_mode row; the explicit one is keyed on the
      // view's c_explicit_* columns so it is null when the mode is derived.
      SqlObject("exposureTimeMode", Join(GnirsSpectroscopyView.ObservationId, ExposureTimeModeView.ObservationId)),
      SqlObject("explicitExposureTimeMode", Join(GnirsSpectroscopyView.ObservationId, ExposureTimeModeView.ObservationId)),
    )

  /**
   * One central wavelength with the exposure time mode and coadds that apply
   * there.  Keyed on (observation, wavelength, version) to match the table's
   * primary key.
   */
  lazy val GnirsCentralWavelengthConfigMapping: ObjectMapping =
    ObjectMapping(GnirsCentralWavelengthConfigType)(
      SqlField("observationId",     GnirsCentralWavelengthConfigTable.ObservationId, key = true, hidden = true),
      // `centralWavelength` is an object in the schema (see WavelengthMapping), so the
      // key is a hidden field on the same column.  It doubles as the sort key.
      SqlField("centralWavelengthKey", GnirsCentralWavelengthConfigTable.CentralWavelength, key = true, hidden = true),
      SqlField("version",           GnirsCentralWavelengthConfigTable.Version, key = true, hidden = true),
      SqlObject("centralWavelength"),
      SqlField("coadds",            GnirsCentralWavelengthConfigTable.Coadds),
      SqlObject("exposureTimeMode", Join(GnirsCentralWavelengthConfigTable.ExposureTimeModeId, ExposureTimeModeView.Id))
    )

  /**
   * Everything long slit and IFU have in common.  Shared by the two per-mode
   * types and by the deprecated combined `GnirsSpectroscopy`.
   */
  private def commonFields: List[FieldMapping] =
    List(

      SqlField("observationId", GnirsSpectroscopyView.ObservationId, key = true, hidden = true),

      // Grating: effective = COALESCE(explicit, initial)
      SqlField("grating",        GnirsSpectroscopyView.GratingEffective),
      SqlField("explicitGrating", GnirsSpectroscopyView.Grating),
      SqlField("initialGrating", GnirsSpectroscopyView.InitialGrating),

      // Prism: effective = COALESCE(explicit, initial)
      SqlField("prism",          GnirsSpectroscopyView.PrismEffective),
      SqlField("explicitPrism",  GnirsSpectroscopyView.Prism),
      SqlField("initialPrism",   GnirsSpectroscopyView.InitialPrism),

      // Central wavelengths: one child row each, in the "current" and "initial"
      // row versions respectively (see the elaborator below).
      SqlObject("centralWavelengths",        Join(GnirsSpectroscopyView.ObservationId, GnirsCentralWavelengthConfigTable.ObservationId)),
      SqlObject("initialCentralWavelengths", Join(GnirsSpectroscopyView.ObservationId, GnirsCentralWavelengthConfigTable.ObservationId)),

      // Camera + Filter
      SqlField("camera",        GnirsSpectroscopyView.Camera),
      SqlField("initialCamera", GnirsSpectroscopyView.InitialCamera),
      SqlField("filter",        GnirsSpectroscopyView.Filter),
      SqlField("initialFilter", GnirsSpectroscopyView.InitialFilter),

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

  /**
   * FPU and telescope configs for the long slit.  `fpu` is a key on the embedded
   * (FailedJoin-on-null) column alias, so the object resolves to null on an IFU
   * row -- whether it appears as `GnirsSpectroscopy.slit` or as the whole
   * `ObservingMode.gnirsLongSlit`.
   */
  private def longSlitFields: List[FieldMapping] =
    List(

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

  /** The IFU counterpart of `longSlitFields`, keyed on c_fpu_ifu. */
  private def ifuFields: List[FieldMapping] =
    List(

      SqlField("fpu",        GnirsSpectroscopyView.FpuIfuConfig, key = true),
      SqlField("initialFpu", GnirsSpectroscopyView.InitialFpuIfuConfig),

      // IFU configs are a single stored value (seeded at creation), a plain
      // [TelescopeConfig] with no slit offset mode -- no default / explicit split.
      SqlField("tcRaw", GnirsSpectroscopyView.TelescopeConfigsEffective, hidden = true),
      CursorFieldJson("telescopeConfigs",
        cursor => cursor.field("tcRaw", None).flatMap(_.as[String]).flatMap(ifuTelescopeConfigsJson),
        List("tcRaw")
      ),

    )

  lazy val GnirsLongSlitMapping: ObjectMapping =
    ObjectMapping(GnirsLongSlitType)(commonFields ++ longSlitFields*)

  lazy val GnirsIfuMapping: ObjectMapping =
    ObjectMapping(GnirsIfuType)(commonFields ++ ifuFields*)

  /**
   * The deprecated combined view of the two modes, which keeps the FPU and
   * telescope configs behind `slit` / `ifu` sub-objects instead of inlining them.
   */
  lazy val GnirsSpectroscopyMapping: ObjectMapping =
    ObjectMapping(GnirsSpectroscopyType)(commonFields ++ List(SqlObject("slit"), SqlObject("ifu"))*)

  lazy val GnirsSpectroscopyLongSlitMapping: ObjectMapping =
    ObjectMapping(GnirsSpectroscopyLongSlitType)(
      SqlField("observationId", GnirsSpectroscopyView.ObservationId, key = true, hidden = true) +: longSlitFields*
    )

  lazy val GnirsSpectroscopyIfuMapping: ObjectMapping =
    ObjectMapping(GnirsSpectroscopyIfuType)(
      SqlField("observationId", GnirsSpectroscopyView.ObservationId, key = true, hidden = true) +: ifuFields*
    )

  // Order the central wavelengths by increasing wavelength -- the order the
  // sequence executes them in -- and limit to one row version.
  private def wavelengthElaborator(v: ObservingModeRowVersion): Elab[Unit] =
    Elab.transformChild: child =>
      OrderBy(
        OrderSelections(List(OrderSelection[Wavelength](GnirsCentralWavelengthConfigType / "centralWavelengthKey"))),
        Filter(Predicates.gnirsSpectroscopyWavelength.version.eql(v), child)
      )

  lazy val GnirsSpectroscopyElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (GnirsLongSlitType | GnirsIfuType | GnirsSpectroscopyType, "centralWavelengths", Nil) =>
      wavelengthElaborator(ObservingModeRowVersion.Current)

    case (GnirsLongSlitType | GnirsIfuType | GnirsSpectroscopyType, "initialCentralWavelengths", Nil) =>
      wavelengthElaborator(ObservingModeRowVersion.Initial)

    case (GnirsSpectroscopyAcquisitionType, "exposureTimeMode" | "explicitExposureTimeMode", Nil) =>
      Elab.transformChild: child =>
        Unique(
          Filter(
            Predicates.exposureTimeMode.role.eql(ExposureTimeModeRole.Acquisition),
            child
          )
        )

}
