// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.syntax.all.*
import coulomb.syntax.*
import grackle.Query.Binding
import grackle.Query.Filter
import grackle.Query.Unique
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosIfuAcquisitionRoi
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.math.Angle
import lucuma.core.math.WavelengthDither
import lucuma.core.math.units.Nanometer
import lucuma.core.model.GmosIfuAnalysis
import lucuma.core.model.sequence.gmos.longslit.*
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.*
import lucuma.odb.json.angle.query.given
import lucuma.odb.json.wavelength.query.given
import lucuma.odb.sequence.gmos.longslit.Config as LongSlitConfig

/**
 * Grackle mappings for the GMOS North/South IFU observing mode.
 *
 * The shape follows GMOS MOS, since both read out through the same long slit machinery and store
 * a plain list of telescope configurations.  What IFU adds is the aperture and the sampling
 * geometry the ITC integrates over.
 */
trait GmosIfuMapping[F[_]]
  extends GmosIfuView[F]
     with SlitTelescopeConfigsMapping[F]
     with ExposureTimeModeMapping[F]
     with OptionalFieldMapping[F]
     with Predicates[F] { this: SkunkMapping[F] =>

  private class CommonFieldMappings(cc: GmosIfuCommonColumns):

    import GmosIfuMapping.*

    val xBin: FieldMapping                = explicitOrElseDefault[GmosXBinning]("xBin", "explicitXBin", "defaultXBin")
    val defaultXBin: FieldMapping         = SqlField("defaultXBin", cc.XBinDefault)
    val explicitXBin: FieldMapping        = SqlField("explicitXBin", cc.XBin)

    val yBin: FieldMapping                = explicitOrElseDefault[GmosYBinning]("yBin", "explicitYBin", "defaultYBin")
    val defaultYBin: FieldMapping         = SqlField("defaultYBin", cc.YBinDefault)
    val explicitYBin: FieldMapping        = SqlField("explicitYBin", cc.YBin)

    val ampReadMode: FieldMapping         = explicitOrElseDefault[GmosAmpReadMode]("ampReadMode", "explicitAmpReadMode", "defaultAmpReadMode")
    val defaultAmpReadMode: FieldMapping  = CursorField[GmosAmpReadMode]("defaultAmpReadMode", _ => Result(DefaultAmpReadMode))
    val explicitAmpReadMode: FieldMapping = SqlField("explicitAmpReadMode", cc.AmpReadMode)

    val ampGain: FieldMapping             = explicitOrElseDefault[GmosAmpGain]("ampGain", "explicitAmpGain", "defaultAmpGain")
    val defaultAmpGain: FieldMapping      = CursorField[GmosAmpGain]("defaultAmpGain", _ => Result(DefaultAmpGain))
    val explicitAmpGain: FieldMapping     = SqlField("explicitAmpGain", cc.AmpGain)

    val roi: FieldMapping                 = explicitOrElseDefault[GmosRoi]("roi", "explicitRoi", "defaultRoi")
    val defaultRoi: FieldMapping          = CursorField[GmosRoi]("defaultRoi", _ => Result(DefaultRoi))
    val explicitRoi: FieldMapping         = SqlField("explicitRoi", cc.Roi)

    val wavelengthDithersString: FieldMapping   =
      SqlField("wavelengthDithersString", cc.WavelengthDithers, hidden = true)

    val explicitWavelengthDithers: FieldMapping =
      CursorFieldJson(
        "explicitWavelengthDithers",
        cursor =>
          cursor
            .field("wavelengthDithersString", None)
            .flatMap(_.as[Option[String]].map(_.map(decodeWavelengthDithers).asJson)),
        List("wavelengthDithersString")
      )

    val telescopeConfigsExpRaw: FieldMapping =
      SqlField("telescopeConfigsExpRaw", cc.TelescopeConfigs, hidden = true)

    val telescopeConfigsDefRaw: FieldMapping =
      SqlField("telescopeConfigsDefRaw", cc.TelescopeConfigsDefault, hidden = true)

    val telescopeConfigsEffRaw: FieldMapping =
      SqlField("telescopeConfigsEffRaw", cc.TelescopeConfigsEffective, hidden = true)

    val telescopeConfigs: FieldMapping =
      plainTelescopeConfigsField("telescopeConfigs", "telescopeConfigsEffRaw")

    val defaultTelescopeConfigs: FieldMapping =
      plainTelescopeConfigsField("defaultTelescopeConfigs", "telescopeConfigsDefRaw")

    val explicitTelescopeConfigs: FieldMapping =
      explicitTelescopeConfigsField("explicitTelescopeConfigs", "telescopeConfigsExpRaw")

  lazy val GmosNorthIfuAcquisitionMapping: ObjectMapping =
    ObjectMapping(GmosNorthIfuAcquisitionType)(
      SqlField("observationId", GmosNorthIfuView.Common.ObservationId, key = true, hidden = true),

      explicitOrElseDefault[GmosNorthFilter]("filter", "explicitFilter", "defaultFilter"),
      SqlField("defaultFilter",  GmosNorthIfuView.AcquisitionFilterDefault),
      SqlField("explicitFilter", GmosNorthIfuView.AcquisitionFilter),

      explicitOrElseDefault[GmosIfuAcquisitionRoi]("roi", "explicitRoi", "defaultRoi"),
      SqlField("defaultRoi",  GmosNorthIfuView.AcquisitionRoiDefault),
      SqlField("explicitRoi", GmosNorthIfuView.AcquisitionRoi),

      SqlObject("exposureTimeMode", Join(GmosNorthIfuView.Common.ObservationId, ExposureTimeModeView.ObservationId))
    )

  lazy val GmosNorthIfuMapping: ObjectMapping =

    import GmosIfuMapping.*

    val common = new CommonFieldMappings(GmosNorthIfuView.Common)

    ObjectMapping(GmosNorthIfuType)(

      SqlField("observationId", GmosNorthIfuView.Common.ObservationId, key = true, hidden = true),

      SqlField("grating", GmosNorthIfuView.Grating),
      SqlField("filter",  GmosNorthIfuView.Filter),
      SqlField("fpu",     GmosNorthIfuView.Fpu),

      SqlObject("centralWavelength"),
      SqlObject("exposureTimeMode", Join(GmosNorthIfuView.Common.ObservationId, ExposureTimeModeView.ObservationId)),
      SqlObject("acquisition"),

      SqlField("ifuAnalysisSumRadiusRaw",    GmosNorthIfuView.Common.IfuAnalysisSumRadius,    hidden = true),
      SqlField("ifuAnalysisSingleOffsetRaw", GmosNorthIfuView.Common.IfuAnalysisSingleOffset, hidden = true),

      CursorFieldJson(
        "ifuAnalysis",
        cursor =>
          for {
            r <- cursor.field("ifuAnalysisSumRadiusRaw", None).flatMap(_.as[Option[Angle]])
            o <- cursor.field("ifuAnalysisSingleOffsetRaw", None).flatMap(_.as[Option[Angle]])
          } yield explicitAnalysisJson(r, o).getOrElse(DefaultAnalysisJson),
        List("ifuAnalysisSumRadiusRaw", "ifuAnalysisSingleOffsetRaw")
      ),

      CursorFieldJson("defaultIfuAnalysis", _ => Result(DefaultAnalysisJson), Nil),

      CursorFieldJson(
        "explicitIfuAnalysis",
        cursor =>
          for {
            r <- cursor.field("ifuAnalysisSumRadiusRaw", None).flatMap(_.as[Option[Angle]])
            o <- cursor.field("ifuAnalysisSingleOffsetRaw", None).flatMap(_.as[Option[Angle]])
          } yield explicitAnalysisJson(r, o).getOrElse(Json.Null),
        List("ifuAnalysisSumRadiusRaw", "ifuAnalysisSingleOffsetRaw")
      ),

      common.xBin,
      common.defaultXBin,
      common.explicitXBin,

      common.yBin,
      common.defaultYBin,
      common.explicitYBin,

      common.ampReadMode,
      common.defaultAmpReadMode,
      common.explicitAmpReadMode,

      common.ampGain,
      common.defaultAmpGain,
      common.explicitAmpGain,

      common.roi,
      common.defaultRoi,
      common.explicitRoi,

      common.wavelengthDithersString,

      CursorFieldJson(
        "wavelengthDithers",
        cursor =>
          for {
            e <- cursor.field("wavelengthDithersString", None).flatMap(_.as[Option[String]].map(_.map(decodeWavelengthDithers)))
            d <- cursor.field("grating", None).flatMap(_.as[GmosNorthGrating]).map(defaultWavelengthDithersNorthJson)
          } yield e.getOrElse(d),
        List("wavelengthDithersString", "grating")
      ),

      common.explicitWavelengthDithers,

      CursorFieldJson(
        "defaultWavelengthDithers",
        cursor =>
          cursor
            .field("grating", None)
            .flatMap(_.as[GmosNorthGrating])
            .map(defaultWavelengthDithersNorthJson),
        List("grating")
      ),

      common.telescopeConfigsExpRaw,
      common.telescopeConfigsDefRaw,
      common.telescopeConfigsEffRaw,
      common.telescopeConfigs,
      common.explicitTelescopeConfigs,
      common.defaultTelescopeConfigs,

      // Read-only snapshot of what the mode was created with.
      SqlField("initialGrating", GmosNorthIfuView.InitialGrating),
      SqlField("initialFilter",  GmosNorthIfuView.InitialFilter),
      SqlField("initialFpu",     GmosNorthIfuView.InitialFpu),
      SqlObject("initialCentralWavelength")
    )

  lazy val GmosSouthIfuAcquisitionMapping: ObjectMapping =
    ObjectMapping(GmosSouthIfuAcquisitionType)(
      SqlField("observationId", GmosSouthIfuView.Common.ObservationId, key = true, hidden = true),

      explicitOrElseDefault[GmosSouthFilter]("filter", "explicitFilter", "defaultFilter"),
      SqlField("defaultFilter",  GmosSouthIfuView.AcquisitionFilterDefault),
      SqlField("explicitFilter", GmosSouthIfuView.AcquisitionFilter),

      explicitOrElseDefault[GmosIfuAcquisitionRoi]("roi", "explicitRoi", "defaultRoi"),
      SqlField("defaultRoi",  GmosSouthIfuView.AcquisitionRoiDefault),
      SqlField("explicitRoi", GmosSouthIfuView.AcquisitionRoi),

      SqlObject("exposureTimeMode", Join(GmosSouthIfuView.Common.ObservationId, ExposureTimeModeView.ObservationId))
    )

  lazy val GmosSouthIfuMapping: ObjectMapping =

    import GmosIfuMapping.*

    val common = new CommonFieldMappings(GmosSouthIfuView.Common)

    ObjectMapping(GmosSouthIfuType)(

      SqlField("observationId", GmosSouthIfuView.Common.ObservationId, key = true, hidden = true),

      SqlField("grating", GmosSouthIfuView.Grating),
      SqlField("filter",  GmosSouthIfuView.Filter),
      SqlField("fpu",     GmosSouthIfuView.Fpu),

      SqlObject("centralWavelength"),
      SqlObject("exposureTimeMode", Join(GmosSouthIfuView.Common.ObservationId, ExposureTimeModeView.ObservationId)),
      SqlObject("acquisition"),

      SqlField("ifuAnalysisSumRadiusRaw",    GmosSouthIfuView.Common.IfuAnalysisSumRadius,    hidden = true),
      SqlField("ifuAnalysisSingleOffsetRaw", GmosSouthIfuView.Common.IfuAnalysisSingleOffset, hidden = true),

      CursorFieldJson(
        "ifuAnalysis",
        cursor =>
          for {
            r <- cursor.field("ifuAnalysisSumRadiusRaw", None).flatMap(_.as[Option[Angle]])
            o <- cursor.field("ifuAnalysisSingleOffsetRaw", None).flatMap(_.as[Option[Angle]])
          } yield explicitAnalysisJson(r, o).getOrElse(DefaultAnalysisJson),
        List("ifuAnalysisSumRadiusRaw", "ifuAnalysisSingleOffsetRaw")
      ),

      CursorFieldJson("defaultIfuAnalysis", _ => Result(DefaultAnalysisJson), Nil),

      CursorFieldJson(
        "explicitIfuAnalysis",
        cursor =>
          for {
            r <- cursor.field("ifuAnalysisSumRadiusRaw", None).flatMap(_.as[Option[Angle]])
            o <- cursor.field("ifuAnalysisSingleOffsetRaw", None).flatMap(_.as[Option[Angle]])
          } yield explicitAnalysisJson(r, o).getOrElse(Json.Null),
        List("ifuAnalysisSumRadiusRaw", "ifuAnalysisSingleOffsetRaw")
      ),

      common.xBin,
      common.defaultXBin,
      common.explicitXBin,

      common.yBin,
      common.defaultYBin,
      common.explicitYBin,

      common.ampReadMode,
      common.defaultAmpReadMode,
      common.explicitAmpReadMode,

      common.ampGain,
      common.defaultAmpGain,
      common.explicitAmpGain,

      common.roi,
      common.defaultRoi,
      common.explicitRoi,

      common.wavelengthDithersString,

      CursorFieldJson(
        "wavelengthDithers",
        cursor =>
          for {
            e <- cursor.field("wavelengthDithersString", None).flatMap(_.as[Option[String]].map(_.map(decodeWavelengthDithers)))
            d <- cursor.field("grating", None).flatMap(_.as[GmosSouthGrating]).map(defaultWavelengthDithersSouthJson)
          } yield e.getOrElse(d),
        List("wavelengthDithersString", "grating")
      ),

      common.explicitWavelengthDithers,

      CursorFieldJson(
        "defaultWavelengthDithers",
        cursor =>
          cursor
            .field("grating", None)
            .flatMap(_.as[GmosSouthGrating])
            .map(defaultWavelengthDithersSouthJson),
        List("grating")
      ),

      common.telescopeConfigsExpRaw,
      common.telescopeConfigsDefRaw,
      common.telescopeConfigsEffRaw,
      common.telescopeConfigs,
      common.explicitTelescopeConfigs,
      common.defaultTelescopeConfigs,

      // Read-only snapshot of what the mode was created with.
      SqlField("initialGrating", GmosSouthIfuView.InitialGrating),
      SqlField("initialFilter",  GmosSouthIfuView.InitialFilter),
      SqlField("initialFpu",     GmosSouthIfuView.InitialFpu),
      SqlObject("initialCentralWavelength")
    )

  lazy val GmosIfuElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (GmosNorthIfuAcquisitionType, "exposureTimeMode", Nil) =>
      Elab.transformChild: child =>
        Unique(Filter(Predicates.exposureTimeMode.role.eql(ExposureTimeModeRole.Acquisition), child))

    case (GmosNorthIfuType, "exposureTimeMode", Nil) =>
      Elab.transformChild: child =>
        Unique(Filter(Predicates.exposureTimeMode.role.eql(ExposureTimeModeRole.Science), child))

    case (GmosSouthIfuAcquisitionType, "exposureTimeMode", Nil) =>
      Elab.transformChild: child =>
        Unique(Filter(Predicates.exposureTimeMode.role.eql(ExposureTimeModeRole.Acquisition), child))

    case (GmosSouthIfuType, "exposureTimeMode", Nil) =>
      Elab.transformChild: child =>
        Unique(Filter(Predicates.exposureTimeMode.role.eql(ExposureTimeModeRole.Science), child))

  lazy val GmosIfuMappings: List[TypeMapping] =
    List(
      GmosNorthIfuMapping,
      GmosNorthIfuAcquisitionMapping,
      GmosSouthIfuMapping,
      GmosSouthIfuAcquisitionMapping
    )
}

object GmosIfuMapping:

  private def parseCsvBigDecimals(s: String): List[BigDecimal] =
    s.split(',').toList.map(n => BigDecimal(n.trim))

  private def decodeWavelengthDithers(s: String): Json =
    parseCsvBigDecimals(s).map(bd => WavelengthDither.nanometers.unsafeGet(bd.withUnit[Nanometer]).asJson).asJson

  private def defaultWavelengthDithersNorthJson(g: GmosNorthGrating): Json =
    LongSlitConfig.defaultWavelengthDithersNorth(g).map(_.asJson).asJson

  private def defaultWavelengthDithersSouthJson(g: GmosSouthGrating): Json =
    LongSlitConfig.defaultWavelengthDithersSouth(g).map(_.asJson).asJson

  // Both are distances from the field centre, so both are plain unsigned angles; the input
  // binding rejects a negative, which `Angle` could not carry anyway.
  private def analysisJson(sumRadius: Option[Angle], singleOffset: Option[Angle]): Json =
    Json.obj(
      "sumRadius"    -> sumRadius.asJson,
      "singleOffset" -> singleOffset.asJson
    )

  /** Null unless one of the two columns is set; the table permits at most one. */
  private def explicitAnalysisJson(sumRadius: Option[Angle], singleOffset: Option[Angle]): Option[Json] =
    (sumRadius, singleOffset) match
      case (None, None) => none
      case _            => analysisJson(sumRadius, singleOffset).some

  private val DefaultAnalysisJson: Json =
    GmosIfuAnalysis.Default match
      case GmosIfuAnalysis.Sum(radius)    => analysisJson(radius.some, none)
      case GmosIfuAnalysis.Single(offset) => analysisJson(none, offset.some)
