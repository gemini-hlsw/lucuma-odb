// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

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
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.math.WavelengthDither
import lucuma.core.math.units.Nanometer
import lucuma.core.model.sequence.gmos.longslit.*
import lucuma.core.model.sequence.gmos.mos.DefaultTelescopeConfigs
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.*
import lucuma.odb.json.wavelength.query.given
import lucuma.odb.sequence.gmos.longslit.Config as LongSlitConfig

trait GmosMosMapping[F[_]]
  extends GmosMosView[F]
     with SlitTelescopeConfigsMapping[F]
     with ExposureTimeModeMapping[F]
     with OptionalFieldMapping[F]
     with Predicates[F] { this: SkunkMapping[F] =>

  private class CommonFieldMappings(cc: GmosMosCommonColumns):

    import GmosMosMapping.*

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

    val telescopeConfigsRaw: FieldMapping =
      SqlField("telescopeConfigsRaw", cc.TelescopeConfigs, hidden = true)

    val telescopeConfigs: FieldMapping =
      effectiveTelescopeConfigsField("telescopeConfigs", "telescopeConfigsRaw", DefaultTelescopeConfigs)

    val defaultTelescopeConfigs: FieldMapping =
      CursorFieldJson("defaultTelescopeConfigs", _ => Result(telescopeConfigsJson(DefaultTelescopeConfigs)), Nil)

    val explicitTelescopeConfigs: FieldMapping =
      explicitTelescopeConfigsField("explicitTelescopeConfigs", "telescopeConfigsRaw")

  lazy val GmosNorthMosCustomMaskMapping: ObjectMapping =
    ObjectMapping(GmosNorthMosType / "customMask")(
      SqlField("observationId", GmosNorthMosView.Common.ObservationId, key = true, hidden = true),
      SqlField("slitWidth",    GmosNorthMosView.Common.SlitWidth),
      SqlField("attachmentId", GmosNorthMosView.Common.MaskAttachmentId)
    )

  lazy val GmosSouthMosCustomMaskMapping: ObjectMapping =
    ObjectMapping(GmosSouthMosType / "customMask")(
      SqlField("observationId", GmosSouthMosView.Common.ObservationId, key = true, hidden = true),
      SqlField("slitWidth",    GmosSouthMosView.Common.SlitWidth),
      SqlField("attachmentId", GmosSouthMosView.Common.MaskAttachmentId)
    )

  lazy val GmosNorthMosAcquisitionMapping: ObjectMapping =
    ObjectMapping(GmosNorthMosAcquisitionType)(
      SqlField("observationId", GmosNorthMosView.Common.ObservationId, key = true, hidden = true),

      explicitOrElseDefault[GmosNorthFilter]("filter", "explicitFilter", "defaultFilter"),
      SqlField("defaultFilter",  GmosNorthMosView.AcquisitionFilterDefault),
      SqlField("explicitFilter", GmosNorthMosView.AcquisitionFilter),

      SqlObject("exposureTimeMode", Join(GmosNorthMosView.Common.ObservationId, ExposureTimeModeView.ObservationId))
    )

  lazy val GmosNorthMosMapping: ObjectMapping =

    import GmosMosMapping.*

    val common = new CommonFieldMappings(GmosNorthMosView.Common)

    ObjectMapping(GmosNorthMosType)(

      SqlField("observationId", GmosNorthMosView.Common.ObservationId, key = true, hidden = true),

      SqlField("grating", GmosNorthMosView.Grating),
      SqlField("filter",  GmosNorthMosView.Filter),

      SqlObject("customMask"),

      SqlObject("centralWavelength"),
      SqlField("acquisitionType", GmosNorthMosView.Common.AcquisitionType),
      SqlObject("exposureTimeMode", Join(GmosNorthMosView.Common.ObservationId, ExposureTimeModeView.ObservationId)),
      SqlObject("acquisition"),

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

      common.telescopeConfigsRaw,
      common.telescopeConfigs,
      common.explicitTelescopeConfigs,
      common.defaultTelescopeConfigs,

      // Read-only snapshot of what the mode was created with.  The mask
      // attachment has no counterpart here: it is expected to arrive later.
      SqlField("initialGrating",   GmosNorthMosView.InitialGrating),
      SqlField("initialFilter",    GmosNorthMosView.InitialFilter),
      SqlField("initialSlitWidth", GmosNorthMosView.Common.InitialSlitWidth),
      SqlObject("initialCentralWavelength")
    )

  lazy val GmosSouthMosAcquisitionMapping: ObjectMapping =
    ObjectMapping(GmosSouthMosAcquisitionType)(
      SqlField("observationId", GmosSouthMosView.Common.ObservationId, key = true, hidden = true),

      explicitOrElseDefault[GmosSouthFilter]("filter", "explicitFilter", "defaultFilter"),
      SqlField("defaultFilter",  GmosSouthMosView.AcquisitionFilterDefault),
      SqlField("explicitFilter", GmosSouthMosView.AcquisitionFilter),

      SqlObject("exposureTimeMode", Join(GmosSouthMosView.Common.ObservationId, ExposureTimeModeView.ObservationId))
    )

  lazy val GmosSouthMosMapping: ObjectMapping =

    import GmosMosMapping.*

    val common = new CommonFieldMappings(GmosSouthMosView.Common)

    ObjectMapping(GmosSouthMosType)(

      SqlField("observationId", GmosSouthMosView.Common.ObservationId, key = true, hidden = true),

      SqlField("grating", GmosSouthMosView.Grating),
      SqlField("filter",  GmosSouthMosView.Filter),

      SqlObject("customMask"),

      SqlObject("centralWavelength"),
      SqlField("acquisitionType", GmosSouthMosView.Common.AcquisitionType),
      SqlObject("exposureTimeMode", Join(GmosSouthMosView.Common.ObservationId, ExposureTimeModeView.ObservationId)),
      SqlObject("acquisition"),

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

      common.telescopeConfigsRaw,
      common.telescopeConfigs,
      common.explicitTelescopeConfigs,
      common.defaultTelescopeConfigs,

      SqlField("initialGrating",   GmosSouthMosView.InitialGrating),
      SqlField("initialFilter",    GmosSouthMosView.InitialFilter),
      SqlField("initialSlitWidth", GmosSouthMosView.Common.InitialSlitWidth),
      SqlObject("initialCentralWavelength")
    )

  lazy val GmosMosElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (GmosNorthMosAcquisitionType, "exposureTimeMode", Nil) =>
      Elab.transformChild: child =>
        Unique(Filter(Predicates.exposureTimeMode.role.eql(ExposureTimeModeRole.Acquisition), child))

    case (GmosNorthMosType, "exposureTimeMode", Nil) =>
      Elab.transformChild: child =>
        Unique(Filter(Predicates.exposureTimeMode.role.eql(ExposureTimeModeRole.Science), child))

    case (GmosSouthMosAcquisitionType, "exposureTimeMode", Nil) =>
      Elab.transformChild: child =>
        Unique(Filter(Predicates.exposureTimeMode.role.eql(ExposureTimeModeRole.Acquisition), child))

    case (GmosSouthMosType, "exposureTimeMode", Nil) =>
      Elab.transformChild: child =>
        Unique(Filter(Predicates.exposureTimeMode.role.eql(ExposureTimeModeRole.Science), child))

  lazy val GmosMosMappings: List[TypeMapping] =
    List(
      GmosNorthMosMapping,
      GmosNorthMosCustomMaskMapping,
      GmosNorthMosAcquisitionMapping,
      GmosSouthMosMapping,
      GmosSouthMosCustomMaskMapping,
      GmosSouthMosAcquisitionMapping
    )
}

object GmosMosMapping:

  private def parseCsvBigDecimals(s: String): List[BigDecimal] =
    s.split(',').toList.map(n => BigDecimal(n.trim))

  private def decodeWavelengthDithers(s: String): Json =
    parseCsvBigDecimals(s).map(bd => WavelengthDither.nanometers.unsafeGet(bd.withUnit[Nanometer]).asJson).asJson

  private def defaultWavelengthDithersNorthJson(g: GmosNorthGrating): Json =
    LongSlitConfig.defaultWavelengthDithersNorth(g).map(_.asJson).asJson

  private def defaultWavelengthDithersSouthJson(g: GmosSouthGrating): Json =
    LongSlitConfig.defaultWavelengthDithersSouth(g).map(_.asJson).asJson

