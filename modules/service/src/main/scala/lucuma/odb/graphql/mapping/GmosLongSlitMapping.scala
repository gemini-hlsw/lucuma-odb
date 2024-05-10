// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.syntax.all.*
import coulomb.*
import grackle.Cursor
import grackle.Result
import grackle.skunk.SkunkMapping
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.ImageQuality
import lucuma.core.math.Offset.Q
import lucuma.core.math.WavelengthDither
import lucuma.core.math.units.Nanometer
import lucuma.core.model.SourceProfile
import lucuma.core.model.sequence.gmos.binning.DefaultSampling
import lucuma.core.model.sequence.gmos.longslit.*
import lucuma.odb.graphql.table.*
import lucuma.odb.json.offset.query.given
import lucuma.odb.json.sourceprofile.given
import lucuma.odb.json.wavelength.query.given
import lucuma.odb.sequence.gmos.longslit.Config

import scala.reflect.ClassTag

trait GmosLongSlitMapping[F[_]]
  extends GmosLongSlitView[F] { this: SkunkMapping[F] =>

  private def explicitOrElseDefault[A: ClassTag : io.circe.Encoder](
    name:          String,
    explicitField: String,
    defaultField:  String
  ): CursorField[A] =
    CursorField[A](
      name,
      cursor => {
        (cursor.fieldAs[Option[A]](explicitField),
          cursor.fieldAs[A](defaultField)
        ).parMapN(_.getOrElse(_))
      },
      List(explicitField, defaultField)
    )

  private class CommonFieldMappings(cc: CommonColumns) {

    import GmosLongSlitMapping._

    val xBin: FieldMapping                = explicitOrElseDefault[GmosXBinning]("xBin", "explicitXBin", "defaultXBin")
    val explicitXBin: FieldMapping        = SqlField("explicitXBin", cc.XBin)

    val yBin: FieldMapping                = explicitOrElseDefault[GmosYBinning]("yBin", "explicitYBin", "defaultYBin")
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

    val spatialOffsetsString: FieldMapping =
      SqlField("spatialOffsetsString", cc.SpatialOffsets, hidden = true)

    val spatialOffsets: FieldMapping =
      CursorFieldJson("spatialOffsets",
        cursor =>
          cursor
            .field("spatialOffsetsString", None)
            .flatMap(_.as[Option[String]].map(_.map(decodeSpatialOffsets)))
            .map(_.getOrElse(defaultSpatialOffsetsJson)),
        List("explicitSpatialOffsets", "defaultSpatialOffsets")
      )

    val explicitSpatialOffsets: FieldMapping =
      CursorFieldJson("explicitSpatialOffsets",
        cursor =>
          cursor
            .field("spatialOffsetsString", None)
            .flatMap(_.as[Option[String]].map(_.map(decodeSpatialOffsets).asJson)),
        List("spatialOffsetsString")
      )

    val defaultSpatialOffsets: FieldMapping =
      CursorFieldJson("defaultSpatialOffsets", _ => Result(defaultSpatialOffsetsJson), Nil)

    val imageQuality: FieldMapping =
      SqlField("imageQuality", cc.ImageQuality, hidden = true)

    val sourceProfile: FieldMapping =
      SqlField("sourceProfile", cc.SourceProfile, hidden = true)

  }

  lazy val GmosNorthLongSlitMapping: ObjectMapping = {

    import GmosLongSlitMapping._

    val common = new CommonFieldMappings(GmosNorthLongSlitView.Common)

    ObjectMapping(GmosNorthLongSlitType)(

      SqlField("observationId", GmosNorthLongSlitView.Common.ObservationId, key = true, hidden = true),

      SqlField("grating", GmosNorthLongSlitView.Grating),
      SqlField("filter",  GmosNorthLongSlitView.Filter),
      SqlField("fpu",     GmosNorthLongSlitView.Fpu),
      SqlObject("centralWavelength"),

      // ---------------------
      // xBin
      // ---------------------

      common.xBin,

      CursorField[GmosXBinning](
        "defaultXBin",
        defaultBinning(northBinning(_, _, _, _, sampling = DefaultSampling)).map(_._1F),
        List("fpu", "grating", "imageQuality", "sourceProfile")
      ),

      common.explicitXBin,

      common.yBin,

      CursorField[GmosYBinning](
        "defaultYBin",
        defaultBinning(northBinning(_, _, _, _, sampling = DefaultSampling)).map(_._2F),
        List("fpu", "grating", "imageQuality", "sourceProfile")
      ),

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

      // ---------------------
      // wavelengthDithers
      // ---------------------

      common.wavelengthDithersString,

      CursorFieldJson(
        "wavelengthDithers",
        cursor =>
          for {
              e <- cursor.field("wavelengthDithersString", None).flatMap(_.as[Option[String]].map(_.map(decodeWavelengthDithers)))
              d <- cursor.field("grating", None).flatMap(_.as[GmosNorthGrating]).map(g => defaultWavelengthDithersNorthJson(g))
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
            .map(g => defaultWavelengthDithersNorthJson(g)),
        List("grating")
      ),

      // ---------------------
      // spatialOffsets
      // ---------------------
      common.spatialOffsetsString,
      common.spatialOffsets,
      common.explicitSpatialOffsets,
      common.defaultSpatialOffsets,

      // ---------------------
      // hidden view fields
      // ---------------------
      common.imageQuality,
      common.sourceProfile,

      // ---------------------
      // initialValues
      // ---------------------

      // We keep up with (read-only) values that were used to create the GMOS LongSlit observing mode initially.
      // Any changes are made via editing `grating`, `filter`, `fpu` and `centralWavelength`.
      SqlField("initialGrating", GmosNorthLongSlitView.InitialGrating),
      SqlField("initialFilter",  GmosNorthLongSlitView.InitialFilter),
      SqlField("initialFpu",     GmosNorthLongSlitView.InitialFpu),
      SqlObject("initialCentralWavelength")
    )

  }

  lazy val GmosSouthLongSlitMapping: ObjectMapping = {

    import GmosLongSlitMapping._

    val common = new CommonFieldMappings(GmosSouthLongSlitView.Common)

    ObjectMapping(GmosSouthLongSlitType)(

      SqlField("observationId", GmosSouthLongSlitView.Common.ObservationId, key = true, hidden = true),

      SqlField("grating", GmosSouthLongSlitView.Grating),
      SqlField("filter",  GmosSouthLongSlitView.Filter),
      SqlField("fpu",     GmosSouthLongSlitView.Fpu),
      SqlObject("centralWavelength"),

      // ---------------------
      // xBin
      // ---------------------

      common.xBin,

      CursorField[GmosXBinning](
        "defaultXBin",
        defaultBinning(southBinning(_, _, _, _, sampling = DefaultSampling)).map(_._1F),
        List("fpu", "grating", "imageQuality", "sourceProfile")
      ),

      common.explicitXBin,

      common.yBin,

      CursorField[GmosYBinning](
        "defaultYBin",
        defaultBinning(southBinning(_, _, _, _, sampling = DefaultSampling)).map(_._2F),
        List("fpu", "grating", "imageQuality", "sourceProfile")
      ),

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

      // ---------------------
      // wavelengthDithers
      // ---------------------

      common.wavelengthDithersString,

      CursorFieldJson(
        "wavelengthDithers",
        cursor =>
          for {
              e <- cursor.field("wavelengthDithersString", None).flatMap(_.as[Option[String]].map(_.map(decodeWavelengthDithers)))
              d <- cursor.field("grating", None).flatMap(_.as[GmosSouthGrating]).map(g => defaultWavelengthDithersSouthJson(g))
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
            .map(g => defaultWavelengthDithersSouthJson(g)),
        List("grating")
      ),

      // ---------------------
      // spatialOffsets
      // ---------------------
      common.spatialOffsetsString,
      common.spatialOffsets,
      common.explicitSpatialOffsets,
      common.defaultSpatialOffsets,

      // ---------------------
      // hidden view fields
      // ---------------------
      common.imageQuality,
      common.sourceProfile,

      // ---------------------
      // initialValues
      // ---------------------

      // We keep up with (read-only) values that were used to create the GMOS LongSlit observing mode initially.
      // Any changes are made via editing `grating`, `filter`, `fpu` and `centralWavelength`.
      SqlField("initialGrating", GmosSouthLongSlitView.InitialGrating),
      SqlField("initialFilter",  GmosSouthLongSlitView.InitialFilter),
      SqlField("initialFpu",     GmosSouthLongSlitView.InitialFpu),
      SqlObject("initialCentralWavelength")
    )

  }
}

object GmosLongSlitMapping {

  private def defaultBinning[U: ClassTag, G: ClassTag](
    f: (U, SourceProfile, ImageQuality, G) => (GmosXBinning, GmosYBinning)
  ): Cursor => Result[(GmosXBinning, GmosYBinning)] = cursor =>
    for {
      fpu <- cursor.fieldAs[U]("fpu")
      iq  <- cursor.fieldAs[ImageQuality]("imageQuality")
      j   <- cursor.fieldAs[Option[Json]]("sourceProfile")
      g   <- cursor.fieldAs[G]("grating")
      sp  <- j.traverse(json => Result.fromEither(json.as[SourceProfile].leftMap(_.message)))
    } yield
      sp.fold((GmosXBinning.One, GmosYBinning.One)) { sourceProfile =>
        f(fpu, sourceProfile, iq, g)
      }

  private def parseCsvBigDecimals(s: String): List[BigDecimal] =
    s.split(',').toList.map(n => BigDecimal(n.trim))

  private def decodeWavelengthDithers(s: String): Json =
    parseCsvBigDecimals(s).map(bd => WavelengthDither.nanometers.unsafeGet(Quantity[Nanometer](bd)).asJson).asJson

  private def defaultWavelengthDithersNorthJson(g: GmosNorthGrating): Json =
    Config.defaultWavelengthDithersNorth(g).map(_.asJson).asJson

  private def defaultWavelengthDithersSouthJson(g: GmosSouthGrating): Json =
    Config.defaultWavelengthDithersSouth(g).map(_.asJson).asJson

  private def decodeSpatialOffsets(s: String): Json =
    parseCsvBigDecimals(s).map(arcsec => Q.signedDecimalArcseconds.reverseGet(arcsec).asJson).asJson

  private val defaultSpatialOffsetsJson: Json =
    Config.DefaultSpatialOffsets.map(_.asJson).asJson

}
