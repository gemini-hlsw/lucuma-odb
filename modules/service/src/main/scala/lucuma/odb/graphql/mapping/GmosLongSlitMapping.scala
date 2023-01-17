// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import cats.syntax.all.*
import coulomb.*
import edu.gemini.grackle.Mapping
import edu.gemini.grackle.Path
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate.*
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query.*
import edu.gemini.grackle.Result
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.skunk.SkunkMapping
import eu.timepit.refined.types.numeric.PosDouble
import io.circe.Json
import io.circe.literal.*
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
import lucuma.core.enums.Site
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.math.Offset.Q
import lucuma.core.math.WavelengthDither
import lucuma.core.math.units.Nanometer
import lucuma.core.model.Observation
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.*
import lucuma.odb.json.angle.query.given
import lucuma.odb.json.sourceprofile.given
import lucuma.odb.json.wavelength.query.given
import lucuma.odb.sequence.gmos.longslit.GmosLongSlitConfig

import java.math.RoundingMode
import scala.collection.immutable.SortedMap
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
    val defaultYBin: FieldMapping         = CursorField[GmosYBinning]("defaultYBin", _ => Result(GmosLongSlitConfig.DefaultYBinning))
    val explicitYBin: FieldMapping        = SqlField("explicitYBin", cc.YBin)

    val ampReadMode: FieldMapping         = explicitOrElseDefault[GmosAmpReadMode]("ampReadMode", "explicitAmpReadMode", "defaultAmpReadMode")
    val defaultAmpReadMode: FieldMapping  = CursorField[GmosAmpReadMode]("defaultAmpReadMode", _ => Result(GmosLongSlitConfig.DefaultAmpReadMode))
    val explicitAmpReadMode: FieldMapping = SqlField("explicitAmpReadMode", cc.AmpReadMode)

    val ampGain: FieldMapping             = explicitOrElseDefault[GmosAmpGain]("ampGain", "explicitAmpGain", "defaultAmpGain")
    val defaultAmpGain: FieldMapping      = CursorField[GmosAmpGain]("defaultAmpGain", _ => Result(GmosLongSlitConfig.DefaultAmpGain))
    val explicitAmpGain: FieldMapping     = SqlField("explicitAmpGain", cc.AmpGain)

    val roi: FieldMapping                 = explicitOrElseDefault[GmosRoi]("roi", "explicitRoi", "defaultRoi")
    val defaultRoi: FieldMapping          = CursorField[GmosRoi]("defaultRoi", _ => Result(GmosLongSlitConfig.DefaultRoi))
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

    ObjectMapping(
      tpe = GmosNorthLongSlitType,
      fieldMappings = List(
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
          cursor =>
            for {
              fpu <- cursor.fieldAs[GmosNorthFpu]("fpu")
              iq  <- cursor.fieldAs[ImageQuality]("imageQuality")
              j   <- cursor.fieldAs[Option[Json]]("sourceProfile")
              sp  <- j.traverse(json => Result.fromEither(json.as[SourceProfile].leftMap(_.message)))
            } yield
              sp.fold(GmosXBinning.Two) { sourceProfile =>  // TODO: What should the real default be if there is no target
                GmosLongSlitConfig.xbinNorth(fpu, sourceProfile, iq, PosDouble.unsafeFrom(2.0))
              },
          List("fpu", "imageQuality", "sourceProfile")
        ),

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
    )
  }

  lazy val GmosSouthLongSlitMapping: ObjectMapping = {

    import GmosLongSlitMapping._

    val common = new CommonFieldMappings(GmosSouthLongSlitView.Common)

    ObjectMapping(
      tpe = GmosSouthLongSlitType,
      fieldMappings = List(
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
          cursor =>
            for {
              fpu <- cursor.fieldAs[GmosSouthFpu]("fpu")
              iq  <- cursor.fieldAs[ImageQuality]("imageQuality")
              j   <- cursor.fieldAs[Option[Json]]("sourceProfile")
              sp  <- j.traverse(json => Result.fromEither(json.as[SourceProfile].leftMap(_.message)))
            } yield
              sp.fold(GmosXBinning.Two) { sourceProfile =>
                GmosLongSlitConfig.xbinSouth(fpu, sourceProfile, iq, PosDouble.unsafeFrom(2.0))
              },
          List("fpu", "imageQuality", "sourceProfile")
        ),

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
    )
  }
}

object GmosLongSlitMapping {

  private def parseCsvBigDecimals(s: String): List[BigDecimal] =
    s.split(',').toList.map(n => BigDecimal(n.trim))

  private def toWavelengthDitherJson(wd: WavelengthDither): Json = {
    val pm: Int = wd.toPicometers.value

    json"""
      {
        "picometers":  $pm,
        "angstroms":   ${BigDecimal(pm, 2)},
        "nanometers":  ${BigDecimal(pm, 3)},
        "micrometers": ${BigDecimal(pm, 6)}
     }
    """
  }

  private def decodeWavelengthDithers(s: String): Json =
    parseCsvBigDecimals(s).map(bd => toWavelengthDitherJson(WavelengthDither.nanometers.unsafeGet(Quantity[Nanometer](bd)))).asJson

  private def defaultWavelengthDithersNorthJson(g: GmosNorthGrating): Json =
    GmosLongSlitConfig.defaultWavelengthDithersNorth(g).map(q => toWavelengthDitherJson(q)).asJson

  private def defaultWavelengthDithersSouthJson(g: GmosSouthGrating): Json =
    GmosLongSlitConfig.defaultWavelengthDithersSouth(g).map(q => toWavelengthDitherJson(q)).asJson

  private def toOffsetQJson(q: Q): Json = {
    val micro: Long =
      Q.signedDecimalArcseconds
       .get(q)
       .bigDecimal
       .movePointRight(6)
       .setScale(0, RoundingMode.HALF_UP)
       .longValue

    json"""
     {
       "microarcseconds": $micro,
       "milliarcseconds": ${BigDecimal(micro, 3)},
       "arcseconds":      ${BigDecimal(micro, 6)}
     }
    """
  }

  private def decodeSpatialOffsets(s: String): Json =
    parseCsvBigDecimals(s).map(arcsec => toOffsetQJson(Q.signedDecimalArcseconds.reverseGet(arcsec))).asJson

  private val defaultSpatialOffsetsJson: Json =
    GmosLongSlitConfig.DefaultSpatialOffsets.map(q => toOffsetQJson(q)).asJson

}
