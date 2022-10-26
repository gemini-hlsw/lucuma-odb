// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.syntax.all.*
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
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.ImageQuality
import lucuma.core.enums.Site
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.math.Offset.Q
import lucuma.core.model.Observation
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.*

import java.math.RoundingMode
import scala.collection.immutable.SortedMap
import scala.reflect.ClassTag

trait GmosNorthLongSlitMapping[F[_]]
  extends GmosNorthLongSlitTable[F] { this: SkunkMapping[F] =>

  lazy val GmosNorthLongSlitMapping: ObjectMapping = {

    def explicitOrElseDefault[A: ClassTag : io.circe.Encoder](
      name:          String,
      explicitField: String,
      defaultField:  String
    ): CursorField[A] =
      CursorField[A](
        name,
        cursor =>
          (cursor.field(explicitField, None).flatMap(_.as[Option[A]]),
           cursor.field(defaultField, None).flatMap(_.as[A])
          ).parMapN(_.getOrElse(_)),
        List(explicitField, defaultField)
      )

    def parseCsvBigDecimals(s: String): List[BigDecimal] =
      s.split(',').toList.map(n => BigDecimal(n.trim))

    def toWavelengthDitherJson(nm: BigDecimal): Json =
      json"""
       {
         "picometers":  ${nm.bigDecimal.movePointRight(3).setScale(0, RoundingMode.HALF_UP).longValue},
         "angstroms":   ${nm.bigDecimal.movePointRight(1).setScale(2, RoundingMode.HALF_UP)},
         "nanometers":  ${nm.bigDecimal.setScale(3, RoundingMode.HALF_UP)},
         "micrometers": ${nm.bigDecimal.movePointLeft(3).setScale(6, RoundingMode.HALF_UP)}
       }
      """

    def decodeWavelengthDithers(s: String): Json =
      parseCsvBigDecimals(s).map(toWavelengthDitherJson).asJson

    def defaultWavelengthDithers(g: GmosNorthGrating): Json =
      GmosLongSlitMath.defaultWavelengthDithersGN(g).map(q => toWavelengthDitherJson(q.value)).asJson

    def toOffsetQJson(arcsec: BigDecimal): Json =
      json"""
       {
         "microarcseconds": ${arcsec.bigDecimal.movePointRight(6).setScale(0, RoundingMode.HALF_UP).longValue},
         "milliarcseconds": ${arcsec.bigDecimal.movePointRight(3).setScale(3, RoundingMode.HALF_UP)},
         "arcseconds":      ${arcsec.bigDecimal.setScale(6, RoundingMode.HALF_UP)}
       }
    """

    def decodeSpatialOffsets(s: String): Json =
      parseCsvBigDecimals(s).map(toOffsetQJson).asJson

    val defaultSpatialOffsets: Json =
      GmosLongSlitMath.DefaultSpatialOffsets.map(q => toOffsetQJson(q.value)).asJson

    ObjectMapping(
      tpe = GmosNorthLongSlitType,
      fieldMappings = List(
        SqlField("observationId", GmosNorthLongSlitTable.ObservationId, key = true, hidden = true),

        SqlField("grating", GmosNorthLongSlitTable.Grating),
        SqlField("filter",  GmosNorthLongSlitTable.Filter),
        SqlField("fpu",     GmosNorthLongSlitTable.Fpu),
        SqlObject("centralWavelength"),

        // ---------------------
        // xBin
        // ---------------------

        explicitOrElseDefault[GmosXBinning]("xBin", "explicitXBin", "defaultXBin"),

        // TODO: a function of FPU, target SourceProfile, and ImageQuality
        FieldRef[GmosNorthFpu]("fpu")
          .as(
            "defaultXBin",
            fpu => GmosLongSlitMath.xbinNorth(
              fpu,
              /*placeholder*/ SourceProfile.Point(SpectralDefinition.BandNormalized(UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.O5V), SortedMap.empty)),
              /*placeholder*/ ImageQuality.TwoPointZero,  // so the FPU size is the effective slit width for now
              PosDouble.unsafeFrom(2.0)
            )
          ),

        // (optional) `explicitXBin` is tied to a table value and is just a lookup
        SqlField("explicitXBin", GmosNorthLongSlitTable.XBin),

        // ---------------------
        // yBin
        // ---------------------

        // `yBin` is just the explicitYBin orElse the default
        // `defaultYBin` is a constant (other similar `default` fields will require a calculation based on other values)
        // (optional) `explicitYBin` is tied to a table value and is just a lookup
        explicitOrElseDefault[GmosYBinning]("yBin", "explicitYBin", "defaultYBin"),
        CursorField[GmosYBinning]("defaultYBin", _ => Result(GmosLongSlitMath.DefaultYBinning)),
        SqlField("explicitYBin", GmosNorthLongSlitTable.YBin),

        // ---------------------
        // ampReadMode
        // ---------------------
        explicitOrElseDefault[GmosAmpReadMode]("ampReadMode", "explicitAmpReadMode", "defaultAmpReadMode"),
        CursorField[GmosAmpReadMode]("defaultAmpReadMode", _ => Result(GmosLongSlitMath.DefaultAmpReadMode)),
        SqlField("explicitAmpReadMode", GmosNorthLongSlitTable.AmpReadMode),

        // ---------------------
        // ampGain
        // ---------------------
        explicitOrElseDefault[GmosAmpGain]("ampGain", "explicitAmpGain", "defaultAmpGain"),
        CursorField[GmosAmpGain]("defaultAmpGain", _ => Result(GmosLongSlitMath.DefaultAmpGain)),
        SqlField("explicitAmpGain", GmosNorthLongSlitTable.AmpGain),

        // ---------------------
        // roi
        // ---------------------
        explicitOrElseDefault[GmosRoi]("roi", "explicitRoi", "defaultRoi"),
        CursorField[GmosRoi]("defaultRoi", _ => Result(GmosLongSlitMath.DefaultRoi)),
        SqlField("explicitRoi", GmosNorthLongSlitTable.Roi),

        // ---------------------
        // wavelengthDithers
        // ---------------------

        SqlField("wavelengthDithersString", GmosNorthLongSlitTable.WavelengthDithers, hidden = true),

        CursorFieldJson(
          "wavelengthDithers",
          cursor =>
            for {
               e <- cursor.field("wavelengthDithersString", None).flatMap(_.as[Option[String]].map(_.map(decodeWavelengthDithers)))
               d <- cursor.field("grating", None).flatMap(_.as[GmosNorthGrating]).map(g => defaultWavelengthDithers(g))
            } yield e.getOrElse(d),
          List("wavelengthDithersString", "grating")
        ),

        CursorFieldJson(
          "explicitWavelengthDithers",
          cursor =>
            cursor
              .field("wavelengthDithersString", None)
              .flatMap(_.as[Option[String]].map(_.map(decodeWavelengthDithers).asJson)),
          List("wavelengthDithersString")
        ),

        CursorFieldJson(
          "defaultWavelengthDithers",
          cursor =>
            cursor
              .field("grating", None)
              .flatMap(_.as[GmosNorthGrating])
              .map(g => defaultWavelengthDithers(g)),
          List("grating")
        ),

        // ---------------------
        // spatialOffsets
        // ---------------------

        SqlField("spatialOffsetsString", GmosNorthLongSlitTable.SpatialOffsets, hidden = true),

        CursorFieldJson("spatialOffsets",
          cursor =>
            cursor
              .field("spatialOffsetsString", None)
              .flatMap(_.as[Option[String]].map(_.map(decodeSpatialOffsets)))
              .map(_.getOrElse(defaultSpatialOffsets)),
          List("explicitSpatialOffsets", "defaultSpatialOffsets")
        ),

        CursorFieldJson("explicitSpatialOffsets",
          cursor =>
            cursor
              .field("spatialOffsetsString", None)
              .flatMap(_.as[Option[String]].map(_.map(decodeSpatialOffsets).asJson)),
          List("spatialOffsetsString")
        ),

        CursorFieldJson("defaultSpatialOffsets", _ => Result(defaultSpatialOffsets), Nil),

        // ---------------------
        // initialValues
        // ---------------------

        // We keep up with (read-only) values that were used to create the GMOS LongSlit observing mode initially.
        // Any changes are made via editing `grating`, `filter`, `fpu` and `centralWavelength`.
        SqlField("initialGrating", GmosNorthLongSlitTable.InitialGrating),
        SqlField("initialFilter",  GmosNorthLongSlitTable.InitialFilter),
        SqlField("initialFpu",     GmosNorthLongSlitTable.InitialFpu),
        SqlObject("initialCentralWavelength")
      )
    )
  }



}
