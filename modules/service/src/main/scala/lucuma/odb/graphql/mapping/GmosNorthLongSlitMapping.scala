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

    def toOffsetQJson(arcsec: BigDecimal): Json =
      json"""
       {
         "microarcseconds": ${arcsec.bigDecimal.movePointRight(6).setScale(6, RoundingMode.HALF_UP).longValue},
         "milliarcseconds": ${arcsec.bigDecimal.movePointRight(3).setScale(6, RoundingMode.HALF_UP)},
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
        // wavelengthDithersNm
        // ---------------------

        // wavelengthDithersNm -- either the explicit value or else the default.
        explicitOrElseDefault[List[BigDecimal]]("wavelengthDithersNm", "explicitWavelengthDithersNm", "defaultWavelengthDithersNm"),

        // Default wavelength dithers are based on the grating.
        FieldRef[GmosNorthGrating]("grating")
          .as(
            "defaultWavelengthDithersNm",
            grating => GmosLongSlitMath.defaultWavelengthDithersGN(grating).map(_.value)
          ),

        // For explicitWavelengthDithersNm, we have to map the csv string representation to a list of decimals.
        SqlField("wavelengthDithersString", GmosNorthLongSlitTable.WavelengthDithers, hidden = true),
        FieldRef[Option[String]]("wavelengthDithersString")
          .as("explicitWavelengthDithersNm", _.map(parseCsvBigDecimals)),


        SqlField("spatialOffsetsString", GmosNorthLongSlitTable.SpatialOffsets, hidden = true),

        // ---------------------
        // spatialOffsets
        // ---------------------

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
