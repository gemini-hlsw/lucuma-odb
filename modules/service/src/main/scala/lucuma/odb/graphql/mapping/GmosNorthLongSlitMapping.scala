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
import lucuma.core.model.Observation
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.*

import scala.collection.immutable.SortedMap

trait GmosNorthLongSlitMapping[F[_]]
  extends GmosNorthLongSlitTable[F] { this: SkunkMapping[F] =>

  lazy val GmosNorthLongSlitMapping: ObjectMapping =
    ObjectMapping(
      tpe = GmosNorthLongSlitType,
      fieldMappings = List(
        SqlField("observationId", GmosNorthLongSlitTable.ObservationId, key = true, hidden = true),

        SqlField("grating", GmosNorthLongSlitTable.Grating),
        SqlField("filter",  GmosNorthLongSlitTable.Filter),
        SqlField("fpu",     GmosNorthLongSlitTable.Fpu),
        SqlObject("centralWavelength"),
    
        // xBin
        CursorField(
          "xBin",
          cursor =>
            (cursor.field("explicitXBin", None).flatMap(_.as[Option[GmosXBinning]]),
             cursor.field("defaultXBin",  None).flatMap(_.as[GmosXBinning])
            ).parMapN(_.getOrElse(_)),
          List("defaultXBin", "explicitXBin")
        ),

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

        // `yBin` is just the explicitYBin orElse the default
        // `defaultYBin` is a constant (other similar `default` fields will require a calculation based on other values)
        // (optional) `explicitYBin` is tied to a table value and is just a lookup
        FieldRef[Option[GmosYBinning]]("explicitYBin").as("yBin", _.getOrElse(GmosLongSlitMath.DefaultYBinning)),
        CursorField[GmosYBinning]("defaultYBin", _ => Result(GmosLongSlitMath.DefaultYBinning)),
        SqlField("explicitYBin", GmosNorthLongSlitTable.YBin),

        FieldRef[Option[GmosAmpReadMode]]("explicitAmpReadMode").as("ampReadMode", _.getOrElse(GmosLongSlitMath.DefaultAmpReadMode)),
        CursorField[GmosAmpReadMode]("defaultAmpReadMode", _ => Result(GmosLongSlitMath.DefaultAmpReadMode)),
        SqlField("explicitAmpReadMode", GmosNorthLongSlitTable.AmpReadMode),

        FieldRef[Option[GmosAmpGain]]("explicitAmpGain").as("ampGain", _.getOrElse(GmosLongSlitMath.DefaultAmpGain)),
        CursorField[GmosAmpGain]("defaultAmpGain", _ => Result(GmosLongSlitMath.DefaultAmpGain)),
        SqlField("explicitAmpGain", GmosNorthLongSlitTable.AmpGain),

        FieldRef[Option[GmosRoi]]("explicitRoi").as("roi", _.getOrElse(GmosLongSlitMath.DefaultRoi)),
        CursorField[GmosRoi]("defaultRoi", _ => Result(GmosLongSlitMath.DefaultRoi)),
        SqlField("explicitRoi", GmosNorthLongSlitTable.Roi),
        
        // wavelengthDithersNm -- either the explicit value or else the default.
        CursorField(
          "wavelengthDithersNm",
          cursor =>
            (cursor.field("explicitWavelengthDithersNm", None).flatMap(_.as[Option[List[BigDecimal]]]),
             cursor.field("defaultWavelengthDithersNm", None).flatMap(_.as[List[BigDecimal]])
            ).parMapN(_.getOrElse(_)),
          List("defaultWavelengthDithersNm", "explicitWavelengthDithersNm")
        ),
        
        // Default wavelength dithers are based on the grating.
        FieldRef[GmosNorthGrating]("grating")
          .as(
            "defaultWavelengthDithersNm",
            grating => {
              val deltaNm = GmosLongSlitMath.Δλ(Site.GN, grating.dispersion)
              List(GmosLongSlitMath.zeroNm, deltaNm, deltaNm, GmosLongSlitMath.zeroNm).map(_.value)
            }
          ),

        // For explicitWavelengthDithersNm, we have to map the csv string representation to a list of decimals.
        SqlField("wavelengthDithersString", GmosNorthLongSlitTable.WavelengthDithers, hidden = true),
        CursorField[Option[List[BigDecimal]]](
          "explicitWavelengthDithersNm",
          c => c.field("wavelengthDithersString", None).flatMap(_.as[Option[String]].map(_.map(s => s.split(',').toList.map(n => BigDecimal(n.trim))))),
          List("wavelengthDithersString")
        ),
        
        // We keep up with (read-only) values that were used to create the GMOS LongSlit observing mode initially.
        // Any changes are made via editing `grating`, `filter`, `fpu` and `centralWavelength`.
        SqlField("initialGrating", GmosNorthLongSlitTable.InitialGrating),
        SqlField("initialFilter",  GmosNorthLongSlitTable.InitialFilter),
        SqlField("initialFpu",     GmosNorthLongSlitTable.InitialFpu),
        SqlObject("initialCentralWavelength")
      )
    )
}
