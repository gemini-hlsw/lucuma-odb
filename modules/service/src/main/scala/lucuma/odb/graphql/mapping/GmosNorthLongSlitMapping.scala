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
import lucuma.core.enums.GmosYBinning
import lucuma.core.model.Observation
import lucuma.odb.graphql.predicate.Predicates

import binding.*
import input.*
import table.*

/*
# GMOS North Grating
grating: GmosNorthGrating!

# GMOS North Filter
filter: GmosNorthFilter

# GMOS North FPU
fpu: GmosNorthBuiltinFpu

centralWavelength: Wavelength!

xBin: GmosXBinning!
defaultXBin: GmosXBinning!
explicitXBin: GmosXBinning

yBin: GmosYBinning!
defaultYBin: GmosYBinning!
explicitYBin: GmosYBinning

ampReadMode: GmosAmpReadMode!
defaultAmpReadMode: GmosAmpReadMode!
explicitAmpReadMode: GmosAmpReadMode

ampGain: GmosAmpGain!
defaultAmpGain: GmosAmpGain!
explicitAmpGain: GmosAmpGain

roi: GmosRoi!
defaultRoi: GmosRoi!
explicitRoi: GmosRoi

wavelengthDithersNm: [BigDecimal!]!
defaultWavelengthDithersNm: [BigDecimal!]!
explicitWavelengthDithersNm: [BigDecimal!]

spatialOffsets: [q!]!
defaultSpatialOffsets: [q!]!
explicitSpatialOffsets: [q!]

# The grating as it was initially selected.  See the `grating` field for the
# grating that will be used in the observation.
initialGrating: GmosNorthGrating!

# The filter as it was initially selected (if any).  See the `filter` field
# for the filter that will be used in the observation.
initialFilter: GmosNorthFilter

# The FPU as it was initially selected.  See the `fpu` field for the FPU that
# will be used in the observation.
initialFPU: GmosNorthBuiltinFpu!

initialCentralWavelength: Wavelength!
*/

trait GmosNorthLongSlitMapping[F[_]]
  extends GmosNorthLongSlitTable[F] { this: SkunkMapping[F] =>

  private val DefaultYBinning: GmosYBinning =
    GmosYBinning.Two

  lazy val GmosNorthLongSlitMapping: ObjectMapping =
    ObjectMapping(
      tpe = GmosNorthLongSlitType,
      fieldMappings = List(
        SqlField("observationId", GmosNorthLongSlitTable.ObservationId, key = true, hidden = true),

        SqlField("grating", GmosNorthLongSlitTable.Grating),
        SqlField("filter", GmosNorthLongSlitTable.Filter),
        SqlField("fpu", GmosNorthLongSlitTable.Fpu),
        SqlObject("centralWavelength"),
    
        // xBin
        // defaultXBin
        SqlField("explicitXBin", GmosNorthLongSlitTable.XBin),

        // (optional) `explicitYBin` is tied to a table value and is just a lookup
        // `defaultYBin` is a constant (other similar `default` fields will require a calculation based on other values)
        // `yBin` is just the explicitYBin orElse the default
        FieldRef[Option[GmosYBinning]]("explicitYBin").as("yBin", _.getOrElse(DefaultYBinning)),
        CursorField[GmosYBinning]("defaultYBin", _ => Result(DefaultYBinning)),
        SqlField("explicitYBin", GmosNorthLongSlitTable.YBin),

        SqlField("initialGrating", GmosNorthLongSlitTable.InitialGrating),
        SqlField("initialFilter",  GmosNorthLongSlitTable.InitialFilter),
        SqlField("initialFpu",     GmosNorthLongSlitTable.InitialFpu),
        SqlObject("initialCentralWavelength")
      )
    )
}
