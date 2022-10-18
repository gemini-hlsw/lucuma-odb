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
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosYBinning
import lucuma.core.model.Observation
import lucuma.odb.graphql.predicate.Predicates

import binding.*
import input.*
import table.*

trait GmosNorthLongSlitMapping[F[_]]
  extends GmosNorthLongSlitTable[F] { this: SkunkMapping[F] =>

  private val DefaultAmpReadMode: GmosAmpReadMode =
    GmosAmpReadMode.Slow
    
  private val DefaultAmpGain: GmosAmpGain =
    GmosAmpGain.Low
    
  private val DefaultRoi: GmosRoi =
    GmosRoi.FullFrame  

  private val DefaultYBinning: GmosYBinning =
    GmosYBinning.Two

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
//        FieldRef[Option[GmosYBinning]]("explicitXBin").as("yBin", _.getOrElse(defaultXBin)),

        // defaultXBin
        // TODO: a function of FPU, target SourceProfile, and ImageQuality

        // (optional) `explicitXBin` is tied to a table value and is just a lookup
        SqlField("explicitXBin", GmosNorthLongSlitTable.XBin),

        // `yBin` is just the explicitYBin orElse the default
        // `defaultYBin` is a constant (other similar `default` fields will require a calculation based on other values)
        // (optional) `explicitYBin` is tied to a table value and is just a lookup
        FieldRef[Option[GmosYBinning]]("explicitYBin").as("yBin", _.getOrElse(DefaultYBinning)),
        CursorField[GmosYBinning]("defaultYBin", _ => Result(DefaultYBinning)),
        SqlField("explicitYBin", GmosNorthLongSlitTable.YBin),

        FieldRef[Option[GmosAmpReadMode]]("explicitAmpReadMode").as("ampReadMode", _.getOrElse(DefaultAmpReadMode)),
        CursorField[GmosAmpReadMode]("defaultAmpReadMode", _ => Result(DefaultAmpReadMode)),
        SqlField("explicitAmpReadMode", GmosNorthLongSlitTable.AmpReadMode),

        FieldRef[Option[GmosAmpGain]]("explicitAmpGain").as("ampGain", _.getOrElse(DefaultAmpGain)),
        CursorField[GmosAmpGain]("defaultAmpGain", _ => Result(DefaultAmpGain)),
        SqlField("explicitAmpGain", GmosNorthLongSlitTable.AmpGain),

        FieldRef[Option[GmosRoi]]("explicitRoi").as("roi", _.getOrElse(DefaultRoi)),
        CursorField[GmosRoi]("defaultRoi", _ => Result(DefaultRoi)),
        SqlField("explicitRoi", GmosNorthLongSlitTable.Roi),
        
        // We keep up with (read-only) values that were used to create the GMOS LongSlit observing mode initially.
        // Any changes are made via editing `grating`, `filter`, `fpu` and `centralWavelength`.
        SqlField("initialGrating", GmosNorthLongSlitTable.InitialGrating),
        SqlField("initialFilter",  GmosNorthLongSlitTable.InitialFilter),
        SqlField("initialFpu",     GmosNorthLongSlitTable.InitialFpu),
        SqlObject("initialCentralWavelength")
      )
    )
}
