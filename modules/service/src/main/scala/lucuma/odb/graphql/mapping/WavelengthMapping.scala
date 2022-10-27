// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.core.math.Wavelength
import lucuma.odb.graphql.table.GmosLongSlitTable
import lucuma.odb.graphql.table.ObservationView
import lucuma.odb.graphql.util.MappingExtras

trait WavelengthMapping[F[_]]
  extends GmosLongSlitTable[F]
     with ObservationView[F] {

  private def wavelengthMapping(
    idColumn: ColumnRef,
    valueColumn: ColumnRef
  ): ObjectMapping = {

    def fromPm(w: Wavelength, left: Int): BigDecimal =
      BigDecimal(w.toPicometers.value.value).underlying.movePointLeft(left)

    val value = FieldRef[Wavelength]("value")

    ObjectMapping(
      tpe = WavelengthType,
      fieldMappings = List(
        SqlField("synthetic_id", idColumn, key = true, hidden = true),
        SqlField("value", valueColumn, hidden = true),
        value.as("picometers",  _.toPicometers.value.value),
        value.as("angstroms",   fromPm(_, 2)),
        value.as("nanometers",  fromPm(_, 3)),
        value.as("micrometers", fromPm(_, 6))
      )
    )
  }

  import ObservationView.ScienceRequirements.Spectroscopy
  
  lazy val WavelengthMapping: PrefixedMapping =
    PrefixedMapping(
      tpe = WavelengthType,
      mappings = List(
        List("centralWavelength")        -> wavelengthMapping(GmosNorthLongSlitTable.Common.ObservationId, GmosNorthLongSlitTable.Common.CentralWavelength),
        List("initialCentralWavelength") -> wavelengthMapping(GmosNorthLongSlitTable.Common.ObservationId, GmosNorthLongSlitTable.Common.InitialCentralWavelength),
        List("wavelength")               -> wavelengthMapping(ObservationView.Id,                   Spectroscopy.Wavelength),
        List("signalToNoiseAt")          -> wavelengthMapping(ObservationView.Id,                   Spectroscopy.SignalToNoiseAt),
        List("wavelengthCoverage")       -> wavelengthMapping(ObservationView.Id,                   Spectroscopy.WavelengthCoverage)
      )
    )
}
