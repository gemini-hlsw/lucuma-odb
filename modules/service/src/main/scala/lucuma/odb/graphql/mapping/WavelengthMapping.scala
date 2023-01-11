// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.core.math.Wavelength
import lucuma.odb.graphql.table.GmosLongSlitView
import lucuma.odb.graphql.table.ObservationView
import lucuma.odb.graphql.util.MappingExtras

trait WavelengthMapping[F[_]]
  extends GmosLongSlitView[F]
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
  
  lazy val WavelengthMapping: TypeMapping =
    SwitchMapping(
      WavelengthType,
      List(
        GmosNorthLongSlitType / "centralWavelength"                -> wavelengthMapping(GmosNorthLongSlitView.Common.ObservationId, GmosNorthLongSlitView.Common.CentralWavelength),
        GmosNorthLongSlitType / "initialCentralWavelength"         -> wavelengthMapping(GmosNorthLongSlitView.Common.ObservationId, GmosNorthLongSlitView.Common.InitialCentralWavelength),
        GmosSouthLongSlitType / "centralWavelength"                -> wavelengthMapping(GmosSouthLongSlitView.Common.ObservationId, GmosSouthLongSlitView.Common.CentralWavelength),
        GmosSouthLongSlitType / "initialCentralWavelength"         -> wavelengthMapping(GmosSouthLongSlitView.Common.ObservationId, GmosSouthLongSlitView.Common.InitialCentralWavelength),
        SpectroscopyScienceRequirementsType / "wavelength"         -> wavelengthMapping(Spectroscopy.Wavelength.SyntheticId, Spectroscopy.Wavelength.Value),
        SpectroscopyScienceRequirementsType / "signalToNoiseAt"    -> wavelengthMapping(Spectroscopy.SignalToNoiseAt.SyntheticId, Spectroscopy.SignalToNoiseAt.Value),
        SpectroscopyScienceRequirementsType / "wavelengthCoverage" -> wavelengthMapping(ObservationView.Id, Spectroscopy.WavelengthCoverage),
      )
    )
}
