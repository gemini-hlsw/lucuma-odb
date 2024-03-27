// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.skunk.SkunkMapping
import lucuma.core.math.Wavelength
import lucuma.odb.graphql.table.ChronConditionsEntryView
import lucuma.odb.graphql.table.GmosDynamicTables
import lucuma.odb.graphql.table.GmosLongSlitView
import lucuma.odb.graphql.table.ObservationView
import lucuma.odb.graphql.table.SpectroscopyConfigOptionTable

trait WavelengthMapping[F[_]]
  extends GmosLongSlitView[F]
     with ChronConditionsEntryView[F]
     with GmosDynamicTables[F]
     with ObservationView[F]
     with SpectroscopyConfigOptionTable[F] {

  private def wavelengthMapping(
    valueColumn: ColumnRef,
    idColumns: ColumnRef*
  ): ObjectMapping = {

    def fromPm(w: Wavelength, left: Int): BigDecimal =
      BigDecimal(w.toPicometers.value.value).underlying.movePointLeft(left)

    val value = FieldRef[Wavelength]("value")

    ObjectMapping(
      tpe = WavelengthType,
      fieldMappings =
        (idColumns.toList.zipWithIndex.map { (ref, idx) =>
          SqlField(s"synthetic_id$idx", ref, key = true, hidden = true)
        }) ++
        List(
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
        ConditionsMeasurementType / "wavelength"                      ->
          wavelengthMapping(ChronConditionsEntryView.Measurement.Wavelength.Value, ChronConditionsEntryView.Measurement.Wavelength.SyntheticId),
        GmosNorthLongSlitType / "centralWavelength"                   ->
          wavelengthMapping(GmosNorthLongSlitView.Common.CentralWavelength, GmosNorthLongSlitView.Common.ObservationId),
        GmosNorthLongSlitType / "initialCentralWavelength"            ->
          wavelengthMapping(GmosNorthLongSlitView.Common.InitialCentralWavelength, GmosNorthLongSlitView.Common.ObservationId),
        GmosSouthLongSlitType / "centralWavelength"                   ->
          wavelengthMapping(GmosSouthLongSlitView.Common.CentralWavelength, GmosSouthLongSlitView.Common.ObservationId),
        GmosSouthLongSlitType / "initialCentralWavelength"            ->
          wavelengthMapping(GmosSouthLongSlitView.Common.InitialCentralWavelength, GmosSouthLongSlitView.Common.ObservationId),
        SpectroscopyConfigOptionType / "wavelengthMin"                ->
          wavelengthMapping(SpectroscopyConfigOptionTable.WavelengthMin, SpectroscopyConfigOptionTable.Instrument, SpectroscopyConfigOptionTable.Index),
        SpectroscopyConfigOptionType / "wavelengthMax"                ->
          wavelengthMapping(SpectroscopyConfigOptionTable.WavelengthMax, SpectroscopyConfigOptionTable.Instrument, SpectroscopyConfigOptionTable.Index),
        SpectroscopyConfigOptionType / "wavelengthOptimal"            ->
          wavelengthMapping(SpectroscopyConfigOptionTable.WavelengthOptimal, SpectroscopyConfigOptionTable.Instrument, SpectroscopyConfigOptionTable.Index),
        SpectroscopyConfigOptionType / "wavelengthCoverage"           ->
          wavelengthMapping(SpectroscopyConfigOptionTable.WavelengthCoverage, SpectroscopyConfigOptionTable.Instrument, SpectroscopyConfigOptionTable.Index),
        SpectroscopyScienceRequirementsType / "wavelength"            ->
          wavelengthMapping(Spectroscopy.Wavelength.Value, Spectroscopy.Wavelength.SyntheticId),
        SpectroscopyScienceRequirementsType / "signalToNoiseAt"       ->
          wavelengthMapping(Spectroscopy.SignalToNoiseAt.Value, Spectroscopy.SignalToNoiseAt.SyntheticId),
        SpectroscopyScienceRequirementsType / "wavelengthCoverage"    ->
          wavelengthMapping(Spectroscopy.WavelengthCoverage.Value, Spectroscopy.WavelengthCoverage.SyntheticId),
        StepRecordType / "gmosNorth" / "gratingConfig" / "wavelength" ->
          wavelengthMapping(GmosNorthDynamicTable.Grating.Wavelength, GmosNorthDynamicTable.Id),
        StepRecordType / "gmosSouth" / "gratingConfig" / "wavelength" ->
          wavelengthMapping(GmosSouthDynamicTable.Grating.Wavelength, GmosSouthDynamicTable.Id)
      )
    )
}
