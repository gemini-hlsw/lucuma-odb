// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path
import grackle.skunk.SkunkMapping
import lucuma.core.math.Wavelength
import lucuma.odb.graphql.table.ChronConditionsEntryView
import lucuma.odb.graphql.table.ExposureTimeModeView
import lucuma.odb.graphql.table.Flamingos2DynamicView
import lucuma.odb.graphql.table.GmosDynamicTables
import lucuma.odb.graphql.table.GmosLongSlitView
import lucuma.odb.graphql.table.ObservationView
import lucuma.odb.graphql.table.SpectroscopyConfigOptionTable

trait WavelengthMapping[F[_]]
  extends GmosLongSlitView[F]
     with ChronConditionsEntryView[F]
     with ExposureTimeModeView[F]
     with Flamingos2DynamicView[F]
     with GmosDynamicTables[F]
     with ObservationView[F]
     with SpectroscopyConfigOptionTable[F]:

  private def wavelengthMappingAtPath(
    path: Path,
    valueColumn: ColumnRef,
    idColumns: ColumnRef*
  ): ObjectMapping =

    def fromPm(w: Wavelength, left: Int): BigDecimal =
      BigDecimal(w.toPicometers.value.value).underlying.movePointLeft(left)

    val value = FieldRef[Wavelength]("value")

    ObjectMapping(path)(
      (idColumns.toList.zipWithIndex.map { (ref, idx) =>
        SqlField(s"synthetic_id$idx", ref, key = true, hidden = true)
      }) ++
      List(
        SqlField("value", valueColumn, hidden = true),
        value.as("picometers", _.toPicometers.value.value),
        value.as("angstroms", fromPm(_, 2)),
        value.as("nanometers", fromPm(_, 3)),
        value.as("micrometers", fromPm(_, 6))
      )*
    )

  import ObservationView.ScienceRequirements.Spectroscopy

  lazy val WavelengthMappings: List[TypeMapping] =
    List(
      wavelengthMappingAtPath(ConditionsMeasurementType / "wavelength", ChronConditionsEntryView.Measurement.Wavelength.Value, ChronConditionsEntryView.Measurement.Wavelength.SyntheticId),
      wavelengthMappingAtPath(GmosNorthLongSlitType / "centralWavelength", GmosNorthLongSlitView.Common.CentralWavelength, GmosNorthLongSlitView.Common.ObservationId),
      wavelengthMappingAtPath(GmosNorthLongSlitType / "initialCentralWavelength", GmosNorthLongSlitView.Common.InitialCentralWavelength, GmosNorthLongSlitView.Common.ObservationId),
      wavelengthMappingAtPath(GmosSouthLongSlitType / "centralWavelength", GmosSouthLongSlitView.Common.CentralWavelength, GmosSouthLongSlitView.Common.ObservationId),
      wavelengthMappingAtPath(GmosSouthLongSlitType / "initialCentralWavelength", GmosSouthLongSlitView.Common.InitialCentralWavelength, GmosSouthLongSlitView.Common.ObservationId),
      wavelengthMappingAtPath(SignalToNoiseExposureTimeModeType / "at", ExposureTimeModeView.SignalToNoise.At, ExposureTimeModeView.SignalToNoise.SyntheticId),
      wavelengthMappingAtPath(SpectroscopyConfigOptionType / "wavelengthMin", SpectroscopyConfigOptionTable.WavelengthMin, SpectroscopyConfigOptionTable.Instrument, SpectroscopyConfigOptionTable.Index),
      wavelengthMappingAtPath(SpectroscopyConfigOptionType / "wavelengthMax", SpectroscopyConfigOptionTable.WavelengthMax, SpectroscopyConfigOptionTable.Instrument, SpectroscopyConfigOptionTable.Index),
      wavelengthMappingAtPath(SpectroscopyConfigOptionType / "wavelengthOptimal", SpectroscopyConfigOptionTable.WavelengthOptimal, SpectroscopyConfigOptionTable.Instrument, SpectroscopyConfigOptionTable.Index),
      wavelengthMappingAtPath(SpectroscopyConfigOptionType / "wavelengthCoverage", SpectroscopyConfigOptionTable.WavelengthCoverage, SpectroscopyConfigOptionTable.Instrument, SpectroscopyConfigOptionTable.Index),
      wavelengthMappingAtPath(SpectroscopyScienceRequirementsType / "wavelength", Spectroscopy.Wavelength.Value, Spectroscopy.Wavelength.SyntheticId),
      wavelengthMappingAtPath(SpectroscopyScienceRequirementsType / "wavelengthCoverage", Spectroscopy.WavelengthCoverage.Value, Spectroscopy.WavelengthCoverage.SyntheticId),
      wavelengthMappingAtPath(StepRecordType / "flamingos2" / "centralWavelength", Flamingos2DynamicView.CentralWavelength, Flamingos2DynamicView.Id),
      wavelengthMappingAtPath(StepRecordType / "gmosNorth" / "centralWavelength", GmosNorthDynamicTable.CentralWavelength.Value, GmosNorthDynamicTable.CentralWavelength.SyntheticId),
      wavelengthMappingAtPath(StepRecordType / "gmosSouth" / "centralWavelength", GmosSouthDynamicTable.CentralWavelength.Value, GmosSouthDynamicTable.CentralWavelength.SyntheticId),
      wavelengthMappingAtPath(StepRecordType / "gmosNorth" / "gratingConfig" / "wavelength", GmosNorthDynamicTable.Grating.Wavelength, GmosNorthDynamicTable.Id),
      wavelengthMappingAtPath(StepRecordType / "gmosSouth" / "gratingConfig" / "wavelength", GmosSouthDynamicTable.Grating.Wavelength, GmosSouthDynamicTable.Id),
      wavelengthMappingAtPath(TimeAndCountExposureTimeModeType / "at", ExposureTimeModeView.TimeAndCount.At, ExposureTimeModeView.TimeAndCount.SyntheticId)
    )
