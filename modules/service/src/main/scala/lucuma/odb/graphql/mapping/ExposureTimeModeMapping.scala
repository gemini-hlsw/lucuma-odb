// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import grackle.Path
import grackle.TypeRef
import lucuma.odb.graphql.table.ExposureTimeModeView


trait ExposureTimeModeMapping[F[_]] extends ExposureTimeModeView[F]:

  /**
   * Maps an `ExposureTimeMode` object at `base / fieldName`.
   *
   * When `explicitOnly` is set the object and its two variants are keyed on the view's
   * `c_explicit_*` columns, which are null unless the user set the mode.  A null key
   * yields a null object, so a derived row reports `explicitExposureTimeMode: null` while
   * its sibling `exposureTimeMode` still resolves to the effective value from the very
   * same row.
   */
  def etmMappings(
    base:         Path,
    view:         BaseExposureTimeModeView,
    fieldName:    String  = "exposureTimeMode",
    explicitOnly: Boolean = false
  ): List[ObjectMapping] =
    val path = base / fieldName

    val etmKey: ColumnRef = if explicitOnly then view.ExplicitId else view.Id
    val snKey:  ColumnRef = if explicitOnly then view.SignalToNoise.ExplicitSyntheticId else view.SignalToNoise.SyntheticId
    val tacKey: ColumnRef = if explicitOnly then view.TimeAndCount.ExplicitSyntheticId else view.TimeAndCount.SyntheticId

    val TimeAndCount: ObjectMapping =
      ObjectMapping(path / "timeAndCount")(
        SqlField("id", tacKey, key = true, hidden = true),
        SqlObject("time"),
        SqlField("count", view.TimeAndCount.Count),
        SqlObject("at")
      )

    val SignalToNoise: ObjectMapping =
      ObjectMapping(path / "signalToNoise")(
        SqlField("id", snKey, key = true, hidden = true),
        SqlField("value", view.SignalToNoise.Value),
        SqlObject("at")
      )

    val ExposureTimeMode: ObjectMapping =
      ObjectMapping(path)(
        SqlField("id",   etmKey, key = true, hidden = true),
        SqlField("role", view.Role, hidden = true),
        SqlObject("signalToNoise"),
        SqlObject("timeAndCount")
      )

    List(TimeAndCount, SignalToNoise, ExposureTimeMode)

  def etmMappings(
    typeRef:      TypeRef,
    view:         BaseExposureTimeModeView,
    fieldName:    String,
    explicitOnly: Boolean
  ): List[ObjectMapping] =
    etmMappings(Path.from(typeRef), view, fieldName, explicitOnly)

  def etmMappings(
    typeRef: TypeRef,
    view:    BaseExposureTimeModeView
  ): List[ObjectMapping] =
    etmMappings(Path.from(typeRef), view)

  lazy val ExposureTimeModeMappings: List[ObjectMapping] =
    List(
      // Flamingos 2
      etmMappings(Flamingos2ImagingFilterType,       ExposureTimeModeView),
      etmMappings(GnirsImagingFilterType,            ExposureTimeModeView),
      etmMappings(GnirsImagingAcquisitionType,       ExposureTimeModeView),
      etmMappings(Flamingos2LongSlitType,            ExposureTimeModeView),
      etmMappings(Flamingos2MosType,                 ExposureTimeModeView),
      etmMappings(Flamingos2LongSlitAcquisitionType, ExposureTimeModeView),
      etmMappings(Flamingos2MosAcquisitionType,      ExposureTimeModeView),

      // Ghost
      etmMappings(GhostIfuType / "blue",  GhostBlueExposureTimeModeView),
      etmMappings(GhostIfuType / "red",   GhostRedExposureTimeModeView),

      // GmosNorth
      etmMappings(GmosNorthImagingFilterType,       ExposureTimeModeView),
      etmMappings(GmosNorthLongSlitType,            ExposureTimeModeView),
      etmMappings(GmosNorthLongSlitAcquisitionType, ExposureTimeModeView),
      etmMappings(GmosNorthIfuAcquisitionType,      ExposureTimeModeView),
      etmMappings(GmosNorthIfuType,                 ExposureTimeModeView),
      etmMappings(GmosSouthIfuAcquisitionType,      ExposureTimeModeView),
      etmMappings(GmosSouthIfuType,                 ExposureTimeModeView),
      etmMappings(GmosNorthMosAcquisitionType,      ExposureTimeModeView),
      etmMappings(GmosNorthMosType,                 ExposureTimeModeView),

      // GmosSouth
      etmMappings(GmosSouthImagingFilterType,       ExposureTimeModeView),
      etmMappings(GmosSouthLongSlitType,            ExposureTimeModeView),
      etmMappings(GmosSouthLongSlitAcquisitionType, ExposureTimeModeView),
      etmMappings(GmosSouthMosAcquisitionType,      ExposureTimeModeView),
      etmMappings(GmosSouthMosType,                 ExposureTimeModeView),

      // GNIRS.  The two acquisition types expose both the effective exposure time mode
      // and the explicit override, which are the same row read two ways.
      etmMappings(GnirsCentralWavelengthConfigType,  ExposureTimeModeView),
      etmMappings(GnirsSpectroscopyAcquisitionType, ExposureTimeModeView),
      etmMappings(GnirsSpectroscopyAcquisitionType, ExposureTimeModeView, "explicitExposureTimeMode", true),
      etmMappings(GnirsImagingAcquisitionType,      ExposureTimeModeView, "explicitExposureTimeMode", true),

      // IGRINS2
      etmMappings(Igrins2LongSlitType, ExposureTimeModeView),

      // Science Requirements
      etmMappings(ScienceRequirementsType, ExposureTimeModeView)
    ).flatten
