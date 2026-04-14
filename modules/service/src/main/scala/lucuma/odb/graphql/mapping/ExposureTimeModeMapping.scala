// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import grackle.Path
import grackle.TypeRef
import lucuma.odb.graphql.table.ExposureTimeModeView


trait ExposureTimeModeMapping[F[_]] extends ExposureTimeModeView[F]:

  def etmMappings(
    base: Path,
    view: BaseExposureTimeModeView
  ): List[ObjectMapping] =
    val path = base / "exposureTimeMode"

    val TimeAndCount: ObjectMapping =
      ObjectMapping(path / "timeAndCount")(
        SqlField("id", view.TimeAndCount.SyntheticId, key = true, hidden = true),
        SqlObject("time"),
        SqlField("count", view.TimeAndCount.Count),
        SqlObject("at")
      )

    val SignalToNoise: ObjectMapping =
      ObjectMapping(path / "signalToNoise")(
        SqlField("id", view.SignalToNoise.SyntheticId, key = true, hidden = true),
        SqlField("value", view.SignalToNoise.Value),
        SqlObject("at")
      )

    val ExposureTimeMode: ObjectMapping =
      ObjectMapping(path)(
        SqlField("id",   view.Id, key = true, hidden = true),
        SqlField("role", view.Role, hidden = true),
        SqlObject("signalToNoise"),
        SqlObject("timeAndCount")
      )

    List(TimeAndCount, SignalToNoise, ExposureTimeMode)

  def etmMappings(
    typeRef: TypeRef,
    view:    BaseExposureTimeModeView
  ): List[ObjectMapping] =
    etmMappings(Path.from(typeRef), view)

  lazy val ExposureTimeModeMappings: List[ObjectMapping] =
    List(
      etmMappings(Flamingos2LongSlitType, ExposureTimeModeView),
      etmMappings(Flamingos2LongSlitAcquisitionType, ExposureTimeModeView),

      etmMappings(GhostIfuType / "blue",  GhostBlueExposureTimeModeView),
      etmMappings(GhostIfuType / "red",   GhostRedExposureTimeModeView),

      etmMappings(GmosNorthImagingFilterType, ExposureTimeModeView),
      etmMappings(GmosNorthLongSlitType, ExposureTimeModeView),
      etmMappings(GmosNorthLongSlitAcquisitionType, ExposureTimeModeView),

      etmMappings(GmosSouthImagingFilterType, ExposureTimeModeView),
      etmMappings(GmosSouthLongSlitType, ExposureTimeModeView),
      etmMappings(GmosSouthLongSlitAcquisitionType, ExposureTimeModeView),

      etmMappings(Igrins2LongSlitType, ExposureTimeModeView),

      etmMappings(ScienceRequirementsType, ExposureTimeModeView)
    ).flatten