// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path
import grackle.skunk.SkunkMapping
import lucuma.core.math.Angle
import lucuma.core.math.HourAngle
import lucuma.odb.graphql.table.ChronConditionsEntryView
import lucuma.odb.graphql.table.ImagingConfigOptionTable
import lucuma.odb.graphql.table.ObservationView
import lucuma.odb.graphql.table.TelescopeConfigGeneratorView
import lucuma.odb.graphql.table.SpectroscopyConfigOptionTable
import lucuma.odb.graphql.util.MappingExtras

trait AngleMapping[F[_]] extends ObservationView[F]
                            with ChronConditionsEntryView[F]
                            with ImagingConfigOptionTable[F]
                            with TelescopeConfigGeneratorView[F]
                            with SpectroscopyConfigOptionTable[F]
                            with MappingExtras[F] {

  private val µPerMilli: Long = 1000L
  private val µPerSec: Long   = 1000L * µPerMilli
  private val µPerMin: Long   =   60L * µPerSec
  private val µPerUnit: Long  =   60L * µPerMin

  private def angleToTime(µs: Long): Angle => BigDecimal = angleToArc(µs * 15)
  private def angleToArc(µas: Long)(a: Angle): BigDecimal = BigDecimal(a.toMicroarcseconds) / µas

  private def angleMappingAtPath(
    path: Path,
    valueColumn: ColumnRef,
    idColumns: ColumnRef*
  ): ObjectMapping =
    ObjectMapping(path)(
      (idColumns.toList.zipWithIndex.map { (ref, idx) =>
        SqlField(s"synthetic_id$idx", ref, key = true, hidden = true)
      }) ++ List(
        SqlField("value", valueColumn, hidden = true),
        FieldRef[Angle]("value").as("microarcseconds", _.toMicroarcseconds),
        FieldRef[Angle]("value").as("microseconds", angleToTime(1L)),
        FieldRef[Angle]("value").as("milliarcseconds", angleToArc(µPerMilli)),
        FieldRef[Angle]("value").as("milliseconds", angleToTime(µPerMilli)),
        FieldRef[Angle]("value").as("arcseconds", angleToArc(µPerSec)),
        FieldRef[Angle]("value").as("seconds", angleToTime(µPerSec)),
        FieldRef[Angle]("value").as("arcminutes", angleToArc(µPerMin)),
        FieldRef[Angle]("value").as("minutes", angleToTime(µPerMin)),
        FieldRef[Angle]("value").as("degrees", angleToArc(µPerUnit)),
        FieldRef[Angle]("value").as("hours", angleToTime(µPerUnit)),
        FieldRef[Angle]("value").as("dms", c => Angle.dms.get(c).format),
        FieldRef[Angle]("value").as("hms", c => HourAngle.fromStringHMS.reverseGet(Angle.hourAngle.get(c)))
      )*
    )

  import ObservationView.ScienceRequirements.Spectroscopy
  import ObservationView.ScienceRequirements.Imaging

  lazy val AngleMappings: List[TypeMapping] =
    List(
      angleMappingAtPath(PosAngleConstraintType / "angle", ObservationView.PosAngleConstraint.Angle, ObservationView.Id),
      angleMappingAtPath(SpectroscopyScienceRequirementsType / "focalPlaneAngle", Spectroscopy.FocalPlaneAngle.Value, Spectroscopy.FocalPlaneAngle.SyntheticId),
      angleMappingAtPath(ConditionsMeasurementType / "seeing", ChronConditionsEntryView.Measurement.Seeing.Value, ChronConditionsEntryView.Measurement.Seeing.SyntheticId),
      angleMappingAtPath(ConditionsMeasurementType / "elevation", ChronConditionsEntryView.Measurement.Pointing.Elevation, ChronConditionsEntryView.Measurement.Pointing.SyntheticId),
      angleMappingAtPath(ConditionsMeasurementType / "azimuth", ChronConditionsEntryView.Measurement.Pointing.Azimuth, ChronConditionsEntryView.Measurement.Pointing.SyntheticId),
      angleMappingAtPath(SpectroscopyConfigOptionType / "slitWidth", SpectroscopyConfigOptionTable.SlitWidth, SpectroscopyConfigOptionTable.Instrument, SpectroscopyConfigOptionTable.Index),
      angleMappingAtPath(SpectroscopyConfigOptionType / "slitLength", SpectroscopyConfigOptionTable.SlitLength, SpectroscopyConfigOptionTable.Instrument, SpectroscopyConfigOptionTable.Index),
      angleMappingAtPath(ImagingConfigOptionType / "fov", ImagingConfigOptionTable.Fov, ImagingConfigOptionTable.Instrument, ImagingConfigOptionTable.Index),
      angleMappingAtPath(ImagingScienceRequirementsType / "minimumFov", Imaging.MinimumFovAngle.Value, Imaging.MinimumFovAngle.SyntheticId),
      angleMappingAtPath(RandomTelescopeConfigGeneratorType / "size", TelescopeConfigGeneratorView.Size, TelescopeConfigGeneratorView.Random.ObservationId, TelescopeConfigGeneratorView.Random.TelescopeConfigGeneratorRole),
      angleMappingAtPath(SpiralTelescopeConfigGeneratorType / "size", TelescopeConfigGeneratorView.Size, TelescopeConfigGeneratorView.Spiral.ObservationId, TelescopeConfigGeneratorView.Spiral.TelescopeConfigGeneratorRole)
    )

}
