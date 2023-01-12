// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.core.math.Angle
import lucuma.core.math.HourAngle
import lucuma.odb.graphql.table.ObservationView
import lucuma.odb.graphql.util.MappingExtras


trait AngleMapping[F[_]] extends ObservationView[F] with MappingExtras[F]  {

  private val µPerMilli: Long = 1000L
  private val µPerSec: Long   = 1000L * µPerMilli
  private val µPerMin: Long   =   60L * µPerSec
  private val µPerUnit: Long  =   60L * µPerMin

  private def angleToTime(µs: Long): Angle => BigDecimal = angleToArc(µs * 15)
  private def angleToArc(µas: Long)(a: Angle): BigDecimal = BigDecimal(a.toMicroarcseconds) / µas

  private def angleMapping(
    idColumn: ColumnRef,
    valueColumn: ColumnRef
  ): ObjectMapping =
    ObjectMapping(
      tpe = AngleType,
      fieldMappings = List(
        SqlField("synthetic_id", idColumn, key = true, hidden = true),
        SqlField("value", valueColumn, hidden = true),
        FieldRef[Angle]("value").as("microarcseconds", _.toMicroarcseconds),
        FieldRef[Angle]("value").as("microseconds",    angleToTime(1L)),
        FieldRef[Angle]("value").as("milliarcseconds", angleToArc(µPerMilli)),
        FieldRef[Angle]("value").as("milliseconds",    angleToTime(µPerMilli)),
        FieldRef[Angle]("value").as("arcseconds",      angleToArc(µPerSec)),
        FieldRef[Angle]("value").as("seconds",         angleToTime(µPerSec)),
        FieldRef[Angle]("value").as("arcminutes",      angleToArc(µPerMin)),
        FieldRef[Angle]("value").as("minutes",         angleToTime(µPerMin)),
        FieldRef[Angle]("value").as("degrees",         angleToArc(µPerUnit)),
        FieldRef[Angle]("value").as("hours",           angleToTime(µPerUnit)),
        FieldRef[Angle]("value").as("dms", c => Angle.dms.get(c).format),
        FieldRef[Angle]("value").as("hms", c => HourAngle.fromStringHMS.reverseGet(Angle.hourAngle.get(c)))
      )
    )

  import ObservationView.ScienceRequirements.Spectroscopy

  lazy val AngleMapping: TypeMapping =
    SwitchMapping(
      AngleType,
      List(
        PosAngleConstraintType / "angle"                        -> angleMapping(ObservationView.Id, ObservationView.PosAngleConstraint.Angle),
        SpectroscopyScienceRequirementsType / "focalPlaneAngle" -> angleMapping(Spectroscopy.FocalPlaneAngle.SyntheticId, Spectroscopy.FocalPlaneAngle.Value),
      )
    )
}
