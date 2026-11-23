// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.syntax.apply.*
import eu.timepit.refined.types.numeric.NonNegInt
import io.circe.refined.given
import lucuma.core.model.sequence.ExecutedSteps
import lucuma.odb.graphql.table.ObscalcTable

trait CalculatedExecutedStepsMapping[F[_]] extends ObscalcTable[F]:

  private lazy val ExecutedStepsMapping: TypeMapping =
    ObjectMapping(CalculatedExecutedStepsType / "value")(
      SqlField("synthetic_id", ObscalcTable.ObservationId, hidden = true, key = true),
      SqlField("successful",   ObscalcTable.ExecutedSteps.Successful),
      SqlField("unsuccessful", ObscalcTable.ExecutedSteps.Unsuccessful),
      CursorField[NonNegInt](
        "total",
        c =>
          (
            c.fieldAs[NonNegInt]("successful"),
            c.fieldAs[NonNegInt]("unsuccessful")
          ).mapN(ExecutedSteps(_, _).total),
        List("successful", "unsuccessful")
      )
    )

  private lazy val CalculatedExecutedStepsMapping: TypeMapping =
    ObjectMapping(CalculatedExecutedStepsType)(
      SqlField("synthetic_id",     ObscalcTable.ObservationId, hidden = true, key = true),
      SqlField("calculationState", ObscalcTable.CalculationState),
      SqlObject("value")
    )

  lazy val CalculatedExecutedStepsMappings: List[TypeMapping] = List(
    ExecutedStepsMapping,
    CalculatedExecutedStepsMapping
  )
