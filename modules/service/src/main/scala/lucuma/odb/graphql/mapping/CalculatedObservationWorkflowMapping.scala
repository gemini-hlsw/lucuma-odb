// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import io.circe.Json
import lucuma.odb.graphql.table.ObscalcTable

trait CalculatedObservationWorkflowMapping[F[_]] extends ObscalcTable[F]:

  private lazy val ObservationWorkflowMapping: TypeMapping =
    ObjectMapping(CalculatedObservationWorkflowType / "value")(
      SqlField("synthetic_id",     ObscalcTable.ObservationId, hidden = true, key = true),
      SqlField("state",            ObscalcTable.Workflow.State),
      SqlField("validTransitions", ObscalcTable.Workflow.Transitions),
      SqlField("validations",      ObscalcTable.Workflow.Validations, hidden = true),
      CursorFieldJson(
        "validationErrors",
        _.fieldAs[Json]("validations").flatMap(ValidationSeverity.addAll),
        List("validations")
      )
    )

  private lazy val CalculatedObservationWorkflowMapping: TypeMapping =
    ObjectMapping(CalculatedObservationWorkflowType)(
      SqlField("synthetic_id",     ObscalcTable.ObservationId, hidden = true, key = true),
      SqlField("calculationState", ObscalcTable.CalculationState),
      // state is deprecated on the graphql schema
      SqlField("state",            ObscalcTable.CalculationState),
      SqlObject("value")
    )

  lazy val CalculatedObservationWorkflowMappings: List[TypeMapping] = List(
    ObservationWorkflowMapping,
    CalculatedObservationWorkflowMapping
  )
