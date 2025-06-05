// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.ObscalcView

trait CalculatedObservationWorkflowMapping[F[_]] extends ObscalcView[F]:

  private lazy val ObservationWorkflowMapping: TypeMapping =
    ObjectMapping(CalculatedObservationWorkflowType / "value")(
      SqlField("synthetic_id",     ObscalcView.ObservationId, hidden = true, key = true),
      SqlField("state",            ObscalcView.Workflow.State),
      SqlField("validTransitions", ObscalcView.Workflow.Transitions),
      SqlJson("validationErrors",  ObscalcView.Workflow.Validations)
    )

  private lazy val CalculatedObservationWorkflowMapping: TypeMapping =
    ObjectMapping(CalculatedObservationWorkflowType)(
      SqlField("synthetic_id", ObscalcView.ObservationId, hidden = true, key = true),
      SqlField("state",        ObscalcView.CalculationState),
      SqlObject("value")
    )

  lazy val CalculatedObservationWorkflowMappings: List[TypeMapping] = List(
    ObservationWorkflowMapping,
    CalculatedObservationWorkflowMapping
  )
