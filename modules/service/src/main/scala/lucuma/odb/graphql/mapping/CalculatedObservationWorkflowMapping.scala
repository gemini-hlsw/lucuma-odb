// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import io.circe.Json
import lucuma.common.middleware.UserContext
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.model.User
import lucuma.odb.graphql.table.ObscalcTable
import lucuma.odb.service.ObservationWorkflowService

trait CalculatedObservationWorkflowMapping[F[_]] extends ObscalcTable[F]:

  private lazy val ObservationWorkflowMapping: TypeMapping =
    ObjectMapping(CalculatedObservationWorkflowType / "value")(
      SqlField("synthetic_id",      ObscalcTable.ObservationId, hidden = true, key = true),
      SqlField("state",             ObscalcTable.Workflow.State),
      SqlField("storedTransitions", ObscalcTable.Workflow.Transitions, hidden = true),
      // Obscalc stores the transitions available to staff; others see only theirs.
      CursorField[List[ObservationWorkflowState]](
        "validTransitions",
        c =>
          for
            u  <- c.envR[User](UserContext.EnvKey)
            s  <- c.fieldAs[ObservationWorkflowState]("state")
            ts <- c.fieldAs[List[ObservationWorkflowState]]("storedTransitions")
          yield ObservationWorkflowService.visibleTransitions(u, s, ts),
        List("state", "storedTransitions")
      ),
      SqlField("validations",       ObscalcTable.Workflow.Validations, hidden = true),
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
