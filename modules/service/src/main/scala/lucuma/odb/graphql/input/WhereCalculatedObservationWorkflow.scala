// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.util.CalculationState
import lucuma.odb.graphql.binding.BooleanBinding
import lucuma.odb.graphql.binding.CalculationStateBinding
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.ObservationWorkflowStateBinding

object WhereCalculatedObservationWorkflow:

  def binding(path: Path): Matcher[Predicate] =
    val WhereCalculationState = WhereOrder.binding(path / "state", CalculationStateBinding)
    val WhereWorkflowState    = WhereOrder.binding(path / "value" / "state", ObservationWorkflowStateBinding)

    ObjectFieldsBinding.rmap:
      case List(
        BooleanBinding.Option("IS_NULL", rIsNull),
        WhereCalculationState.Option("calculationState", rCalc),
        WhereWorkflowState.Option("workflowState", rWork)
      ) => (rIsNull, rCalc, rWork).parMapN: (isNull, calc, work) =>
        and(List(
          isNull.map(IsNull(path / "synthetic_id", _)),
          calc,
          work
        ).flatten)