// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.enums.ObservationWorkflowState
import lucuma.odb.graphql.binding.*

object WhereObservationWorkflow {

  def binding(path: Path): Matcher[Predicate] = {

    val StateBinding = WhereEq.binding(path / "state", enumeratedBinding[ObservationWorkflowState])

    lazy val WhereObservationWorkflowBinding = binding(path)
    ObjectFieldsBinding.rmap {
      case List(
        WhereObservationWorkflowBinding.List.Option("AND", rAND),
        WhereObservationWorkflowBinding.List.Option("OR", rOR),
        WhereObservationWorkflowBinding.Option("NOT", rNOT),
        StateBinding.Option("state", rState),
      ) =>
        (rAND, rOR, rNOT, rState).parMapN {
          (AND, OR, NOT, state) =>
            and(List(
              AND.map(and),
              OR.map(or),
              NOT.map(Not(_)),
              state
            ).flatten)
        }
    }

  }

}

