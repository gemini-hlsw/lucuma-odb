// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.enums.GroupStandingStatus
import lucuma.odb.graphql.binding.CalculationStateBinding
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.WhereEq
import lucuma.odb.graphql.binding.WhereOrder
import lucuma.odb.graphql.binding.enumeratedBinding

object WhereCalculatedGroupStanding:

  def binding(path: Path): Matcher[Predicate] =
    val WhereCalculationState = WhereOrder.binding(path / "calculationState", CalculationStateBinding)
    val WhereStatus           = WhereEq.binding(path / "value" / "status", enumeratedBinding[GroupStandingStatus])

    ObjectFieldsBinding.rmap:
      case List(
        WhereCalculationState.Option("calculationState", rCalc),
        WhereStatus.Option("status", rStatus)
      ) => (rCalc, rStatus).parMapN: (calc, status) =>
        and(List(calc, status).flatten)
