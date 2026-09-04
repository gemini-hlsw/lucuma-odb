// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import lucuma.core.enums.ConditionsExpectationType
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.binding.*

final case class ConditionsExpectationInput(
   tpe: ConditionsExpectationType,
   timespan: TimeSpan
)

object ConditionsExpectationInput:

 val Binding: Matcher[ConditionsExpectationInput] =
   ObjectFieldsBinding.rmap {
   case List(
      ConditionsExpectationTypeBinding("type", rType),
      TimeSpanInput.Binding("timeframe", rTimeFrame)
   ) =>
      (rType, rTimeFrame).parMapN(ConditionsExpectationInput.apply)
   }

