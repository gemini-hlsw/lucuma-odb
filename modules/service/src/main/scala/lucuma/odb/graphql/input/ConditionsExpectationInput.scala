// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import lucuma.core.util.TimeSpan
import lucuma.odb.data.Tag
import lucuma.odb.graphql.binding.*

final case class ConditionsExpectationInput(
   tpe: Tag,
   timespan: TimeSpan
)

object ConditionsExpectationInput:

 val Binding: Matcher[ConditionsExpectationInput] =
   ObjectFieldsBinding.rmap {
   case List(
      TagBinding("type", rType),
      TimeSpanInput.Binding("timeframe", rTimeFrame)
   ) =>
      (rType, rTimeFrame).parMapN(ConditionsExpectationInput.apply)
   }

