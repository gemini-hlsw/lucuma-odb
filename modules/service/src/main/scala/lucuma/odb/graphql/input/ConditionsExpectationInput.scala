// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.data.NonEmptyList
import cats.syntax.all.*
import edu.gemini.grackle.Path
import edu.gemini.grackle.Predicate
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding._
import lucuma.core.util.TimeSpan
import lucuma.odb.data.Tag

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

