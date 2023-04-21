// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all._
import edu.gemini.grackle.Result
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.util.TimeSpan
import lucuma.odb.data.Existence
import lucuma.odb.data.Group
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding._

object GroupPropertiesInput {

  case class Create(
    name: Option[NonEmptyString],
    description: Option[NonEmptyString],
    minimumRequired: Option[NonNegShort],
    ordered: Boolean,
    minimumInterval: Option[TimeSpan],
    maximumInterval: Option[TimeSpan],
    parentGroupId: Option[Group.Id],
    parentGroupIndex: NonNegShort,
  )

  val Empty: Create =
    Create(None, None, None, false, None, None, None, NonNegShort.unsafeFrom(0))

  val CreateBinding: Matcher[Create] =
    ObjectFieldsBinding.rmap {
      case List(
        NonEmptyStringBinding.Option("name", rName),
        NonEmptyStringBinding.Option("description", rDescription),
        NonNegShortBinding.Option("minimumRequired", rMinimumRequired),
        BooleanBinding.Option("ordered", rOrdered),
        TimeSpanInput.Binding.Option("minimumInterval", rMinimumInterval),
        TimeSpanInput.Binding.Option("maximumInterval", rMaximumInterval),
        GroupIdBinding.Option("parentGroup", rParentGroup),
        NonNegShortBinding.Option("parentGroupIndex", rParentGroupIndex),
      ) =>
        (rName, rDescription, rMinimumRequired, rOrdered, rMinimumInterval, rMaximumInterval, rParentGroup, rParentGroupIndex).parTupled.flatMap {
          (name, description, minimumRequired, ordered, minimumInterval, maximumInterval, parentGroup, parentGroupIndex) =>
            (minimumInterval, maximumInterval) match
              case (Some(min), Some(max)) if max <= min => Result.failure("Minimum interval must be less than maximum interval.")
              case _ =>
                Result(Create(
                  name, 
                  description, 
                  minimumRequired, 
                  ordered.getOrElse(false), 
                  minimumInterval, 
                  maximumInterval, 
                  parentGroup, 
                  parentGroupIndex.getOrElse(NonNegShort.unsafeFrom(0))
                ))
        }
    }

}