// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import lucuma.core.model.Group
import lucuma.core.util.TimeSpan
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

object GroupPropertiesInput {

  case class Create(
    name: Option[NonEmptyString],
    description: Option[NonEmptyString],
    minimumRequired: Option[NonNegShort],
    ordered: Boolean,
    minimumInterval: Option[TimeSpan],
    maximumInterval: Option[TimeSpan],
    parentGroupId: Option[Group.Id],
    parentGroupIndex: Option[NonNegShort],
  )

  case class Edit(
    name: Nullable[NonEmptyString],
    description: Nullable[NonEmptyString],
    minimumRequired: Nullable[NonNegShort],
    ordered: Option[Boolean],
    minimumInterval: Nullable[TimeSpan],
    maximumInterval: Nullable[TimeSpan],
    parentGroupId: Nullable[Group.Id],
    parentGroupIndex: Option[NonNegShort],
  )

  val Empty: Create =
    Create(None, None, None, false, None, None, None, None)

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
              case (Some(min), Some(max)) if max <= min => Matcher.validationFailure("Minimum interval must be less than maximum interval.")
              case _ =>
                Result(Create(
                  name, 
                  description, 
                  minimumRequired, 
                  ordered.getOrElse(false), 
                  minimumInterval, 
                  maximumInterval, 
                  parentGroup, 
                  parentGroupIndex,
                ))
        }
    }

  val EditBinding: Matcher[Edit] =
    ObjectFieldsBinding.rmap {
      case List(
        NonEmptyStringBinding.Nullable("name", rName),
        NonEmptyStringBinding.Nullable("description", rDescription),
        NonNegShortBinding.Nullable("minimumRequired", rMinimumRequired),
        BooleanBinding.Option("ordered", rOrdered),
        TimeSpanInput.Binding.Nullable("minimumInterval", rMinimumInterval),
        TimeSpanInput.Binding.Nullable("maximumInterval", rMaximumInterval),
        GroupIdBinding.Nullable("parentGroup", rParentGroup),
        NonNegShortBinding.NonNullable("parentGroupIndex", rParentGroupIndex),
      ) =>
        (rName, rDescription, rMinimumRequired, rOrdered, rMinimumInterval, rMaximumInterval, rParentGroup, rParentGroupIndex).parTupled.flatMap {
          (name, description, minimumRequired, ordered, minimumInterval, maximumInterval, parentGroup, parentGroupIndex) =>
            (minimumInterval.toOption, maximumInterval.toOption) match // Scala can't typecheck it if we match Nullable.NonNull for some reason :-\
              case (Some(min), Some(max)) if max <= min => Matcher.validationFailure("Minimum interval must be less than maximum interval.")
              case _ =>
                Result(Edit(
                  name, 
                  description, 
                  minimumRequired, 
                  ordered,
                  minimumInterval, 
                  maximumInterval, 
                  parentGroup, 
                  parentGroupIndex,
                ))
        }
    }

}
