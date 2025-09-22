// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import lucuma.core.model.Group
import lucuma.core.util.TimeSpan
import lucuma.odb.data.Existence
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
    existence: Existence
  ) {

    def withEdit(edit: Edit): Create =
      Create(
        edit.name.toOptionOption.getOrElse(name),
        edit.description.toOptionOption.getOrElse(description),
        edit.minimumRequired.toOptionOption.getOrElse(minimumRequired),
        edit.ordered.getOrElse(ordered),
        edit.minimumInterval.toOptionOption.getOrElse(minimumInterval),
        edit.maximumInterval.toOptionOption.getOrElse(maximumInterval),
        edit.parentGroupId.toOptionOption.getOrElse(parentGroupId),
        edit.parentGroupIndex.orElse(parentGroupIndex),
        edit.existence.getOrElse(existence)
      )

  }

  case class Edit(
    name: Nullable[NonEmptyString],
    description: Nullable[NonEmptyString],
    minimumRequired: Nullable[NonNegShort],
    ordered: Option[Boolean],
    minimumInterval: Nullable[TimeSpan],
    maximumInterval: Nullable[TimeSpan],
    parentGroupId: Nullable[Group.Id],
    parentGroupIndex: Option[NonNegShort],
    existence: Option[Existence]
  )

  val Empty: Create =
    Create(None, None, None, false, None, None, None, None, Existence.Default)

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
        ExistenceBinding.Option("existence", rExistence),
      ) =>
        (rName, rDescription, rMinimumRequired, rOrdered, rMinimumInterval, rMaximumInterval, rParentGroup, rParentGroupIndex, rExistence).parTupled.flatMap {
          (name, description, minimumRequired, ordered, minimumInterval, maximumInterval, parentGroup, parentGroupIndex, existence) =>
            (minimumInterval, maximumInterval) match
              case (Some(min), Some(max)) if max < min => Matcher.validationFailure("Minimum interval must be less than or equal maximum interval.")
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
                  existence.getOrElse(Existence.Default),
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
        ExistenceBinding.Option("existence", rExistence),
      ) =>
        (rName, rDescription, rMinimumRequired, rOrdered, rMinimumInterval, rMaximumInterval, rParentGroup, rParentGroupIndex, rExistence).parTupled.flatMap {
          (name, description, minimumRequired, ordered, minimumInterval, maximumInterval, parentGroup, parentGroupIndex, existence) =>
            (minimumInterval.toOption, maximumInterval.toOption) match // Scala can't typecheck it if we match Nullable.NonNull for some reason :-\
              case (Some(min), Some(max)) if max < min => Matcher.validationFailure("Minimum interval must be less than or equal maximum interval.")
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
                  existence,
                ))
        }
    }

}
