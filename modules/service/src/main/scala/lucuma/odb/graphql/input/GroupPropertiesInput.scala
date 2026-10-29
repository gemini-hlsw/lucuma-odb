// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.numeric.PosShort
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import grackle.syntax.*
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
    sameNight: Boolean,
    minimumObservationsPerNight: Option[PosShort],
    maximumObservationsPerNight: Option[PosShort],
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
        edit.sameNight.getOrElse(sameNight),
        edit.minimumObservationsPerNight.toOptionOption.getOrElse(minimumObservationsPerNight),
        edit.maximumObservationsPerNight.toOptionOption.getOrElse(maximumObservationsPerNight),
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
    sameNight: Option[Boolean],
    minimumObservationsPerNight: Nullable[PosShort],
    maximumObservationsPerNight: Nullable[PosShort],
    parentGroupId: Nullable[Group.Id],
    parentGroupIndex: Option[NonNegShort],
    existence: Option[Existence]
  )

  val Empty: Create =
    Create(None, None, None, false, None, None, false, None, None, None, None, Existence.Default)

  // "Observe 0 of N" is never meaningful, so an OR group must require at least one element.
  // The complementary upper bound (no more than the element count) needs that count and so
  // lives in `GroupService`.
  val MinimumRequiredTooSmall: String =
    "Minimum required must be at least 1."

  val ObservationsPerNightOutOfOrder: String =
    "Minimum observations per night must be less than or equal maximum observations per night."

  val ObservationsPerNightSameNight: String =
    "Same night and observations per night are mutually exclusive."

  val CreateBinding: Matcher[Create] =
    ObjectFieldsBinding.rmap {
      case List(
        NonEmptyStringBinding.Option("name", rName),
        NonEmptyStringBinding.Option("description", rDescription),
        NonNegShortBinding.Option("minimumRequired", rMinimumRequired),
        BooleanBinding.Option("ordered", rOrdered),
        TimeSpanInput.Binding.Option("minimumInterval", rMinimumInterval),
        TimeSpanInput.Binding.Option("maximumInterval", rMaximumInterval),
        BooleanBinding.Option("sameNight", rSameNight),
        PosShortBinding.Option("minimumObservationsPerNight", rMinimumObservationsPerNight),
        PosShortBinding.Option("maximumObservationsPerNight", rMaximumObservationsPerNight),
        GroupIdBinding.Option("parentGroup", rParentGroup),
        NonNegShortBinding.Option("parentGroupIndex", rParentGroupIndex),
        ExistenceBinding.Option("existence", rExistence),
      ) =>
        val rMinimumRequiredʹ = rMinimumRequired.flatMap: m =>
          if m.exists(_.value < 1) then Result.failure(MinimumRequiredTooSmall)
          else m.success
        (rName, rDescription, rMinimumRequiredʹ, rOrdered, rMinimumInterval, rMaximumInterval, rSameNight, rMinimumObservationsPerNight, rMaximumObservationsPerNight, rParentGroup, rParentGroupIndex, rExistence).parFlatMapN {
          (name, description, minimumRequired, ordered, minimumInterval, maximumInterval, sameNight, minimumObservationsPerNight, maximumObservationsPerNight, parentGroup, parentGroupIndex, existence) =>
            (minimumInterval, maximumInterval) match
              case (Some(min), Some(max)) if max < min => Result.failure("Minimum interval must be less than or equal maximum interval.")
              case _ if sameNight.contains(true) && maximumInterval.isDefined =>
                Result.failure("Same night and maximum interval are mutually exclusive.")
              case _ if (minimumObservationsPerNight, maximumObservationsPerNight).mapN(_.value > _.value).getOrElse(false) =>
                Result.failure(ObservationsPerNightOutOfOrder)
              case _ if sameNight.contains(true) && (minimumObservationsPerNight.isDefined || maximumObservationsPerNight.isDefined) =>
                Result.failure(ObservationsPerNightSameNight)
              case _ =>
                Result(Create(
                  name,
                  description,
                  minimumRequired,
                  ordered.getOrElse(false),
                  minimumInterval,
                  maximumInterval,
                  sameNight.getOrElse(false),
                  minimumObservationsPerNight,
                  maximumObservationsPerNight,
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
        BooleanBinding.Option("sameNight", rSameNight),
        PosShortBinding.Nullable("minimumObservationsPerNight", rMinimumObservationsPerNight),
        PosShortBinding.Nullable("maximumObservationsPerNight", rMaximumObservationsPerNight),
        GroupIdBinding.Nullable("parentGroup", rParentGroup),
        NonNegShortBinding.NonNullable("parentGroupIndex", rParentGroupIndex),
        ExistenceBinding.Option("existence", rExistence),
      ) =>
        val rMinimumRequiredʹ = rMinimumRequired.flatMap: m =>
          if m.toOption.exists(_.value < 1) then Result.failure(MinimumRequiredTooSmall)
          else m.success
        (rName, rDescription, rMinimumRequiredʹ, rOrdered, rMinimumInterval, rMaximumInterval, rSameNight, rMinimumObservationsPerNight, rMaximumObservationsPerNight, rParentGroup, rParentGroupIndex, rExistence).parFlatMapN {
          (name, description, minimumRequired, ordered, minimumInterval, maximumInterval, sameNight, minimumObservationsPerNight, maximumObservationsPerNight, parentGroup, parentGroupIndex, existence) =>
            (minimumInterval.toOption, maximumInterval.toOption) match // Scala can't typecheck it if we match Nullable.NonNull for some reason :-\
              case (Some(min), Some(max)) if max < min => Result.failure("Minimum interval must be less than or equal maximum interval.")
              case _ if sameNight.contains(true) && maximumInterval.toOption.isDefined =>
                Result.failure("Same night and maximum interval are mutually exclusive.")
              case _ if (minimumObservationsPerNight.toOption, maximumObservationsPerNight.toOption).mapN(_.value > _.value).getOrElse(false) =>
                Result.failure(ObservationsPerNightOutOfOrder)
              case _ if sameNight.contains(true) && (minimumObservationsPerNight.toOption.isDefined || maximumObservationsPerNight.toOption.isDefined) =>
                Result.failure(ObservationsPerNightSameNight)
              case _ =>
                Result(Edit(
                  name,
                  description,
                  minimumRequired,
                  ordered,
                  minimumInterval,
                  maximumInterval,
                  sameNight,
                  minimumObservationsPerNight,
                  maximumObservationsPerNight,
                  parentGroup,
                  parentGroupIndex,
                  existence,
                ))
        }
    }

}
