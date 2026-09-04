// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.data.Ior
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.enums.ProgramStatus
import lucuma.core.syntax.string.*
import lucuma.odb.data.Existence
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

import java.time.LocalDate
object ProgramPropertiesInput:

  private val ObservationValidationWarningBinding:  Matcher[ObservationValidationCode.Warning] = 
    enumeratedBinding[ObservationValidationCode].emap: c =>
      c.fold(
        e => Left(s"Fatal error ${e.tag.toScreamingSnakeCase} cannot be dismissed."),
        w => Right(w)
      )

  case class Create(
    name:        Option[NonEmptyString],
    description: Option[NonEmptyString],
    goa:         GoaPropertiesInput.Create,
    existence:   Existence,
    explicitStatus: Nullable[ProgramStatus],
    active:      Option[Ior[LocalDate, LocalDate]],
    dismissedWarnings: Option[List[ObservationValidationCode.Warning]],
  )

  object Create:
    val Default: Create =
      Create(None, None, GoaPropertiesInput.Create.Default, Existence.Present, Nullable.Absent, None, None)

    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap:
        case List(
          NonEmptyStringBinding.Option("name", rName),
          NonEmptyStringBinding.Option("description", rDescription),
          GoaPropertiesInput.Create.Binding.Option("goa", rGoa),
          ExistenceBinding.Option("existence", rExistence),
          ProgramStatusBinding.Nullable("explicitStatus", rExplicitStatus),
          DateBinding.Option("activeStart", rActiveStart),
          DateBinding.Option("activeEnd",   rActiveEnd),
          ObservationValidationWarningBinding.List.NonNullable("dismissedWarnings", rdismissedWarnings),
        ) =>
          val rActive = date.validateOptionalInputInterval("activeStart", "activeEnd", rActiveStart, rActiveEnd)
          (rName, rDescription, rGoa, rExistence, rExplicitStatus, rActive, rdismissedWarnings).parMapN: (name, description, goa, existence, explicitStatus, active, dismissedWarnings) =>
            Create(
              name,
              description,
              goa.getOrElse(GoaPropertiesInput.Create.Default),
              existence.getOrElse(Existence.Present),
              explicitStatus,
              active,
              dismissedWarnings,
            )

  case class Edit(
    name:           Nullable[NonEmptyString],
    description:    Nullable[NonEmptyString],
    goa:            Option[GoaPropertiesInput.Edit],
    existence:      Option[Existence],
    explicitStatus: Nullable[ProgramStatus],
    active:         Option[Ior[LocalDate, LocalDate]],
    dismissedWarnings: Option[List[ObservationValidationCode.Warning]],
  )

  object Edit:
    val Default: Edit =
      Edit(Nullable.Absent, Nullable.Absent, None, None, Nullable.Absent, None, None)

    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap:
        case List(
          NonEmptyStringBinding.Nullable("name", rName),
          NonEmptyStringBinding.Nullable("description", rDescription),
          GoaPropertiesInput.Edit.Binding.Option("goa", rGoa),
          ExistenceBinding.Option("existence", rExistence),
          ProgramStatusBinding.Nullable("explicitStatus", rExplicitStatus),
          DateBinding.Option("activeStart", rActiveStart),
          DateBinding.Option("activeEnd",   rActiveEnd),
          ObservationValidationWarningBinding.List.NonNullable("dismissedWarnings", rdismissedWarnings)
        ) =>
          val rActive = date.validateOptionalInputInterval("activeStart", "activeEnd", rActiveStart, rActiveEnd)
          (rName, rDescription, rGoa, rExistence, rExplicitStatus, rActive, rdismissedWarnings).parMapN(Edit.apply)
