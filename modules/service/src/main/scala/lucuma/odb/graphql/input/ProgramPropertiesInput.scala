// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.data.Ior
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.odb.data.Existence
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

import java.time.LocalDate

object ProgramPropertiesInput:

  case class Create(
    name:        Option[NonEmptyString],
    description: Option[NonEmptyString],
    goa:         GoaPropertiesInput.Create,
    existence:   Existence,
    active:      Option[Ior[LocalDate, LocalDate]]
  )

  object Create:
    val Default: Create =
      Create(None, None, GoaPropertiesInput.Create.Default, Existence.Present, None)

    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap:
        case List(
          NonEmptyStringBinding.Option("name", rName),
          NonEmptyStringBinding.Option("description", rDescription),
          GoaPropertiesInput.Create.Binding.Option("goa", rGoa),
          ExistenceBinding.Option("existence", rExistence),
          DateBinding.Option("activeStart", rActiveStart),
          DateBinding.Option("activeEnd",   rActiveEnd)
        ) =>
          val rActive = date.validateOptionalInputInterval("activeStart", "activeEnd", rActiveStart, rActiveEnd)
          (rName, rDescription, rGoa, rExistence, rActive).parMapN: (name, description, goa, existence, active) =>
            Create(
              name,
              description,
              goa.getOrElse(GoaPropertiesInput.Create.Default),
              existence.getOrElse(Existence.Present),
              active
            )

  case class Edit(
    name:        Nullable[NonEmptyString],
    description: Nullable[NonEmptyString],
    goa:         Option[GoaPropertiesInput.Edit],
    existence:   Option[Existence],
    active:      Option[Ior[LocalDate, LocalDate]]
  )

  object Edit:
    val Default: Edit =
      Edit(Nullable.Absent, Nullable.Absent, None, None, None)

    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap:
        case List(
          NonEmptyStringBinding.Nullable("name", rName),
          NonEmptyStringBinding.Nullable("description", rDescription),
          GoaPropertiesInput.Edit.Binding.Option("goa", rGoa),
          ExistenceBinding.Option("existence", rExistence),
          DateBinding.Option("activeStart", rActiveStart),
          DateBinding.Option("activeEnd",   rActiveEnd)
        ) =>
          val rActive = date.validateOptionalInputInterval("activeStart", "activeEnd", rActiveStart, rActiveEnd)
          (rName, rDescription, rGoa, rExistence, rActive).parMapN(Edit.apply)
