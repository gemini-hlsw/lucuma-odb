// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import lucuma.odb.data.Existence
import lucuma.odb.graphql.binding.*

object ProgramPropertiesInput:

  case class Create(
    name:      Option[NonEmptyString],
    goa:       GoaPropertiesInput.Create,
    existence: Existence
  )

  object Create:
    val Default: Create =
      Create(None, GoaPropertiesInput.Create.Default, Existence.Present)

    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap:
        case List(
          NonEmptyStringBinding.Option("name", rName),
          GoaPropertiesInput.Create.Binding.Option("goa", rGoa),
          ExistenceBinding.Option("existence", rExistence)
        ) => (rName, rGoa, rExistence).parMapN: (name, goa, existence) =>
          Create(
            name,
            goa.getOrElse(GoaPropertiesInput.Create.Default),
            existence.getOrElse(Existence.Present)
          )

  case class Edit(
    name:       Option[NonEmptyString],
    goa:        Option[GoaPropertiesInput.Edit],
    existence:  Option[Existence]
  )

  object Edit:
    val Default: Edit =
      Edit(None, None, None)

    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap:
        case List(
          NonEmptyStringBinding.Option("name", rName),
          GoaPropertiesInput.Edit.Binding.Option("goa", rGoa),
          ExistenceBinding.Option("existence", rExistence)
        ) => (rName, rGoa, rExistence).parMapN(Edit.apply)