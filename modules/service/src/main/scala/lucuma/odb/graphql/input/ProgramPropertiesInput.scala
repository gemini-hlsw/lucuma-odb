// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import lucuma.odb.data.Existence
import lucuma.odb.graphql.binding.*

object ProgramPropertiesInput {

  case class Create(
    name: Option[NonEmptyString]
  )

  object Create {
    val Empty: Create =
      Create(None)
  }

  case class Edit(
    name:           Option[NonEmptyString],
    existence:      Option[Existence]
  )

  object Edit {
    val Empty: Edit =
      Edit(None, None)
  }

  private val data: Matcher[(
    Option[NonEmptyString],
    Option[Existence]
  )] =
    ObjectFieldsBinding.rmap {
      case List(
        NonEmptyStringBinding.Option("name", rName),
        ExistenceBinding.Option("existence", rExistence),
      ) =>
        (rName, rExistence).parTupled
    }

  val CreateBinding: Matcher[ProgramPropertiesInput.Create] =
    data.map((n, _) => Create(n))

  val EditBinding: Matcher[Edit] =
    data.map(Edit.apply)

}
