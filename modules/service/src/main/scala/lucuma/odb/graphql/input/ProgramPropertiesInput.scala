// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import lucuma.core.model.Semester
import lucuma.odb.data.Existence
import lucuma.odb.graphql.binding.*

object ProgramPropertiesInput {

  case class Create(
    name:     Option[NonEmptyString],
    semester: Option[Semester],
  )

  object Create {
    val Empty: Create =
      Create(None, None)
  }

  case class Edit(
    name:           Option[NonEmptyString],
    semester:       Option[Semester],
    existence:      Option[Existence]
  )

  object Edit {
    val Empty: Edit =
      Edit(None, None, None)
  }

  private val data: Matcher[(
    Option[NonEmptyString],
    Option[Semester],
    Option[Existence]
  )] =
    ObjectFieldsBinding.rmap {
      case List(
        NonEmptyStringBinding.Option("name", rName),
        SemesterBinding.Option("semester", rSemester),
        ExistenceBinding.Option("existence", rExistence),
      ) =>
        (rName, rSemester, rExistence).parTupled
    }

  val CreateBinding: Matcher[ProgramPropertiesInput.Create] =
    data.map((n, s, _) => Create(n, s))

  val EditBinding: Matcher[Edit] =
    data.map(Edit.apply)

}
