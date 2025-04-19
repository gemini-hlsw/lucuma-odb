// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import lucuma.core.enums.DatasetQaState
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

case class DatasetPropertiesInput(
  qaState: Nullable[DatasetQaState],
  comment: Nullable[NonEmptyString]
)

object DatasetPropertiesInput {

  val Binding: Matcher[DatasetPropertiesInput] =
    ObjectFieldsBinding.rmap {
      case List(
        DatasetQaStateBinding.Nullable("qaState", rQaState),
        NonEmptyStringBinding.Nullable("comment", rComment)
      ) => (rQaState, rComment).mapN { (qa, comment) =>
        DatasetPropertiesInput(qa, comment)
      }
    }

}
