// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.model.sequence.Dataset
import lucuma.odb.graphql.binding.*

object DatasetIdInput {

  val Binding: Matcher[Dataset.Id] =
    ObjectFieldsBinding.rmap {
      case List(
        StepIdBinding("stepId", rStepId),
        NonNegShortBinding("index", rIndex)
      ) =>
        (rStepId, rIndex).parMapN { (sid, index) =>
          Dataset.Id(sid, index)
        }
    }

}