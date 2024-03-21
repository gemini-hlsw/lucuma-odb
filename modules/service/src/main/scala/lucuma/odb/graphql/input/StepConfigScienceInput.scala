// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.enums.StepGuideState.Enabled
import lucuma.core.model.sequence.StepConfig.Science
import lucuma.odb.graphql.binding.*

object StepConfigScienceInput {

  val Binding: Matcher[Science] =
    ObjectFieldsBinding.rmap {
      case List(
        OffsetInput.Binding("offset", rOffset),
        StepGuideStateBinding.Option("guiding", rGuiding)
      ) => (rOffset, rGuiding).parMapN { (o, g) =>
         Science(o, g.getOrElse(Enabled))
      }
    }

}