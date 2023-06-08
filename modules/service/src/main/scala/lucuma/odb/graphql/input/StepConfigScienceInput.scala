// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import lucuma.core.model.sequence.StepConfig.Science
import lucuma.odb.graphql.binding.*

object StepConfigScienceInput {

  val Binding: Matcher[Science] =
    ObjectFieldsBinding.rmap {
      case List(
        OffsetInput.Binding("offset", rOffset)
      ) => rOffset.map(Science(_))
    }

}