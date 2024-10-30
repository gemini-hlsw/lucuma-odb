// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.enums.StepGuideState.Enabled
import lucuma.core.math.Offset.Zero
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.odb.graphql.binding.*

object TelescopeConfigInput:

  val Binding: Matcher[TelescopeConfig] =
    ObjectFieldsBinding.rmap {
      case List(
        OffsetInput.Binding.Option("offset", rOffset),
        StepGuideStateBinding.Option("guiding", rGuiding)
      ) => (rOffset, rGuiding).parMapN { (o, g) =>
        TelescopeConfig(o.getOrElse(Zero), g.getOrElse(Enabled))
      }
    }


