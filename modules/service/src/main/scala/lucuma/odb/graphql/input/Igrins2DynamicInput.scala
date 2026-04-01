// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.odb.graphql.binding.*

object Igrins2DynamicInput:

  val Binding: Matcher[Igrins2DynamicConfig] =
    ObjectFieldsBinding.rmap:
      case List(
        TimeSpanInput.Binding("exposure", rExposure)
      ) => rExposure.map(Igrins2DynamicConfig(_))
