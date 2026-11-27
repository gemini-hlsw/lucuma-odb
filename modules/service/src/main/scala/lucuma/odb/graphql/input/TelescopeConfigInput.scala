// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.enums.OffsetPurpose
import lucuma.core.enums.StepGuideState
import lucuma.core.enums.StepGuideState.Enabled
import lucuma.core.math.Offset.Zero
import lucuma.core.model.sequence.TelescopeConfigSpec
import lucuma.odb.graphql.binding.*

object TelescopeConfigInput:

  /**
   * Guiding when unspecified: the default for the explicit purpose (a sky
   * position is not guided), otherwise guided.  The purpose itself may still be
   * derived later from the geometry, but only an explicit one decides the
   * guiding default.
   */
  def defaultGuiding(explicitPurpose: Option[OffsetPurpose]): StepGuideState =
    explicitPurpose.fold(Enabled)(_.defaultGuiding)

  val Binding: Matcher[TelescopeConfigSpec] =
    ObjectFieldsBinding.rmap {
      case List(
        OffsetInput.Binding.Option("offset", rOffset),
        StepGuideStateBinding.Option("guiding", rGuiding),
        OffsetPurposeBinding.Option("explicitPurpose", rPurpose)
      ) => (rOffset, rGuiding, rPurpose).parMapN { (o, g, p) =>
        TelescopeConfigSpec(o.getOrElse(Zero), g.getOrElse(defaultGuiding(p)), p)
      }
    }
