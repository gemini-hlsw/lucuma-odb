// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.data.NonEmptyList
import cats.syntax.parallel.*
import grackle.syntax.*
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.sequence.TelescopeConfigAlongSlit
import lucuma.odb.graphql.binding.*

object TelescopeConfigAlongSlitInput:
  val Binding: Matcher[TelescopeConfigAlongSlit] =
    ObjectFieldsBinding.rmap:
      case List(
        OffsetComponentInput.BindingQ("q", rQ),
        StepGuideStateBinding("guiding", rGuiding)
      ) =>
        (rQ, rGuiding).parMapN(TelescopeConfigAlongSlit.apply)

object SlitTelescopeConfigsInput:
  val Binding: Matcher[SlitTelescopeConfigs] =
    OneOfBinding(
      "alongSlit" -> TelescopeConfigAlongSlitInput.Binding.List.rmap: cs =>
        NonEmptyList.fromList(cs).toResult("alongSlit must not be empty").map(SlitTelescopeConfigs.AlongSlit(_)),
      "toSky"     -> TelescopeConfigInput.Binding.List.rmap: cs =>
        NonEmptyList.fromList(cs).toResult("toSky must not be empty").map(SlitTelescopeConfigs.ToSky(_))
    )
