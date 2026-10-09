// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.data.NonEmptyList
import cats.syntax.parallel.*
import cats.syntax.traverse.*
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
    ObjectFieldsBinding.rmap:
      case List(
        TelescopeConfigAlongSlitInput.Binding.List.Option("alongSlit", rAlongSlit),
        TelescopeConfigInput.Binding.List.Option("toSky", rOnSky)
      ) =>
        val rAlongSlitʹ =
          rAlongSlit.flatMap(_.traverse(cs => NonEmptyList.fromList(cs).toResult("alongSlit must not be empty").map(SlitTelescopeConfigs.AlongSlit(_))))
        val rOnSkyʹ     =
          rOnSky.flatMap(_.traverse(cs => NonEmptyList.fromList(cs).toResult("toSky must not be empty").map(SlitTelescopeConfigs.ToSky(_))))
        (rAlongSlitʹ, rOnSkyʹ).parFlatMapN: (alongSlit, onSky) =>
          oneOrFail[SlitTelescopeConfigs](
            alongSlit -> "alongSlit",
            onSky     -> "toSky"
          )
