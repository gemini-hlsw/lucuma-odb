// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.data.NonEmptyList
import cats.syntax.apply.*
import cats.syntax.parallel.*
import grackle.Result
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
        (rAlongSlit, rOnSky).tupled.flatMap:
          case (Some(cs), None) =>
            NonEmptyList.fromList(cs).fold(
              Matcher.validationFailure("alongSlit must not be empty")
            )(nel => Result(SlitTelescopeConfigs.AlongSlit(nel)))
          case (None, Some(cs)) =>
            NonEmptyList.fromList(cs).fold(
              Matcher.validationFailure("toSky must not be empty")
            )(nel => Result(SlitTelescopeConfigs.ToSky(nel)))
          case _ =>
            Matcher.validationFailure("Exactly one of alongSlit or toSky must be provided")
