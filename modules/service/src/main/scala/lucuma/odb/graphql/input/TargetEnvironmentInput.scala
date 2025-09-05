// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.model.Target
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

sealed trait TargetEnvironmentInput

object TargetEnvironmentInput:

  final case class Create(
    explicitBase:      Option[CoordinatesInput.Create],
    asterism:          Option[List[Target.Id]],
    blindOffsetTarget: Option[TargetPropertiesInput.Create]
  ) extends TargetEnvironmentInput
  object Create:
    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap {
        case List(
          CoordinatesInput.Create.Binding.Option("explicitBase", rBase),
          TargetIdBinding.List.Option("asterism", rAsterism),
          TargetPropertiesInput.Binding.Option("blindOffsetTarget", rBlindOffsetTarget)
        ) => (rBase, rAsterism, rBlindOffsetTarget).parMapN(Create(_, _, _))
      }


  final case class Edit(
    explicitBase:      Nullable[CoordinatesInput.Edit],
    asterism:          Nullable[List[Target.Id]],
    blindOffsetTarget: Nullable[TargetPropertiesInput.Create]
  ) extends TargetEnvironmentInput

  object Edit:
    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          CoordinatesInput.Edit.Binding.Nullable("explicitBase", rBase),
          TargetIdBinding.List.Nullable("asterism", rAsterism),
          TargetPropertiesInput.Binding.Nullable("blindOffsetTarget", rBlindOffsetTarget)
        ) => (rBase, rAsterism, rBlindOffsetTarget).parMapN(Edit(_, _, _))
      }
