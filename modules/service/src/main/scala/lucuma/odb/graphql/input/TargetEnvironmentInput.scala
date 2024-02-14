// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import lucuma.core.model.Target
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.TargetIdBinding
import lucuma.odb.graphql.binding.*

trait TargetEnvironmentInput:
  def asterism: Nullable[List[Target.Id]]

object TargetEnvironmentInput:

  final case class Create(
    explicitBase: Nullable[CoordinatesInput.Create],
    asterism:     Nullable[List[Target.Id]]
  ) extends TargetEnvironmentInput
  object Create:
    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap {
        case List(
          CoordinatesInput.Create.Binding.Nullable("explicitBase", rBase),
          TargetIdBinding.List.Nullable("asterism", rAsterism)
        ) => (rBase, rAsterism).parMapN(Create(_, _))
      }


  final case class Edit(
    explicitBase: Nullable[CoordinatesInput.Edit],
    asterism:     Nullable[List[Target.Id]]
  ) extends TargetEnvironmentInput
  object Edit:
    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          CoordinatesInput.Edit.Binding.Nullable("explicitBase", rBase),
          TargetIdBinding.List.Nullable("asterism", rAsterism)
        ) => (rBase, rAsterism).parMapN(Edit(_, _))
      }


