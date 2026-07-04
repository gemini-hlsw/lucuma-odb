// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import cats.syntax.partialOrder.*
import lucuma.core.model.Access
import lucuma.core.model.Target
import lucuma.odb.data.BlindOffsetType
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

sealed trait TargetEnvironmentInput

object TargetEnvironmentInput:

  final case class Create(
    explicitBase:          Option[CoordinatesInput.Create],
    asterism:              Option[List[Target.Id]],
    signalToNoiseTargetId: Option[Target.Id],
    useBlindOffset:        Option[Boolean],
    blindOffsetTarget:     Option[TargetPropertiesInput.Create],
    blindOffsetType:       BlindOffsetType
  ) extends TargetEnvironmentInput
  object Create:
    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap {
        case List(
          CoordinatesInput.Create.Binding.Option("explicitBase", rBase),
          TargetIdBinding.List.Option("asterism", rAsterism),
          TargetIdBinding.Option("signalToNoiseTargetId", rSnTarget),
          BooleanBinding.Option("useBlindOffset", rUseBlindOffset),
          TargetPropertiesInput.Binding.Option("blindOffsetTarget", rBlindOffsetTarget),
          BlindOffsetTypeBinding.Option("blindOffsetType", rBlindOffsetType)
        ) => (rBase, rAsterism, rSnTarget, rUseBlindOffset, rBlindOffsetTarget, rBlindOffsetType)
          .parMapN((b, a, sn, u, t, o) => Create(b, a, sn, u, t, o.getOrElse(BlindOffsetType.Manual)))
      }


  final case class Edit(
    explicitBase:          Nullable[CoordinatesInput.Edit],
    asterism:              Nullable[List[Target.Id]],
    signalToNoiseTargetId: Nullable[Target.Id],
    useBlindOffset:        Option[Boolean],
    blindOffsetTarget:     Nullable[TargetPropertiesInput.Create],
    blindOffsetType:       BlindOffsetType
  ) extends TargetEnvironmentInput:
    def limitToPreExecution(access: Access): Boolean =
      // staff can edit the blind offset for ongoing observations
      access <= Access.Pi || asterism.isDefined || explicitBase.isDefined || signalToNoiseTargetId.isDefined

  object Edit:
    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          CoordinatesInput.Edit.Binding.Nullable("explicitBase", rBase),
          TargetIdBinding.List.Nullable("asterism", rAsterism),
          TargetIdBinding.Nullable("signalToNoiseTargetId", rSnTarget),
          BooleanBinding.Option("useBlindOffset", rUseBlindOffset),
          TargetPropertiesInput.Binding.Nullable("blindOffsetTarget", rBlindOffsetTarget),
          BlindOffsetTypeBinding.Option("blindOffsetType", rBlindOffsetType)
        ) => (rBase, rAsterism, rSnTarget, rUseBlindOffset, rBlindOffsetTarget, rBlindOffsetType)
          .parMapN((b, a, sn, u, t, o) => Edit(b, a, sn, u, t, o.getOrElse(BlindOffsetType.Manual)))
      }
