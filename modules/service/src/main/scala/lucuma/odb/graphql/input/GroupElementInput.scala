// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.odb.graphql.binding.GroupIdBinding
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.ObservationIdBinding

final case class GroupElementInput(value: Either[Group.Id, Observation.Id])

object GroupElementInput:
  val Binding = ObjectFieldsBinding.rmap:
    case List(
      GroupIdBinding.Option("groupId", rGroupId),
      ObservationIdBinding.Option("observationId", rObservationId),
    ) =>
      (rGroupId, rObservationId).parFlatMapN: (g, o) =>
        oneOrFail(
          g.map(_.asLeft[Observation.Id]) -> "groupId",
          o.map(_.asRight[Group.Id])      -> "observationId"
        ).map(GroupElementInput(_))
