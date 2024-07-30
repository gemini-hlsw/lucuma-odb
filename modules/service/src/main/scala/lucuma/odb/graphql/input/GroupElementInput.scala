// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.ObservationIdBinding
import lucuma.odb.graphql.binding.GroupIdBinding
import grackle.Result

final case class GroupElementInput(value: Either[Group.Id, Observation.Id])

object GroupElementInput:
  val Binding = ObjectFieldsBinding.rmap:
    case List(
      GroupIdBinding.Option("groupId", rGroupId),
      ObservationIdBinding.Option("observationId", rObservationId),
    ) =>
      (rGroupId, rObservationId).parTupled.flatMap:
        case (Some(g), None) => Result.Success(GroupElementInput(Left(g)))
        case (None, Some(p)) => Result.Success(GroupElementInput(Right(p)))
        case _ => Result.failure("Exactly one of groupId and observationId must be specified.")
