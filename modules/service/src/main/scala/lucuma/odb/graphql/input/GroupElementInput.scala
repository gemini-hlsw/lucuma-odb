// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.odb.graphql.binding.GroupIdBinding
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObservationIdBinding
import lucuma.odb.graphql.binding.OneOfBinding

final case class GroupElementInput(value: Either[Group.Id, Observation.Id])

object GroupElementInput:
  val Binding: Matcher[GroupElementInput] =
    OneOfBinding(
      "groupId"       -> GroupIdBinding.map(g => GroupElementInput(Left(g))),
      "observationId" -> ObservationIdBinding.map(o => GroupElementInput(Right(o)))
    )
