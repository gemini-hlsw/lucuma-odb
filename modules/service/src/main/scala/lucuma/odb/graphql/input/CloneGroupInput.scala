// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import lucuma.core.model.Group
import lucuma.odb.graphql.binding.*

final case class CloneGroupInput(
  groupId:  Group.Id,
  SET:      Option[GroupPropertiesInput.Edit],
)

object CloneGroupInput {

 val Binding: Matcher[CloneGroupInput] =
    ObjectFieldsBinding.rmap {
      case List(
        GroupIdBinding("groupId", rGroupId),
        GroupPropertiesInput.EditBinding.Option("SET", rSET),
      ) =>
        (rGroupId, rSET).mapN(CloneGroupInput.apply)
    }

}