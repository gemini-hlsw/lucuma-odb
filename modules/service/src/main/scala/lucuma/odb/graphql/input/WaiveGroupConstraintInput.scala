// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.Group
import lucuma.core.model.group.GroupConstraintViolation.Waivable
import lucuma.odb.graphql.binding.*

case class WaiveGroupConstraintInput(
  groupId:   Group.Id,
  violation: Waivable,
  reason:    Option[NonEmptyString]
)

object WaiveGroupConstraintInput:

  val Binding: Matcher[WaiveGroupConstraintInput] =
    ObjectFieldsBinding.rmap:
      case List(
        GroupIdBinding("groupId", rGroupId),
        GroupConstraintViolationInput.Binding("violation", rViolation),
        NonEmptyStringBinding.Option("reason", rReason)
      ) =>
        (rGroupId, rViolation, rReason).parMapN(apply)
