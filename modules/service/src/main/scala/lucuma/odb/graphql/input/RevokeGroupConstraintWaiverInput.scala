// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.group.GroupConstraintWaiver
import lucuma.odb.graphql.binding.*

case class RevokeGroupConstraintWaiverInput(
  waiverId: GroupConstraintWaiver.Id,
  reason:   Option[NonEmptyString]
)

object RevokeGroupConstraintWaiverInput:

  private val WaiverIdBinding = gidBinding[GroupConstraintWaiver.Id]("group constraint waiver")

  val Binding: Matcher[RevokeGroupConstraintWaiverInput] =
    ObjectFieldsBinding.rmap:
      case List(
        WaiverIdBinding("waiverId", rId),
        NonEmptyStringBinding.Option("reason", rReason)
      ) =>
        (rId, rReason).parMapN(apply)
