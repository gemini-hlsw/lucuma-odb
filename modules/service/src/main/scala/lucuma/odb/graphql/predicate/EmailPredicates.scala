// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import cats.syntax.option.*
import cats.syntax.order.*
import eu.timepit.refined.cats.refTypeOrder
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import grackle.Query.OrderSelection
import grackle.Query.OrderSelections
import lucuma.core.data.EmailAddress
import lucuma.core.model.Access.Staff
import lucuma.core.model.User
import lucuma.core.util.Timestamp
import lucuma.odb.data.EmailId

class EmailPredicates(path: Path):
  val id = LeafPredicates[EmailId](path / "id")

  /** Oldest first, then by recipient, so that a single request's emails have a stable order. */
  val order: OrderSelections =
    OrderSelections(List(
      OrderSelection[Timestamp](path / "originalTime"),
      OrderSelection[EmailAddress](path / "recipientEmail"),
      OrderSelection[EmailId](path / "id")
    ))

  /** Emails are visible to the program's PI and to staff. */
  def isVisibleTo(user: User): Predicate =
    if user.role.access >= Staff then True
    else Eql(path / "piUserId", Const(user.id.some))
