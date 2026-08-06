// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.TypeRef
import lucuma.odb.graphql.table.TooTriggerTable

// One result type per single-trigger mutation (foo -> FooInput -> FooResult),
// each wrapping the affected trigger.  They share the same shape via this helper.
trait TooTriggerResultMappings[F[_]] extends TooTriggerTable[F]:

  private def resultMapping(tpe: TypeRef): ObjectMapping =
    ObjectMapping(tpe)(
      SqlField("id", TooTriggerTable.Id, key = true, hidden = true),
      SqlObject("tooTrigger")
    )

  lazy val RequestTooTriggerResultMapping  = resultMapping(RequestTooTriggerResultType)
  lazy val WithdrawTooTriggerResultMapping = resultMapping(WithdrawTooTriggerResultType)
  lazy val AcceptTooTriggerResultMapping   = resultMapping(AcceptTooTriggerResultType)
  lazy val DenyTooTriggerResultMapping     = resultMapping(DenyTooTriggerResultType)
