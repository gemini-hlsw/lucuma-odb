// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.ChronTooTriggerUpdateTable
import lucuma.odb.graphql.table.TooTriggerTable
import lucuma.odb.graphql.table.UserTable

trait TooTriggerChronicleEntryMapping[F[_]] extends ChronTooTriggerUpdateTable[F]
                                              with TooTriggerTable[F]
                                              with UserTable[F]:

  lazy val TooTriggerChronicleEntryMapping: ObjectMapping =
    ObjectMapping(TooTriggerChronicleEntryType)(
      SqlField("id",            ChronTooTriggerUpdateTable.ChronId, key = true),
      SqlField("transactionId", ChronTooTriggerUpdateTable.TransactionId),
      SqlObject("user",         Join(ChronTooTriggerUpdateTable.UserId, UserTable.UserId)),
      SqlField("timestamp",     ChronTooTriggerUpdateTable.Timestamp),
      SqlField("operation",     ChronTooTriggerUpdateTable.OperationId),

      SqlObject("tooTrigger",   Join(ChronTooTriggerUpdateTable.TooTriggerId, TooTriggerTable.Id)),

      SqlField("modObservationId",    ChronTooTriggerUpdateTable.Mod.ObservationId),
      SqlField("modProgramId",        ChronTooTriggerUpdateTable.Mod.ProgramId),
      SqlField("modStatus",           ChronTooTriggerUpdateTable.Mod.Status),
      SqlField("modResolutionReason", ChronTooTriggerUpdateTable.Mod.ResolutionReason),
      SqlField("modTooActivation",    ChronTooTriggerUpdateTable.Mod.TooActivation),
      SqlField("modSupersedes",       ChronTooTriggerUpdateTable.Mod.Supersedes),

      SqlField("newObservationId",    ChronTooTriggerUpdateTable.New.ObservationId),
      SqlField("newProgramId",        ChronTooTriggerUpdateTable.New.ProgramId),
      SqlField("newStatus",           ChronTooTriggerUpdateTable.New.Status),
      SqlField("newResolutionReason", ChronTooTriggerUpdateTable.New.ResolutionReason),
      SqlField("newTooActivation",    ChronTooTriggerUpdateTable.New.TooActivation),
      SqlField("newSupersedes",       ChronTooTriggerUpdateTable.New.Supersedes)
    )
