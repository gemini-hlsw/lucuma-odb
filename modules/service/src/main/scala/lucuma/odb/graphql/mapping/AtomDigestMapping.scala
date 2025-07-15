// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.AtomDigestTable

trait AtomDigestMapping[F[_]] extends AtomDigestTable[F]:

  lazy val AtomDigestMapping: ObjectMapping =
    ObjectMapping(AtomDigestType)(
      SqlField("id",            AtomDigestTable.AtomId,        key    = true),
      SqlField("index",         AtomDigestTable.AtomIndex,     hidden = true),
      SqlField("observationId", AtomDigestTable.ObservationId, hidden = true),

      SqlField("observeClass",  AtomDigestTable.ObserveClass),
      SqlObject("timeEstimate"),
      SqlField("stepTypes",     AtomDigestTable.StepTypes),
      SqlField("lampTypes",     AtomDigestTable.LampTypes)
    )