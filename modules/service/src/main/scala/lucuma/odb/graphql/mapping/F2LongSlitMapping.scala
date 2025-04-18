// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.skunk.SkunkMapping
import lucuma.core.enums.F2ReadMode
import lucuma.core.enums.F2Reads
import lucuma.odb.graphql.table.*

trait F2LongSlitMapping[F[_]]
  extends F2LongSlitView[F] with OptionalFieldMapping[F] { this: SkunkMapping[F] =>

  lazy val F2LongSlitMapping: ObjectMapping =
    ObjectMapping(F2LongSlitType)(

      SqlField("observationId", F2LongSlitView.ObservationId, key = true, hidden = true),

      SqlField("disperser", F2LongSlitView.Disperser),
      SqlField("filter",  F2LongSlitView.Filter),
      SqlField("fpu",     F2LongSlitView.Fpu),

      explicitOrElseDefault[F2ReadMode]("readMode", "explicitReadMode", "defaultReadMode"),
      SqlField("defaultReadMode", F2LongSlitView.ReadModeDefault),
      SqlField("explicitReadMode", F2LongSlitView.ReadMode),

      explicitOrElseDefault[F2Reads]("reads", "explicitReads", "defaultReads"),
      SqlField("defaultReads", F2LongSlitView.ReadsDefault),
      SqlField("explicitReads", F2LongSlitView.Reads),

      SqlField("decker",     F2LongSlitView.Decker),
      SqlField("readoutMode",     F2LongSlitView.ReadoutMode),

      SqlField("initialDisperser", F2LongSlitView.InitialDisperser),
      SqlField("initialFilter",  F2LongSlitView.InitialFilter),
      SqlField("initialFpu",     F2LongSlitView.InitialFpu),

    )

}
