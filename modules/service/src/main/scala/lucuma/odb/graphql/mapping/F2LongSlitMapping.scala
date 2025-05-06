// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.skunk.SkunkMapping
import lucuma.core.enums.F2Decker
import lucuma.core.enums.F2ReadoutMode
import lucuma.odb.graphql.table.*

trait F2LongSlitMapping[F[_]]
  extends F2LongSlitView[F] with OptionalFieldMapping[F] { this: SkunkMapping[F] =>

  lazy val F2LongSlitMapping: ObjectMapping =
    ObjectMapping(F2LongSlitType)(

      SqlField("observationId", F2LongSlitView.ObservationId, key = true, hidden = true),

      SqlField("disperser", F2LongSlitView.Disperser),
      SqlField("filter",    F2LongSlitView.Filter),
      SqlField("fpu",       F2LongSlitView.Fpu),

      SqlField("explicitReadMode", F2LongSlitView.ReadMode),
      SqlField("explicitReads", F2LongSlitView.Reads),

      explicitOrElseDefault[F2Decker]("decker", "explicitDecker", "defaultDecker"),
      SqlField("defaultDecker",  F2LongSlitView.DeckerDefault),
      SqlField("explicitDecker", F2LongSlitView.Decker),

      explicitOrElseDefault[F2ReadoutMode]("readoutMode", "explicitReadoutMode", "defaultReadoutMode"),
      SqlField("defaultReadoutMode",  F2LongSlitView.ReadoutModeDefault),
      SqlField("explicitReadoutMode", F2LongSlitView.ReadoutMode),

      SqlField("initialDisperser", F2LongSlitView.InitialDisperser),
      SqlField("initialFilter",    F2LongSlitView.InitialFilter),
      SqlField("initialFpu",       F2LongSlitView.InitialFpu),

    )

}
