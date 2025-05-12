// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.Flamingos2DynamicTable

trait Flamingos2DynamicMapping[F[_]] extends Flamingos2DynamicTable[F]:

  lazy val Flamingos2DynamicMapping: TypeMapping =
    ObjectMapping(StepRecordType / "flamingos2")(
      SqlField("id",          Flamingos2DynamicTable.Id, key = true, hidden = true),
      SqlObject("exposure"),
      SqlField("disperser",   Flamingos2DynamicTable.Disperser),
      SqlField("filter",      Flamingos2DynamicTable.Filter),
      SqlField("readMode",    Flamingos2DynamicTable.ReadMode),
      SqlField("lyotWheel",   Flamingos2DynamicTable.LyotWheel),
      SqlObject("mask"),
      SqlField("readoutMode", Flamingos2DynamicTable.ReadoutMode),
      SqlField("reads",       Flamingos2DynamicTable.Reads)
    )