// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.Flamingos2DynamicView

trait Flamingos2DynamicMapping[F[_]] extends Flamingos2DynamicView[F]:

  lazy val Flamingos2DynamicMapping: TypeMapping =
    ObjectMapping(StepRecordType / "flamingos2")(
      SqlField("id",          Flamingos2DynamicView.Id, key = true, hidden = true),
      SqlObject("exposure"),
      SqlField("disperser",   Flamingos2DynamicView.Disperser),
      SqlField("filter",      Flamingos2DynamicView.Filter),
      SqlField("readMode",    Flamingos2DynamicView.ReadMode),
      SqlField("lyotWheel",   Flamingos2DynamicView.LyotWheel),
      SqlObject("fpu"),
      SqlField("decker",      Flamingos2DynamicView.Decker),
      SqlField("readoutMode", Flamingos2DynamicView.ReadoutMode),
      SqlField("reads",       Flamingos2DynamicView.Reads)
    )