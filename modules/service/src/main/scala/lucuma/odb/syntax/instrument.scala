// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.syntax

import lucuma.core.enums.Instrument

trait ToInstrumentOps {

  extension (self: Instrument) {
    def fieldName: String =
      self.tag.updated(0, self.tag.charAt(0).toLower)
  }

}

object instrument extends ToInstrumentOps
