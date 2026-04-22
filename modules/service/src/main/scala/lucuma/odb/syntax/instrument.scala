// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.syntax

import lucuma.core.enums.Instrument
import lucuma.core.enums.Site
import lucuma.core.data.Metadata
import java.time.Instant

trait ToInstrumentOps {

  extension (self: Instrument) {
    def fieldName: String =
      self.tag.updated(0, self.tag.charAt(0).toLower)

    def site: Site =
      Metadata
        .placeholder
        .availability(self)
        .siteForInstant(Instant.now())
        .get // TODO: this is all bad, but for now we're ok
  }

}

object instrument extends ToInstrumentOps
