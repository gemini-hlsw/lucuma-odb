// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.syntax

import lucuma.core.enums.Instrument
import lucuma.core.enums.Site

trait ToInstrumentOps {

  extension (self: Instrument) {
    def fieldName: String =
      self.tag.updated(0, self.tag.charAt(0).toLower)

    def site: Set[Site] =
      self match {
        case Instrument.AcqCam     => Set(Site.GN, Site.GS)
        case Instrument.Bhros      => Set(Site.GS)
        case Instrument.Flamingos2 => Set(Site.GS)
        case Instrument.Ghost      => Set(Site.GS)
        case Instrument.GmosNorth  => Set(Site.GN)
        case Instrument.GmosSouth  => Set(Site.GS)
        case Instrument.Gnirs      => Set(Site.GN)
        case Instrument.Gpi        => Set(Site.GN)
        case Instrument.Gsaoi      => Set(Site.GS)
        case Instrument.Igrins2    => Set(Site.GN)
        case Instrument.Michelle   => Set(Site.GN)
        case Instrument.Nici       => Set(Site.GN)
        case Instrument.Nifs       => Set(Site.GN)
        case Instrument.Niri       => Set(Site.GN)
        case Instrument.Phoenix    => Set(Site.GN)
        case Instrument.Trecs      => Set(Site.GS)
        case Instrument.Visitor    => Set(Site.GN, Site.GS)
        case Instrument.Scorpio    => Set(Site.GS)
        case Instrument.Alopeke    => Set(Site.GN)
        case Instrument.Zorro      => Set(Site.GS)
      }
  }

}

object instrument extends ToInstrumentOps
