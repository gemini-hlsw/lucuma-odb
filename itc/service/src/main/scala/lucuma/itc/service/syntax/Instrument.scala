// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.syntax

import lucuma.core.enums.Instrument

import scala.concurrent.duration.*

trait InstrumentSyntax:
  extension (self: Instrument)

    /** Minimum exposure time allowed by the instrument. Always positive. */
    def minExposureDuration: FiniteDuration =
      self match
        case Instrument.GmosNorth => 1.second
        case Instrument.GmosSouth => 1.second
        case i                    => sys.error(s"Minimum exposure time for $i is not know.")

    /**
     * Minimum exposure time allowed by the instrument. Always greater than `minExposureDuration`.
     */
    def maxExposureDuration: FiniteDuration =
      self match
        case Instrument.GmosNorth => 20.minutes
        case Instrument.GmosSouth => 20.minutes
        case i                    => sys.error(s"Maximum exposure time for $i is not know.")

    /** True if the instrument requires exposure times in integral seconds. */
    def integralDurations: Boolean =
      self match
        case Instrument.GmosNorth => true
        case Instrument.GmosSouth => true
        case i                    => sys.error(s"Integral durations for $i is not know.")

object instrument extends InstrumentSyntax
