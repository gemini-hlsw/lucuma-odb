// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.syntax

import cats.syntax.option.*
import lucuma.core.enums.ExchangeObservingModeType
import lucuma.core.enums.FacilityObservingModeType
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.VisitorObservingModeType

trait ToObservingModeTypeOps:

  extension (self: ObservingModeType)

    /**
     * The Gemini instrument associated with this observing mode, if any.
     * Exchange modes (Keck/Subaru) are not Gemini instruments and have none.
     */
    def instrumentOption: Option[Instrument] =
      self match
        case f: FacilityObservingModeType => f.instrument.some
        case v: VisitorObservingModeType  => v.instrument.some
        case _: ExchangeObservingModeType => none

object observingModeType extends ToObservingModeTypeOps
