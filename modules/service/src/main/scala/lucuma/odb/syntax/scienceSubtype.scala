// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.syntax

import cats.syntax.eq.*
import lucuma.core.enums.GeminiCallForProposalsType
import lucuma.core.enums.ScienceSubtype

trait ToScienceSubtypeOps {

  import ScienceSubtype.*

  extension (s: ScienceSubtype)
    def title: String =
      s match
        case Classical          => "Classical"
        case DemoScience        => "Demo Science"
        case DirectorsTime      => "Director's Time"
        case FastTurnaround     => "Fast Turnaround"
        case LargeProgram       => "Large Program"
        case PoorWeather        => "Poor Weather"
        case Queue              => "Queue"
        case SystemVerification => "System Verification"

    def callType: GeminiCallForProposalsType =
      s match
        case Classical | Queue  => GeminiCallForProposalsType.RegularSemester
        case DemoScience        => GeminiCallForProposalsType.DemoScience
        case DirectorsTime      => GeminiCallForProposalsType.DirectorsTime
        case FastTurnaround     => GeminiCallForProposalsType.FastTurnaround
        case LargeProgram       => GeminiCallForProposalsType.LargeProgram
        case PoorWeather        => GeminiCallForProposalsType.PoorWeather
        case SystemVerification => GeminiCallForProposalsType.SystemVerification

    def isCompatibleWith(c: GeminiCallForProposalsType): Boolean =
      callType === c

}

object scienceSubtype extends ToScienceSubtypeOps
