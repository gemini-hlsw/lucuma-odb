// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.syntax

import cats.syntax.eq.*
import lucuma.core.enums.CallForProposalsType
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

    def callType: CallForProposalsType =
      s match
        case Classical | Queue  => CallForProposalsType.RegularSemester
        case DemoScience        => CallForProposalsType.DemoScience
        case DirectorsTime      => CallForProposalsType.DirectorsTime
        case FastTurnaround     => CallForProposalsType.FastTurnaround
        case LargeProgram       => CallForProposalsType.LargeProgram
        case PoorWeather        => CallForProposalsType.PoorWeather
        case SystemVerification => CallForProposalsType.SystemVerification

    def isCompatibleWith(c: CallForProposalsType): Boolean =
      callType === c

}

object scienceSubtype extends ToScienceSubtypeOps
