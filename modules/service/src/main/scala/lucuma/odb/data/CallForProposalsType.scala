// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.util.Enumerated

enum CallForProposalsType(val tag: String) {
  case DemoScience        extends CallForProposalsType("demo_science")
  case DirectorsTime      extends CallForProposalsType("directors_time")
  case FastTurnaround     extends CallForProposalsType("fast_turnaround")
  case LargeProgram       extends CallForProposalsType("large_program")
  case PoorWeather        extends CallForProposalsType("poor_weather")
  case RegularSemester    extends CallForProposalsType("regular_semester")
  case SystemVerification extends CallForProposalsType("system_verification")
}

object CallForProposalsType {

  given Enumerated[CallForProposalsType] = Enumerated.derived

}