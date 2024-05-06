// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.util.Enumerated

enum CallForProposalsType(val tag: String, val title: String) {
  case DemoScience        extends CallForProposalsType("demo_science", "Demo Science")
  case DirectorsTime      extends CallForProposalsType("directors_time", "Director's Time")
  case FastTurnaround     extends CallForProposalsType("fast_turnaround", "Fast Turnaround")
  case LargeProgram       extends CallForProposalsType("large_program", "Large Program")
  case PoorWeather        extends CallForProposalsType("poor_weather", "Poor Weather")
  case RegularSemester    extends CallForProposalsType("regular_semester", "Regular Semester")
  case SystemVerification extends CallForProposalsType("system_verification", "System Verification")
}

object CallForProposalsType {

  given Enumerated[CallForProposalsType] = Enumerated.derived

}