// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.syntax.eq.*
import lucuma.core.util.Enumerated

enum ScienceType(val tag: String, val letter: Char) derives Enumerated:
  case Classical          extends ScienceType("classical", 'C')
  case DirectorsTime      extends ScienceType("directors_time", 'D')
  case FastTurnaround     extends ScienceType("fast_turnaround", 'F')
  case LargeProgram       extends ScienceType("large_program", 'L')
  case Queue              extends ScienceType("queue", 'Q')
  case DemoScience        extends ScienceType("demo_science", 'S')
  case SystemVerification extends ScienceType("system_verification", 'V')

object ScienceType {

  def fromLetter(c: Char): Option[ScienceType] =
    values.find(_.letter === c)

}