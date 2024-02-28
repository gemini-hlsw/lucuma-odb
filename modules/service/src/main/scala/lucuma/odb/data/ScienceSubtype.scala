// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.syntax.eq.*
import lucuma.core.util.Enumerated

enum ScienceSubtype(val tag: String, val letter: Char) derives Enumerated:
  case Classical          extends ScienceSubtype("classical", 'C')
  case DirectorsTime      extends ScienceSubtype("directors_time", 'D')
  case FastTurnaround     extends ScienceSubtype("fast_turnaround", 'F')
  case LargeProgram       extends ScienceSubtype("large_program", 'L')
  case Queue              extends ScienceSubtype("queue", 'Q')
  case DemoScience        extends ScienceSubtype("demo_science", 'S')
  case SystemVerification extends ScienceSubtype("system_verification", 'V')

object ScienceSubtype {

  def fromLetter(c: Char): Option[ScienceSubtype] =
    values.find(_.letter === c)

}