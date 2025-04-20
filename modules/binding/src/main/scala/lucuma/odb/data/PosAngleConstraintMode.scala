// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.util.Enumerated

// There is something similar in lucuma-core.  It needs to be updated and then
// we can switch to that.

enum PosAngleConstraintMode(val dbTag: String):
  case Unbounded           extends PosAngleConstraintMode("unbounded")
  case Fixed               extends PosAngleConstraintMode("fixed")
  case AllowFlip           extends PosAngleConstraintMode("allow_flip")
  case AverageParallactic  extends PosAngleConstraintMode("average_parallactic")
  case ParallacticOverride extends PosAngleConstraintMode("parallactic_override")

object PosAngleConstraintMode:

  val Default: PosAngleConstraintMode =
    Unbounded

  given Enumerated[PosAngleConstraintMode] =
    Enumerated.from(
      Unbounded,
      Fixed,
      AllowFlip,
      AverageParallactic,
      ParallacticOverride
    ).withTag(_.dbTag)

