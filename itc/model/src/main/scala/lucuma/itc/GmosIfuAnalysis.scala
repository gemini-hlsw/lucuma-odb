// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.Eq
import lucuma.core.math.Angle

/**
 * How to sample a GMOS IFU field. The number of fibres on sky is deliberately not part of this: it
 * follows from the focal plane unit, so the sampling geometry is all the caller chooses.
 */
enum GmosIfuAnalysis:
  /** Sum every IFU element whose centre falls within `radius` of the field centre. */
  case Sum(radius: Angle)

  /** Measure the single IFU element sitting `offset` from the field centre. */
  case Single(offset: Angle)

object GmosIfuAnalysis:

  /**
   * One lenslet pitch: 0.186" element diameter plus 0.014" of dead space. A radius that small
   * encloses only the element on the field centre, so summing over it measures that one element.
   */
  val DefaultSumRadius: Angle = Angle.microarcseconds.reverseGet(200_000)

  val Default: GmosIfuAnalysis = Sum(DefaultSumRadius)

  given Eq[GmosIfuAnalysis] = Eq.by:
    case Sum(radius)    => (0, radius)
    case Single(offset) => (1, offset)
