// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gmos.longslit

// TODO: Delete me?

/**
 * Science atoms consist of a science exposure and a corresponding flat, but
 * not necessarily in that order.  StepOrder specifies the correct order.
 */
enum StepOrder:
  def next: StepOrder =
    this match {
      case ScienceThenFlat => FlatThenScience
      case FlatThenScience => ScienceThenFlat
    }
  case ScienceThenFlat, FlatThenScience

