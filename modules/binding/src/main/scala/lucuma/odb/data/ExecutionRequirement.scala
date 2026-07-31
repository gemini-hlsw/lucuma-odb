// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.util.Enumerated

/**
 * How the Scheduler is permitted to treat an observation once it is running.
 * The values form a chain of increasing restriction: `Unconstrained` grants
 * the scheduler every freedom, each subsequent value withdraws one.
 *
 * The legal states are exactly this chain because splittability subsumes
 * interruptibility: an observation willing to be delivered across separate
 * visits is necessarily willing to be stopped between them.  So the nonsensical
 * "splittable but not interruptible" combination is simply unrepresentable.
 *
 * Defined here in the odb project for now; move to lucuma-core once the design
 * settles.
 */
enum ExecutionRequirement(
  val tag:             String,
  val isSplittable:    Boolean,
  val isInterruptible: Boolean
) derives Enumerated:

  /** Normal: the sequence may be split across visits and interrupted. */
  case Unconstrained extends ExecutionRequirement("unconstrained", true, true)

  /** The sequence may be interrupted, but not planned across multiple visits. */
  case Contiguous extends ExecutionRequirement("contiguous", false, true)

  /** The sequence must run start-to-finish in one uninterrupted visit. */
  case Uninterruptible extends ExecutionRequirement("uninterruptible", false, false)