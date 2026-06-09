// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString

object UnsplittableAtom:

  /**
   * Default title for the single atom that appears in the science sequence of
   * an unsplittable observation.
   */
  val Description: NonEmptyString =
    NonEmptyString.unsafeFrom("Unsplittable Science Atom")

  /**
   * Size limit for the number of steps in the single atom of an unsplittable
   * observation.
   */
  val StepLimit: PosInt =
    PosInt.unsafeFrom(2048)