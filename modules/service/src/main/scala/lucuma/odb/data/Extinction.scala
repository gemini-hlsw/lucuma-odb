// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import eu.timepit.refined.types.numeric.NonNegShort
import lucuma.core.optics.SplitMono
import lucuma.core.optics.Format

/** Extinction in mags, a non-negative number with two decimal points of precision, in [0.00, 327.67]. */
opaque type Extinction = NonNegShort

object Extinction:

  val FromMillimags: Format[Short, Extinction] =
    Format(NonNegShort.from(_).toOption, _.value)

  val FromMags: Format[Float, Extinction] =
    FromMillimags.imapA(_ / 1000f, f => (f * 1000f).toShort)

  