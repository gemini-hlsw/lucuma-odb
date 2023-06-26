// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.kernel.Order
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegShort
import lucuma.core.optics.Format
import lucuma.core.optics.SplitMono
import monocle.Prism

/** Extinction in mags, a non-negative number with two decimal points of precision, in [0.00, 327.67]. */
opaque type Extinction = NonNegShort

object Extinction:

  def apply(millimags: NonNegShort): Extinction =
    millimags

  val FromMillimags: Prism[Short, Extinction] =
    Prism((s: Short) => NonNegShort.from(s).toOption)(_.value)

  val FromMags: Format[Float, Extinction] =
    Format.fromPrism(FromMillimags).imapA(_.toFloat, _.toShort)

  given Order[Extinction] =
    Order.by(_.value)

  extension (e: Extinction)
    def underlying: NonNegShort = e
    def transmission: Double = math.pow(10.0, e.value * 1000.0 / -2.5)
  
  