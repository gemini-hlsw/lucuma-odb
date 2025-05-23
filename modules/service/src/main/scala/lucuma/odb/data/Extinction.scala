// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.kernel.Order
import eu.timepit.refined.types.numeric.NonNegShort
import io.circe.Decoder
import io.circe.Encoder
import lucuma.core.optics.Format
import monocle.Prism

/** 
 * Extinction in mags, a non-negative number with two decimal points of precision, 
 * in [0.00, 327.67].
 */
opaque type Extinction = NonNegShort

object Extinction:

  def apply(millimags: NonNegShort): Extinction =
    millimags

  val FromMillimags: Prism[Short, Extinction] =
    Prism((s: Short) => NonNegShort.from(s).toOption)(_.value)

  val FromMags: Format[Double, Extinction] =
    Format(
      d => if d.isNaN || d.isInfinite then None else FromMillimags.getOption(BigDecimal(d).bigDecimal.movePointRight(2).shortValue),
      e => BigDecimal(FromMillimags.reverseGet(e)).bigDecimal.movePointLeft(2).doubleValue 
    )

  given Order[Extinction] =
    Order.by(_.value)

  given Encoder[Extinction] =
    Encoder[Double].contramap(FromMags.reverseGet)

  given Decoder[Extinction] =
    Decoder[Double].emap(d => FromMags.getOption(d).toRight(s"Invalid extinction: $d"))

  extension (e: Extinction)
    def underlying: NonNegShort = e
    def transmission: Double = math.pow(10.0, e.value * 1000.0 / -2.5)
  
  