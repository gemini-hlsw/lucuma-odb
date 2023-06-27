// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.kernel.Order
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegShort
import io.circe.Encoder
import lucuma.core.optics.Format
import lucuma.core.optics.SplitMono
import monocle.Prism

import java.math.MathContext

/** Extinction in mags, a non-negative number with two decimal points of precision, in [0.00, 327.67]. */
opaque type Extinction = NonNegShort

object Extinction:

  def apply(millimags: NonNegShort): Extinction =
    millimags

  val FromMillimags: Prism[Short, Extinction] =
    Prism((s: Short) => NonNegShort.from(s).toOption)(_.value)

  val FromMags: Format[Double, Extinction] =
    Format.fromPrism(FromMillimags).imapA(
      s => BigDecimal(s).bigDecimal.movePointLeft(2).doubleValue, 
      d => BigDecimal(d).bigDecimal.movePointRight(2).shortValue
    )

  given Order[Extinction] =
    Order.by(_.value)

  given Encoder[Extinction] =
    Encoder[Double].contramap(FromMags.reverseGet)

  extension (e: Extinction)
    def underlying: NonNegShort = e
    def transmission: Double = math.pow(10.0, e.value * 1000.0 / -2.5)
  
  