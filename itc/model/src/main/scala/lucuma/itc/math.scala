// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.math

import scala.math.*

// Round the value to n significant figures, this to remove
// the extra precision produced by ITC calculations using doubles
// This will not work for extremely small numbers but we don't expect such
def roundToSignificantFigures(num: BigDecimal, n: Int): Double =
  if num == 0 then 0
  else if num.isExactDouble then num.toDouble
  else
    val d     = ceil(log10(abs(num.toDouble)))
    val power = n - d.toInt

    val magnitude = pow(10, power)
    val shifted   = round(num.toDouble * magnitude)
    shifted / magnitude
