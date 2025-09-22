// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client.json

import eu.timepit.refined.types.numeric.PosBigDecimal
import io.circe.*
import io.circe.syntax.*

given Encoder[PosBigDecimal] with
  def apply(pbd: PosBigDecimal): Json =
    pbd.value.asJson
