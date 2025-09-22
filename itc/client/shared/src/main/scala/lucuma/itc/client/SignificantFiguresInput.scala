// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*

final case class SignificantFiguresInput(
  xAxis: PosInt,
  yAxis: PosInt,
  ccd:   PosInt
)

object SignificantFiguresInput {

  given Encoder[SignificantFiguresInput] with
    def apply(a: SignificantFiguresInput): Json =
      Json
        .obj(
          "xAxis" -> a.xAxis.value.asJson,
          "yAxis" -> a.yAxis.value.asJson,
          "ccd"   -> a.ccd.value.asJson
        )
        .dropNullValues

  given Eq[SignificantFiguresInput] =
    Eq.by { a =>
      (
        a.xAxis,
        a.yAxis,
        a.ccd
      )
    }
}
