// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import lucuma.itc.SignificantFigures
import lucuma.odb.graphql.binding.*

object SignificantFiguresInput:

  val Binding: Matcher[SignificantFigures] =
    ObjectFieldsBinding.rmap:
      case List(
            PosIntBinding.Option("xAxis", xAxis),
            PosIntBinding.Option("yAxis", yAxis),
            PosIntBinding.Option("ccd", ccd)
          ) =>
        (xAxis, yAxis, ccd).parMapN(SignificantFigures.apply)
