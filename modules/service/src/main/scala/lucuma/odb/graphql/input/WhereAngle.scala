// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.odb.graphql.binding.*


object WhereAngle {

  def binding(path: Path): Matcher[Predicate] = {
    val MicroarcsecondsBinding = WhereOrder.binding(path / "microarcseconds", LongBinding)
    val MicrosecondsBinding    = WhereOrder.binding(path / "microseconds",    BigDecimalBinding)

    val MilliarcsecondsBinding = WhereOrder.binding(path / "milliarcseconds", BigDecimalBinding)
    val MillisecondsBinding    = WhereOrder.binding(path / "milliseconds",    BigDecimalBinding)

    val ArcsecondsBinding      = WhereOrder.binding(path / "arcseconds",      BigDecimalBinding)
    val SecondsBinding         = WhereOrder.binding(path / "seconds",         BigDecimalBinding)

    val ArcminutesBinding      = WhereOrder.binding(path / "arcminutes",      BigDecimalBinding)
    val MinutesBinding         = WhereOrder.binding(path / "minutes",         BigDecimalBinding)

    val DegreesBinding         = WhereOrder.binding(path / "degrees",         BigDecimalBinding)
    val HoursBinding           = WhereOrder.binding(path / "hours",           BigDecimalBinding)

    lazy val WhereAngleBinding = binding(path)

    ObjectFieldsBinding.rmap {
      case List(
        WhereAngleBinding.List.Option("AND", rAND),
        WhereAngleBinding.List.Option("OR", rOR),
        WhereAngleBinding.Option("NOT", rNOT),

        MicroarcsecondsBinding.Option("microarcseconds", rMicroarcseconds),
        MicrosecondsBinding.Option("microseconds",       rMicroseconds),

        MilliarcsecondsBinding.Option("milliarcseconds", rMilliarcseconds),
        MillisecondsBinding.Option("milliseconds",       rMilliseconds),

        ArcsecondsBinding.Option("arcseconds",           rArcseconds),
        SecondsBinding.Option("seconds",                 rSeconds),

        ArcminutesBinding.Option("arcminutes",           rArcminutes),
        MinutesBinding.Option("minutes",                 rMinutes),

        DegreesBinding.Option("degrees",                 rDegrees),
        HoursBinding.Option("hours",                     rHours)
      ) =>
        (rAND, rOR, rNOT, rMicroarcseconds, rMicroseconds, rMilliarcseconds, rMilliseconds, rArcseconds, rSeconds, rArcminutes, rMinutes, rDegrees, rHours).parMapN {
          (AND, OR, NOT, microarcseconds, microseconds, milliarcseconds, milliseconds, arcseconds, seconds, arcminutes, minutes, degrees, hours) =>
            and(List(
              AND.map(and),
              OR.map(or),
              NOT.map(Not(_)),
              microarcseconds,
              microseconds,
              milliarcseconds,
              milliseconds,
              arcseconds,
              seconds,
              arcminutes,
              minutes,
              degrees,
              hours
            ).flatten)
        }
    }
  }

}
