// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.Site
import lucuma.odb.graphql.binding.*

object WhereImagingConfigOption {

  def binding(path: Path): Matcher[Predicate] = {
    val WhereAdaptiveOpt  = WhereBoolean.binding(path / "adaptiveOptics", BooleanBinding)
    val WhereInstrument   = WhereEq.binding[Instrument](path / "instrument", InstrumentBinding)
    val WhereSite         = WhereEq.binding[Site](path / "site", SiteBinding)
    val WhereFov          = WhereAngle.binding(path / "fov")

    lazy val WhereConfigBinding = binding(path)

    ObjectFieldsBinding.rmap {
      case List(
        WhereConfigBinding.List.Option("AND", rAND),
        WhereConfigBinding.List.Option("OR", rOR),
        WhereConfigBinding.Option("NOT", rNOT),

        WhereAdaptiveOpt.Option("adaptiveOptics", rAdp),
        WhereInstrument.Option("instrument", rIns),
        WhereFov.Option("fov", rFov),
        WhereSite.Option("site", rSte),

      ) =>
        (rAND, rOR, rNOT, rAdp, rIns, rFov, rSte).parMapN {
          (AND, OR, NOT, adp, ins, fov, ste) =>
            and(List(
              AND.map(and),
              OR.map(or),
              NOT.map(Not(_)),
              adp,
              ins,
              fov,
              ste
            ).flatten)
        }
    }
  }

}
