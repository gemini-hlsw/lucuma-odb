// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.enums.FocalPlane
import lucuma.core.enums.Instrument
import lucuma.core.enums.Site
import lucuma.core.enums.SpectroscopyCapabilities
import lucuma.odb.graphql.binding.*

object WhereSpectroscopyConfigOption {

  def binding(path: Path): Matcher[Predicate] = {
    val WhereAdaptiveOpt  = WhereBoolean.binding(path / "adaptiveOptics", BooleanBinding)
    val WhereCapabilities = WhereOptionEq.binding[SpectroscopyCapabilities](path / "capability", SpectroscopyCapabilitiesBinding)
    val WhereFocalPlane   = WhereEq.binding[FocalPlane](path / "focalPlane", FocalPlaneBinding)
    val WhereInstrument   = WhereEq.binding[Instrument](path / "instrument", InstrumentBinding)
    val WhereResolution   = WhereOrder.binding[PosInt](path / "resolution", PosIntBinding)
    val WhereSite         = WhereEq.binding[Site](path / "site", SiteBinding)
    val WhereSlitLength   = WhereAngle.binding(path / "slitLength")
    val WhereSlitWidth    = WhereAngle.binding(path / "slitWidth")
    val RangeIncludes     = WavelengthInput.Binding.map { w =>
      and(List(
        LtEql(path / "wavelengthMin" / "picometers", Const(w.toPicometers.value)),
        GtEql(path / "wavelengthMax" / "picometers", Const(w.toPicometers.value))
      ))
    }
    val WhereOptimal      = WhereWavelength.binding(path / "wavelengthOptimal")
    val WhereCoverage     = WhereWavelength.binding(path / "wavelengthCoverage")

    lazy val WhereConfigBinding = binding(path)

    ObjectFieldsBinding.rmap {
      case List(
        WhereConfigBinding.List.Option("AND", rAND),
        WhereConfigBinding.List.Option("OR", rOR),
        WhereConfigBinding.Option("NOT", rNOT),

        WhereAdaptiveOpt.Option("adaptiveOptics", rAdp),
        WhereCapabilities.Option("capability", rCap),
        WhereFocalPlane.Option("focalPlane", rFoc),
        WhereInstrument.Option("instrument", rIns),
        WhereResolution.Option("resolution", rRes),
        WhereSite.Option("site", rSte),
        WhereSlitLength.Option("slitLength", rLen),
        WhereSlitWidth.Option("slitWidth", rWid),
        RangeIncludes.Option("rangeIncludes", rRan),
        WhereOptimal.Option("wavelengthOptimal", rOpt),
        WhereCoverage.Option("wavelengthCoverage", rCov)

      ) =>
        (rAND, rOR, rNOT, rAdp, rCap, rFoc, rIns, rRes, rSte, rLen, rWid, rRan, rOpt, rCov).parMapN {
          (AND, OR, NOT, adp, cap, foc, ins, res, ste, len, wid, ran, opt, cov) =>
            and(List(
              AND.map(and),
              OR.map(or),
              NOT.map(Not(_)),
              adp,
              cap,
              foc,
              ins,
              res,
              ste,
              len,
              wid,
              ran,
              opt,
              cov
            ).flatten)
        }
    }
  }

}
