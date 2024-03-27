// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
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
    val WhereCapabilities = WhereOptionEq.binding[SpectroscopyCapabilities](path / "capability", SpectroscopyCapabilitiesBinding)
    val WhereFocalPlane   = WhereEq.binding[FocalPlane](path / "focalPlane", FocalPlaneBinding)
    val WhereInstrument   = WhereEq.binding[Instrument](path / "instrument", InstrumentBinding)
    val WhereResolution   = WhereOrder.binding[PosInt](path / "resolution", PosIntBinding)
    val WhereSite         = WhereEq.binding[Site](path / "site", SiteBinding)
    val WhereSlitLength   = WhereAngle.binding(path / "slitLength")
    val WhereSlitWidth    = WhereAngle.binding(path / "slitWidth")

    ObjectFieldsBinding.rmap {
      case List(
        WhereCapabilities.Option("capability", rCap),
        WhereFocalPlane.Option("focalPlane", rFoc),
        WhereInstrument.Option("instrument", rIns),
        WhereResolution.Option("resolution", rRes),
        WhereSite.Option("site", rSte),
        WhereSlitLength.Option("slitLength", rLen),
        WhereSlitWidth.Option("slitWidth", rWid)
      ) =>
        (rCap, rFoc, rIns, rRes, rSte, rLen, rWid).parMapN {
          (cap, foc, ins, res, ste, len, wid) =>
            and(List(
              cap,
              foc,
              ins,
              res,
              ste,
              len,
              wid
            ).flatten)
        }
    }
  }

//  site: WhereEqSite
//  wavelength: WavelengthInput
//  wavelengthCoverage: WavelengthInput

}
