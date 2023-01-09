// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos
package longslit


import eu.timepit.refined.types.numeric.PosDouble
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.ImageQuality
import lucuma.core.math.WavelengthDither
import lucuma.core.model.SourceProfile
import lucuma.core.model.arb.ArbSourceProfile
import lucuma.core.util.arb.ArbEnumerated
import lucuma.odb.sequence.data.SciExposureTime
import lucuma.odb.sequence.gmos.longslit.arb.ArbGmosLongSlitConfig
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*

import java.time.Duration

class ScienceSuite extends ScalaCheckSuite {

  import ArbEnumerated.*
  import ArbGmosLongSlitConfig.given
  import ArbSourceProfile.given

  val tenMin: SciExposureTime =
    SciExposureTime.fromDuration(Duration.ofMinutes(10)).get


  property("cycles through wavelength dithers") {

    forAll { (ls: GmosLongSlitConfig.North, sp: SourceProfile, iq: ImageQuality) =>
      val sz = 20
      val as = Science.GmosNorth.compute(ls, tenMin, sp, iq, PosDouble.unsafeFrom(2.0))

      val actual   = as.map(a => DynamicOptics.North.wavelength.getOption(a.science.instrumentConfig).get).take(sz)
      val expected = (ls.wavelengthDithers match {
        case Nil => LazyList.continually(WavelengthDither.Zero)
        case ds  => LazyList.continually(ds.to(LazyList)).flatten
      }).take(sz).map { d => ls.centralWavelength.offset(d).getOrElse(ls.centralWavelength) }

      assertEquals(actual.toList, expected.toList)
    }
  }

}
