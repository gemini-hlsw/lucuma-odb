// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos
package longslit

import eu.timepit.refined.types.numeric.PosDouble
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.ImageQuality
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.WavelengthDither
import lucuma.core.model.SourceProfile
import lucuma.core.model.arb.ArbSourceProfile
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.util.arb.ArbEnumerated
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.SciExposureTime
import lucuma.odb.sequence.gmos.longslit.arb.ArbGmosLongSlitConfig
import munit.Location
import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.*

import java.time.Duration

class ScienceSuite extends ScalaCheckSuite {

  import ArbEnumerated.*
  import ArbGmosLongSlitConfig.given
  import ArbSourceProfile.given

  val tenMin: SciExposureTime =
    SciExposureTime.fromDuration(Duration.ofMinutes(10)).get

  def northSequence(
    ls: Config.GmosNorth,
    sp: SourceProfile,
    iq: ImageQuality
  ): LazyList[Science.Atom[GmosNorth]] =
    Science.GmosNorth.compute(ls, tenMin, sp, iq, PosDouble.unsafeFrom(2.0))


  def sequencesEqual[A](
    obtained: LazyList[A],
    expected: LazyList[A]
  )(implicit loc: Location): Unit = {
    val sz = 20
    assertEquals(obtained.take(sz).toList, expected.take(sz).toList)
  }

  property("prefers explicitly set parameters") {
    forAll { (ls: Config.GmosNorth, sp: SourceProfile, iq: ImageQuality) =>
      val as = northSequence(ls, sp, iq)

      sequencesEqual(
        as.map(a => DynamicOptics.North.yBin.get(a.science.value)),
        LazyList.continually(ls.explicitYBin.getOrElse(ls.defaultYBin))
      )
    }
  }

  property("cycles through wavelength dithers") {
    forAll { (ls: Config.GmosNorth, sp: SourceProfile, iq: ImageQuality) =>
      val as = northSequence(ls, sp, iq)

      sequencesEqual(
        as.map(a => DynamicOptics.North.wavelength.getOption(a.science.value).get),
        (ls.wavelengthDithers match {
          case Nil => LazyList.continually(WavelengthDither.Zero)
          case ds  => LazyList.continually(ds.to(LazyList)).flatten
        }).map { d => ls.centralWavelength.offset(d).getOrElse(ls.centralWavelength) }
      )
    }
  }

  property("cycles through spatial offsets") {
    val offset =
      ProtoStep
        .stepConfig[GmosNorth]
        .andThen(StepConfig.science)
        .andThen(StepConfig.Science.offset)
        .andThen(Offset.q)
        .andThen(Offset.Component.angle)

    forAll { (ls: Config.GmosNorth, sp: SourceProfile, iq: ImageQuality) =>
      val as = northSequence(ls, sp, iq)

      sequencesEqual(
        as.map(_.science).flatMap(s => offset.getOption(s).toList),
        (ls.spatialOffsets match {
          case Nil => LazyList.continually(Offset.Q.Zero)
          case qs  => LazyList.continually(qs.to(LazyList)).flatten
        }).map(_.toAngle)
      )
    }
  }
}
