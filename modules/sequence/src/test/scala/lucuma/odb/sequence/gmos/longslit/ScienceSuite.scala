// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos
package longslit

import fs2.Pure
import fs2.Stream
import lucuma.core.math.Offset
import lucuma.core.math.WavelengthDither
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.util.arb.ArbEnumerated
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.SciExposureTime
import lucuma.odb.sequence.gmos.longslit.arb.ArbGmosLongSlitConfig
import munit.Location
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*

import java.time.Duration

class ScienceSuite extends ScalaCheckSuite {

  import ArbEnumerated.*
  import ArbGmosLongSlitConfig.given

  val tenMin: SciExposureTime =
    SciExposureTime.fromDuration(Duration.ofMinutes(10)).get

  def northSequence(
    ls: Config.GmosNorth,
  ): Stream[Pure, ScienceAtom[GmosNorth]] =
    Science.GmosNorth.compute(ls, tenMin)

  def sequencesEqual[A](
    obtained: Stream[Pure, A],
    expected: Stream[Pure, A]
  )(implicit loc: Location): Unit = {
    val sz = 20
    assertEquals(obtained.take(sz).toList, expected.take(sz).toList)
  }

  property("prefers explicitly set parameters") {
    forAll { (ls: Config.GmosNorth) =>
      val as = northSequence(ls)

      sequencesEqual(
        as.map(a => DynamicOptics.North.yBin.get(a.science.value)),
        Stream(ls.explicitYBin.getOrElse(ls.defaultYBin)).repeat
      )
    }
  }

  property("cycles through wavelength dithers") {
    forAll { (ls: Config.GmosNorth) =>
      val as = northSequence(ls)

      sequencesEqual(
        as.map(a => DynamicOptics.North.wavelength.getOption(a.science.value).get),
        (ls.wavelengthDithers match {
          case Nil => Stream(WavelengthDither.Zero).repeat
          case ds  => Stream.emits(ds).repeat
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

    forAll { (ls: Config.GmosNorth) =>
      val as = northSequence(ls)

      sequencesEqual(
        as.map(_.science).flatMap(s => Stream.fromOption(offset.getOption(s))),
        (ls.spatialOffsets match {
          case Nil => Stream(Offset.Q.Zero).repeat
          case qs  => Stream.emits(qs).repeat
        }).map(_.toAngle)
      )
    }
  }
}
