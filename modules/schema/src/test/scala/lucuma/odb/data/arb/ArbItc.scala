// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data
package arb

import cats.data.NonEmptyList
import eu.timepit.refined.scalacheck.all.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.data.Zipper
import lucuma.core.data.arb.ArbZipper
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.math.SignalToNoise
import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbSignalToNoise
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.model.Target
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbGid
import lucuma.core.util.arb.ArbTimeSpan
import lucuma.itc.IntegrationTime
import lucuma.itc.SignalToNoiseAt
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.*

trait ArbItc:
  import ArbEnumerated.given
  import ArbGid.given
  import ArbSignalToNoise.given
  import ArbTimeSpan.given
  import ArbWavelength.given
  import ArbZipper.given

  // This may exist elsewhere but I couldn't find it.
  given Arbitrary[IntegrationTime] =
    Arbitrary:
      for
        t <- arbitrary[TimeSpan]
        c <- arbitrary[PosInt]
      yield IntegrationTime(t, c)

  given Cogen[IntegrationTime] =
    Cogen[(TimeSpan, PosInt)].contramap: a =>
      (a.exposureTime, a.exposureCount)

  // This may exist elsewhere but I couldn't find it.
  given Arbitrary[SignalToNoiseAt] =
    Arbitrary:
      for
        w <- arbitrary[Wavelength]
        s <- arbitrary[SignalToNoise]
        t <- arbitrary[SignalToNoise]
      yield SignalToNoiseAt(w, SingleSN(s), TotalSN(t))

  given Cogen[SignalToNoiseAt] =
    Cogen[(Wavelength, SignalToNoise, SignalToNoise)].contramap: a =>
      (a.wavelength, a.single.value, a.total.value)

  given Arbitrary[Itc.Result] =
    Arbitrary:
      for
        t <- arbitrary[Target.Id]
        v <- arbitrary[IntegrationTime]
        s <- arbitrary[Option[SignalToNoiseAt]]
      yield Itc.Result(t, v, s)

  given Cogen[Itc.Result] =
    Cogen[(Target.Id, IntegrationTime, Option[SignalToNoiseAt])].contramap: a =>
      (a.targetId, a.value, a.signalToNoise)

  given Arbitrary[Itc.GmosNorthImaging] =
    Arbitrary:
      for
        f0 <- arbitrary[GmosNorthFilter]
        fs <- Gen.listOf(arbitrary[GmosNorthFilter]).map(fs => (f0 :: fs).distinct)
        zs <- Gen.listOfN(fs.size, arbitrary[Zipper[Itc.Result]])
      yield Itc.GmosNorthImaging(NonEmptyList.fromListUnsafe(fs.zip(zs)).toNem)

  given Cogen[Itc.GmosNorthImaging] =
    Cogen[List[(GmosNorthFilter, Zipper[Itc.Result])]].contramap: a =>
      a.science.toNel.toList

  given Arbitrary[Itc.GmosSouthImaging] =
    Arbitrary:
      for
        f0 <- arbitrary[GmosSouthFilter]
        fs <- Gen.listOf(arbitrary[GmosSouthFilter]).map(fs => (f0 :: fs).distinct)
        zs <- Gen.listOfN(fs.size, arbitrary[Zipper[Itc.Result]])
      yield Itc.GmosSouthImaging(NonEmptyList.fromListUnsafe(fs.zip(zs)).toNem)

  given Cogen[Itc.GmosSouthImaging] =
    Cogen[List[(GmosSouthFilter, Zipper[Itc.Result])]].contramap: a =>
      a.science.toNel.toList

  given Arbitrary[Itc.Spectroscopy] =
    Arbitrary:
      for
        a <- arbitrary[Zipper[Itc.Result]]
        s <- arbitrary[Zipper[Itc.Result]]
      yield Itc.Spectroscopy(a, s)

  given Cogen[Itc.Spectroscopy] =
    Cogen[(Zipper[Itc.Result], Zipper[Itc.Result])].contramap: a =>
      (a.acquisition, a.science)

  given Arbitrary[Itc] =
    Arbitrary:
      Gen.oneOf(
        arbitrary[Itc.GmosNorthImaging],
        arbitrary[Itc.GmosSouthImaging],
        arbitrary[Itc.Spectroscopy]
      )

  given Cogen[Itc] =
    Cogen[(
      Option[Itc.GmosNorthImaging],
      Option[Itc.GmosSouthImaging],
      Option[Itc.Spectroscopy]
    )].contramap: a =>
      (
        Itc.gmosNorthImaging.getOption(a),
        Itc.gmosSouthImaging.getOption(a),
        Itc.spectroscopy.getOption(a)
      )

object ArbItc extends ArbItc