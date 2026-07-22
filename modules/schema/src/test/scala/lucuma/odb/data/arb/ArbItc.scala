// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data
package arb

import cats.data.NonEmptyList
import eu.timepit.refined.scalacheck.all.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.data.Zipper
import lucuma.core.data.arb.ArbZipper
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GnirsAcquisitionType
import lucuma.core.enums.GnirsFilter
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

  given Arbitrary[ItcResult] =
    Arbitrary:
      for
        t <- arbitrary[Target.Id]
        v <- arbitrary[IntegrationTime]
        s <- arbitrary[Option[SignalToNoiseAt]]
      yield ItcResult(t, v, s)

  given Cogen[ItcResult] =
    Cogen[(Target.Id, IntegrationTime, Option[SignalToNoiseAt])].contramap: a =>
      (a.targetId, a.value, a.signalToNoise)

  given Arbitrary[ItcScience.Flamingos2Imaging] =
    Arbitrary:
      for
        f0 <- arbitrary[Flamingos2Filter]
        fs <- Gen.listOf(arbitrary[Flamingos2Filter]).map(fs => (f0 :: fs).distinct)
        zs <- Gen.listOfN(fs.size, arbitrary[Zipper[ItcResult]])
      yield ItcScience.Flamingos2Imaging(NonEmptyList.fromListUnsafe(fs.zip(zs)).toNem)

  given Cogen[ItcScience.Flamingos2Imaging] =
    Cogen[List[(Flamingos2Filter, Zipper[ItcResult])]].contramap: a =>
      a.science.toNel.toList

  given Arbitrary[ItcScience.GhostIfu] =
    Arbitrary:
      for
        red  <- arbitrary[Zipper[ItcResult]]
        blue <- arbitrary[Zipper[ItcResult]]
      yield ItcScience.GhostIfu(red, blue)

  given Cogen[ItcScience.GhostIfu] =
    Cogen[(Zipper[ItcResult], Zipper[ItcResult])].contramap: a =>
      (a.red, a.blue)

  given Arbitrary[ItcScience.GmosNorthImaging] =
    Arbitrary:
      for
        f0 <- arbitrary[GmosNorthFilter]
        fs <- Gen.listOf(arbitrary[GmosNorthFilter]).map(fs => (f0 :: fs).distinct)
        zs <- Gen.listOfN(fs.size, arbitrary[Zipper[ItcResult]])
      yield ItcScience.GmosNorthImaging(NonEmptyList.fromListUnsafe(fs.zip(zs)).toNem)

  given Cogen[ItcScience.GmosNorthImaging] =
    Cogen[List[(GmosNorthFilter, Zipper[ItcResult])]].contramap: a =>
      a.science.toNel.toList

  given Arbitrary[ItcScience.GmosSouthImaging] =
    Arbitrary:
      for
        f0 <- arbitrary[GmosSouthFilter]
        fs <- Gen.listOf(arbitrary[GmosSouthFilter]).map(fs => (f0 :: fs).distinct)
        zs <- Gen.listOfN(fs.size, arbitrary[Zipper[ItcResult]])
      yield ItcScience.GmosSouthImaging(NonEmptyList.fromListUnsafe(fs.zip(zs)).toNem)

  given Cogen[ItcScience.GmosSouthImaging] =
    Cogen[List[(GmosSouthFilter, Zipper[ItcResult])]].contramap: a =>
      a.science.toNel.toList

  given Arbitrary[ItcScience.GnirsImaging] =
    Arbitrary:
      for
        f0 <- arbitrary[GnirsFilter]
        fs <- Gen.listOf(arbitrary[GnirsFilter]).map(fs => (f0 :: fs).distinct)
        zs <- Gen.listOfN(fs.size, arbitrary[Zipper[ItcResult]])
      yield ItcScience.GnirsImaging(NonEmptyList.fromListUnsafe(fs.zip(zs)).toNem)

  given Cogen[ItcScience.GnirsImaging] =
    Cogen[List[(GnirsFilter, Zipper[ItcResult])]].contramap: a =>
      a.science.toNel.toList

  given Arbitrary[ItcScience.Spectroscopy] =
    Arbitrary:
      arbitrary[Zipper[ItcResult]].map(ItcScience.Spectroscopy.apply)

  given Cogen[ItcScience.Spectroscopy] =
    Cogen[Zipper[ItcResult]].contramap(_.science)

  given Arbitrary[ItcScience] =
    Arbitrary:
      Gen.oneOf(
        arbitrary[ItcScience.Flamingos2Imaging],
        arbitrary[ItcScience.GhostIfu],
        arbitrary[ItcScience.GmosNorthImaging],
        arbitrary[ItcScience.GmosSouthImaging],
        arbitrary[ItcScience.GnirsImaging],
        arbitrary[ItcScience.Spectroscopy]
      )

  given Cogen[ItcScience] =
    Cogen[
      Either[ItcScience.Spectroscopy, Either[ItcScience.GmosNorthImaging, Either[ItcScience.GmosSouthImaging, Either[ItcScience.GnirsImaging, Either[ItcScience.GhostIfu, ItcScience.Flamingos2Imaging]]]]]
    ].contramap:
      case a: ItcScience.Spectroscopy      => Left(a)
      case a: ItcScience.GmosNorthImaging  => Right(Left(a))
      case a: ItcScience.GmosSouthImaging  => Right(Right(Left(a)))
      case a: ItcScience.GnirsImaging      => Right(Right(Right(Left(a))))
      case a: ItcScience.GhostIfu          => Right(Right(Right(Right(Left(a)))))
      case a: ItcScience.Flamingos2Imaging => Right(Right(Right(Right(Right(a)))))

  given Arbitrary[ItcAcquisition.Available] =
    Arbitrary:
      for
        z <- arbitrary[Zipper[ItcResult]]
        t <- arbitrary[Option[GnirsAcquisitionType]]
      yield ItcAcquisition.Available(z, t)

  given Cogen[ItcAcquisition.Available] =
    Cogen[(Zipper[ItcResult], Option[GnirsAcquisitionType])].contramap: a =>
      (a.times, a.gnirsAcqType)

  given Arbitrary[ItcAcquisition] =
    Arbitrary:
      Gen.oneOf(
        Gen.const(ItcAcquisition.NotApplicable),
        arbitrary[String].map(ItcAcquisition.Failed.apply),
        arbitrary[ItcAcquisition.Available]
      )

  given Cogen[ItcAcquisition] =
    Cogen[Either[Unit, Either[String, ItcAcquisition.Available]]].contramap:
      case ItcAcquisition.NotApplicable       => Left(())
      case ItcAcquisition.Failed(msg)         => Right(Left(msg))
      case a: ItcAcquisition.Available        => Right(Right(a))

  given Arbitrary[Itc] =
    Arbitrary:
      for
        a <- arbitrary[ItcAcquisition]
        s <- arbitrary[ItcScience]
      yield Itc(a, s)

  given Cogen[Itc] =
    Cogen[(ItcAcquisition, ItcScience)].contramap: a =>
      (a.acquisition, a.science)

object ArbItc extends ArbItc
