// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package data

import cats.syntax.eq.*
import cats.syntax.functor.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.FieldLens
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.enums.PortDisposition
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessValue
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.itc.AltairParameters
import lucuma.itc.client.ImagingParameters
import lucuma.itc.client.InstrumentMode
import lucuma.itc.client.TargetInput
import lucuma.odb.sequence.data.arb.ArbItcInput.given
import lucuma.odb.sequence.util.HashBytes
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*

class ItcInputSuite extends ScalaCheckSuite:

  property("acquisition sequence uses blind offset target when available, science sequence never uses blind offset"):
    forAll { (itcInput: ItcInput.Spectroscopy) =>
      val acq = itcInput.acquisitionInput
      val sci = itcInput.scienceInput

      // Acquisition should use blind offset target only.
      val bot = itcInput.blindOffset.map(_._2)
      assertEquals(acq.asterism.length, bot.as(1).getOrElse(itcInput.targets.length))
      assertEquals(acq.asterism.head,   bot.getOrElse(itcInput.targets.head._2))

      // Science should use regular targets only
      assert(sci.asterism.length === itcInput.targets.size)
      assert(sci.asterism.head === itcInput.targets.head._2)
    }

  property("GNIRS spectroscopy hashes every central wavelength"):
    forAll { (itcInput: ItcInput.GnirsSpectroscopy) =>
      // Dropping a central wavelength must change the cache key: two observations
      // differing only in an extra wavelength must not collide in t_itc_result.
      val trimmed = itcInput.science.toList match
        case _ :: (t @ _ :: _) => Some(itcInput.copy(science = cats.data.NonEmptyList.fromListUnsafe(t)))
        case _                 => None

      trimmed.foreach: t =>
        assert(
          !java.util.Arrays.equals(HashBytes[ItcInput].hashBytes(itcInput), HashBytes[ItcInput].hashBytes(t)),
          "Removing a central wavelength did not change the hash"
        )
    }

  // The arbitrary instrument modes are never GNIRS, so Altair has to be put where it can appear.
  private val gnirsImaging: InstrumentMode =
    InstrumentMode.GnirsImaging(
      ExposureTimeMode.SignalToNoiseMode(
        SignalToNoise.unsafeFromBigDecimalExact(100),
        Wavelength.fromIntNanometers(2200).get
      ),
      GnirsFilter.K,
      GnirsCamera.LongBlue,
      GnirsReadMode.Bright,
      GnirsWellDepth.Shallow,
      PosInt.unsafeFrom(1),
      PortDisposition.Bottom,
      none
    )

  private val ngs: AltairParameters =
    AltairParameters.Ngs(Angle.fromDoubleArcseconds(3.5), BrightnessValue.unsafeFrom(12.5), FieldLens.Out)

  private def hash(input: ItcInput): List[Byte] =
    HashBytes[ItcInput].hashBytes(input).toList

  private def assertAltairFreeHash(input: ItcInput): Unit =
    val free = hash(ItcInput.withAltairParameters(input, none))
    assertEquals(hash(ItcInput.withAltairParameters(input, ngs.some)), free)
    assertEquals(hash(ItcInput.withAltairParameters(input, AltairParameters.LgsP1.some)), free)

  // The generator resolves the Altair parameters from a guide star, while obscalc and the workflow
  // read the same observation from the database alone. Both must arrive at the same cache key, so
  // the Altair parameters are keyed apart (t_itc_result.c_altair_hash) rather than hashed here.
  property("the GNIRS Altair parameters are not part of the hash"):
    forAll { (im: ItcInput.Imaging, sp: ItcInput.GnirsSpectroscopy) =>
      val imaging = im.copy(
        science     = im.science.map(ImagingParameters.mode.replace(gnirsImaging)),
        acquisition = im.acquisition.map(ImagingParameters.mode.replace(gnirsImaging))
      )
      assertAltairFreeHash(imaging)
      assertAltairFreeHash(sp.copy(acquisition = ImagingParameters.mode.replace(gnirsImaging)(sp.acquisition)))

      // The rest of the mode is hashed as it always was.
      assertNotEquals(hash(imaging), hash(im))
    }
