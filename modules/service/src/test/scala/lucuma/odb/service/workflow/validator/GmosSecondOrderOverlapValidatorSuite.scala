// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow
package validator

import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.gmos.SecondOrderOverlap
import munit.FunSuite

import scala.collection.immutable.SortedSet

import GmosSecondOrderOverlapValidator.*

class GmosSecondOrderOverlapValidatorSuite extends FunSuite:

  private def nm(n: Int): Wavelength =
    Wavelength.unsafeFromIntPicometers(n * 1000)

  test("message"):
    assertEquals(
      message(nm(720), NonEmptyList.of(nm(700))),
      "Second order light overlaps the spectrum beyond 720 nm at central wavelength 700 nm. It is recommended to use a blocking filter to avoid second order overlap."
    )
    assertEquals(
      message(nm(720), NonEmptyList.of(nm(700), Wavelength.unsafeFromIntPicometers(705_500))),
      "Second order light overlaps the spectrum beyond 720 nm at central wavelengths 700 nm, 705.5 nm. It is recommended to use a blocking filter to avoid second order overlap."
    )

  test("no overlaps, no warning"):
    assertEquals(validation(SortedSet.empty), None)

  test("one warning, a message per onset, wavelengths in order"):
    val v = validation(SortedSet(
      SecondOrderOverlap(nm(920), nm(800)),
      SecondOrderOverlap(nm(720), nm(708)),
      SecondOrderOverlap(nm(720), nm(692)),
      SecondOrderOverlap(nm(720), nm(700))
    ))
    assertEquals(v.map(_.code), Some(ObservationValidationCode.SecondOrderOverlap))
    assertEquals(
      v.map(_.messages),
      Some(NonEmptyChain(
        message(nm(720), NonEmptyList.of(nm(692), nm(700), nm(708))),
        message(nm(920), NonEmptyList.of(nm(800)))
      ))
    )
