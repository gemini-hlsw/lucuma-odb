// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package data

import cats.data.NonEmptyList
import lucuma.core.enums.SequenceType
import lucuma.core.model.Target
import lucuma.core.util.Timestamp
import lucuma.itc.client.ImagingParameters
import lucuma.itc.client.InstrumentMode
import lucuma.itc.client.SpectroscopyParameters
import lucuma.itc.client.TargetInput
import lucuma.itc.client.arb.ArbInstrumentMode.given
import lucuma.itc.client.arb.ArbIntegrationTimeInput.given
import lucuma.itc.client.arb.ArbTargetInput.given
import munit.FunSuite
import org.scalacheck.Arbitrary.arbitrary

class ItcInputSuite extends FunSuite:

  private def createItcInput(
    targets: NonEmptyList[(Target.Id, TargetInput, Option[Timestamp])],
    blindOffsetTarget: Option[(Target.Id, TargetInput, Option[Timestamp])]
  ): ItcInput =
    val mode = arbitrary[InstrumentMode].sample.get
    ItcInput(
      arbitrary[ImagingParameters].sample.get.copy(mode = mode),
      arbitrary[SpectroscopyParameters].sample.get.copy(mode = mode),
      targets,
      blindOffsetTarget
    )

  test("acquisition sequence uses blind offset target when available"):
    val regularTarget = (Target.Id.fromLong(1L).get, arbitrary[TargetInput].sample.get, Option.empty[Timestamp])
    val blindOffsetTarget = (Target.Id.fromLong(2L).get, arbitrary[TargetInput].sample.get, Option.empty[Timestamp])

    val itcInput = createItcInput(
      NonEmptyList.one(regularTarget),
      Some(blindOffsetTarget)
    )

    val acquisitionInput = itcInput.imagingInput(SequenceType.Acquisition)
    val scienceInput = itcInput.spectroscopyInput(SequenceType.Science)

    // Acquisition should use blind offset target only
    assertEquals(acquisitionInput.asterism.length, 1)
    assertEquals(acquisitionInput.asterism.head, blindOffsetTarget._2)

    // Science should use regular targets only
    assertEquals(scienceInput.asterism.length, 1)
    assertEquals(scienceInput.asterism.head, regularTarget._2)

  test("both sequences use regular targets when no blind offset exists"):
    val regularTargets = NonEmptyList.of(
      (Target.Id.fromLong(1L).get, arbitrary[TargetInput].sample.get, Option.empty[Timestamp]),
      (Target.Id.fromLong(2L).get, arbitrary[TargetInput].sample.get, Option.empty[Timestamp])
    )

    val itcInput = createItcInput(regularTargets, None)

    val acquisitionInput = itcInput.imagingInput(SequenceType.Acquisition)
    val scienceInput = itcInput.spectroscopyInput(SequenceType.Science)

    // Both should use all regular targets when no blind offset
    assertEquals(acquisitionInput.asterism.length, 2)
    assertEquals(scienceInput.asterism.length, 2)
    assertEquals(acquisitionInput.asterism, scienceInput.asterism)

  test("science sequence ignores blind offset target"):
    val regularTarget = (Target.Id.fromLong(1L).get, arbitrary[TargetInput].sample.get, Option.empty[Timestamp])
    val blindOffsetTarget = (Target.Id.fromLong(2L).get, arbitrary[TargetInput].sample.get, Option.empty[Timestamp])

    val itcInput = createItcInput(
      NonEmptyList.one(regularTarget),
      Some(blindOffsetTarget)
    )

    val scienceInput = itcInput.spectroscopyInput(SequenceType.Science)

    // Science sequence should always use regular targets, never blind offset
    assertEquals(scienceInput.asterism.length, 1)
    assertEquals(scienceInput.asterism.head, regularTarget._2)