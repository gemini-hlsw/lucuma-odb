// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package data
package arb

import cats.syntax.either.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.ExecutionState
import lucuma.core.enums.ScienceBand
import lucuma.core.util.Timestamp
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbTimestamp
import lucuma.itc.client.ImagingParameters
import lucuma.itc.client.InstrumentMode
import lucuma.itc.client.SpectroscopyParameters
import lucuma.itc.client.arb.ArbInstrumentMode
import lucuma.odb.sequence.gmos.longslit.Config
import lucuma.odb.sequence.gmos.longslit.arb.ArbGmosLongSlitConfig
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbGeneratorParams:
  import ArbEnumerated.given
  import ArbGmosLongSlitConfig.given
  import ArbInstrumentMode.given
  import ArbItcInput.given
  import ArbTimestamp.given

  private def genItcInput(mo: InstrumentMode): Gen[ItcInput] =
    arbitrary[ItcInput.Spectroscopy]
      .map: sp =>
        val acq = ImagingParameters.mode.replace(mo)(sp.acquisition)
        val sci = SpectroscopyParameters.mode.replace(mo)(sp.science)
        ItcInput.Spectroscopy(acq, sci, sp.targets, sp.blindOffset)

  val genGmosNorthLongSlit: Gen[GeneratorParams] =
    for
      mo  <- arbitrary[InstrumentMode.GmosNorthSpectroscopy]
      itc <- genItcInput(mo)
      bnd <- arbitrary[Option[ScienceBand]]
      cfg <- arbitrary[Config.GmosNorth]
      rol <- arbitrary[Option[CalibrationRole]]
      dc  <- arbitrary[Boolean]
      es  <- arbitrary[ExecutionState]
      ts  <- arbitrary[Option[Timestamp]]
    yield GeneratorParams(Either.right(itc), bnd, cfg, rol, dc, es, ts)

  val genGmosSouthLongSlit: Gen[GeneratorParams] =
    for
      mo  <- arbitrary[InstrumentMode.GmosSouthSpectroscopy]
      itc <- genItcInput(mo)
      bnd <- arbitrary[Option[ScienceBand]]
      cfg <- arbitrary[Config.GmosSouth]
      rol <- arbitrary[Option[CalibrationRole]]
      dc  <- arbitrary[Boolean]
      es  <- arbitrary[ExecutionState]
      ts  <- arbitrary[Option[Timestamp]]
    yield GeneratorParams(Either.right(itc), bnd, cfg, rol, dc, es, ts)

  given Arbitrary[GeneratorParams] =
    Arbitrary:
      Gen.oneOf(
        genGmosNorthLongSlit,
        genGmosSouthLongSlit
      )

object ArbGeneratorParams extends ArbGeneratorParams
