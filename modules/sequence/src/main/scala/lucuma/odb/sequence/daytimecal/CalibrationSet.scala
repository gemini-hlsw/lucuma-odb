// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.daytimecal

import cats.Eq
import cats.syntax.eq.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.CalibrationSetType
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.odb.json.all.transport.given
import lucuma.odb.sequence.util.HashBytes

/**
 * One generator row of a daytime calibration
 * These are parameters that can produce an atome for a daytime calibration.
 * It will produce an atom with daytime calibration steps that could use this same setup.
 */
final case class CalibrationSet[D](
  kind:  CalibrationSetType,
  count: PosInt,
  gcal:  Option[StepConfig.Gcal],
  setup: D
)

object CalibrationSet:

  given [D](using Eq[D]): Eq[CalibrationSet[D]] =
    Eq.by(s => (s.kind, s.count.value, s.gcal, s.setup))

  private given HashBytes[StepConfig.Gcal] =
    HashBytes.forJsonEncoder

  given [D](using HashBytes[D]): HashBytes[CalibrationSet[D]] with
    def hashBytes(s: CalibrationSet[D]): Array[Byte] =
      Array.concat(
        HashBytes[CalibrationSetType].hashBytes(s.kind),
        HashBytes[PosInt].hashBytes(s.count),
        HashBytes[Option[StepConfig.Gcal]].hashBytes(s.gcal),
        HashBytes[D].hashBytes(s.setup)
      )

/**
 * The instrument setups a calibration set may carry.
 */
enum InstrumentSetup:
  case GmosNorth(config: DynamicConfig.GmosNorth)
  case GmosSouth(config: DynamicConfig.GmosSouth)

  def instrument: lucuma.core.enums.Instrument =
    this match
      case GmosNorth(_) => lucuma.core.enums.Instrument.GmosNorth
      case GmosSouth(_) => lucuma.core.enums.Instrument.GmosSouth

object InstrumentSetup:

  given Eq[InstrumentSetup] =
    Eq.instance:
      case (GmosNorth(a), GmosNorth(b)) => a === b
      case (GmosSouth(a), GmosSouth(b)) => a === b
      case _                            => false

  private given HashBytes[DynamicConfig.GmosNorth] = HashBytes.forJsonEncoder
  private given HashBytes[DynamicConfig.GmosSouth] = HashBytes.forJsonEncoder

  given HashBytes[InstrumentSetup] with
    def hashBytes(s: InstrumentSetup): Array[Byte] =
      s match
        case GmosNorth(c) => Array.concat(Array[Byte](0), HashBytes[DynamicConfig.GmosNorth].hashBytes(c))
        case GmosSouth(c) => Array.concat(Array[Byte](1), HashBytes[DynamicConfig.GmosSouth].hashBytes(c))
