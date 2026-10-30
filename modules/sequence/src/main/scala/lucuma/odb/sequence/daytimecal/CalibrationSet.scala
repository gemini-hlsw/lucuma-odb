// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.daytimecal

import cats.Eq
import cats.syntax.eq.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.CalibrationSetType
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.odb.json.all.transport.given
import lucuma.odb.sequence.util.HashBytes

/**
 * One generator row of a daytime calibration: the parameters that produce one
 * atom of DAY_CAL steps taken with this setup.  A manual kind takes `count`
 * identical steps with its own `gcal`; a smart kind has neither, since the
 * Smart GCAL lookup supplies the steps.
 */
final case class CalibrationSet[D](
  kind:  CalibrationSetType,
  count: Option[PosInt],
  gcal:  Option[StepConfig.Gcal],
  setup: D
)

object CalibrationSet:

  given [D](using Eq[D]): Eq[CalibrationSet[D]] =
    Eq.by(s => (s.kind, s.count.map(_.value), s.gcal, s.setup))

  private given HashBytes[StepConfig.Gcal] =
    HashBytes.forJsonEncoder

  given [D](using HashBytes[D]): HashBytes[CalibrationSet[D]] with
    def hashBytes(s: CalibrationSet[D]): Array[Byte] =
      Array.concat(
        HashBytes[CalibrationSetType].hashBytes(s.kind),
        HashBytes[Option[PosInt]].hashBytes(s.count),
        HashBytes[Option[StepConfig.Gcal]].hashBytes(s.gcal),
        HashBytes[D].hashBytes(s.setup)
      )

/**
 * The instrument setups a calibration set may carry.
 */
enum InstrumentSetup:
  case Flamingos2(config: Flamingos2DynamicConfig)
  case GmosNorth(config: DynamicConfig.GmosNorth)
  case GmosSouth(config: DynamicConfig.GmosSouth)

  def instrument: lucuma.core.enums.Instrument =
    this match
      case Flamingos2(_) => lucuma.core.enums.Instrument.Flamingos2
      case GmosNorth(_)  => lucuma.core.enums.Instrument.GmosNorth
      case GmosSouth(_)  => lucuma.core.enums.Instrument.GmosSouth

  /**
   * The set kinds this instrument may be given.  The Flamingos 2 list is
   * mirrored by the kind trigger in V1377, keep them in step.
   */
  def allowedKinds: List[CalibrationSetType] =
    this match
      case Flamingos2(_)               => CalibrationSetType.values.toList.filterNot(k => k === CalibrationSetType.Bias || k === CalibrationSetType.DaytimeObserve)
      case GmosNorth(_) | GmosSouth(_) => CalibrationSetType.values.toList

  /** Whether the setup disperses light, which an arc requires. */
  def hasDisperser: Boolean =
    this match
      case Flamingos2(c) => c.disperser.isDefined
      case GmosNorth(c)  => c.gratingConfig.isDefined
      case GmosSouth(c)  => c.gratingConfig.isDefined

object InstrumentSetup:

  given Eq[InstrumentSetup] =
    Eq.instance:
      case (Flamingos2(a), Flamingos2(b)) => a === b
      case (GmosNorth(a), GmosNorth(b))   => a === b
      case (GmosSouth(a), GmosSouth(b))   => a === b
      case _                              => false

  private given HashBytes[Flamingos2DynamicConfig] = HashBytes.forJsonEncoder
  private given HashBytes[DynamicConfig.GmosNorth] = HashBytes.forJsonEncoder
  private given HashBytes[DynamicConfig.GmosSouth] = HashBytes.forJsonEncoder

  given HashBytes[InstrumentSetup] with
    def hashBytes(s: InstrumentSetup): Array[Byte] =
      s match
        case GmosNorth(c)  => Array.concat(Array[Byte](0), HashBytes[DynamicConfig.GmosNorth].hashBytes(c))
        case GmosSouth(c)  => Array.concat(Array[Byte](1), HashBytes[DynamicConfig.GmosSouth].hashBytes(c))
        case Flamingos2(c) => Array.concat(Array[Byte](2), HashBytes[Flamingos2DynamicConfig].hashBytes(c))
