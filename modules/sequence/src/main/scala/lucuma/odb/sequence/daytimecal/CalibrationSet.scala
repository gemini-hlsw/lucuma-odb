// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.daytimecal

import cats.Eq
import cats.syntax.apply.*
import cats.syntax.eq.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.CalibrationSetType
import lucuma.core.enums.Flamingos2CustomSlitWidth
import lucuma.core.enums.GmosCustomSlitWidth
import lucuma.core.math.Angle
import lucuma.core.model.MaskDefinition
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMirrorMode
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.odb.json.all.transport.given
import lucuma.odb.json.ghost.transport.given
import lucuma.odb.sequence.util.HashBytes

/**
 * One generator row of a daytime calibration: the parameters that produce a run
 * of DAY_CAL steps taken with this setup.  A manual kind takes
 * `explicitCount` identical steps with its own `gcal`; a smart kind has
 * neither, since the Smart GCAL lookup supplies the steps.
 */
final case class CalibrationSet[D](
  kind:          CalibrationSetType,
  explicitCount: Option[PosInt],
  gcal:          Option[StepConfig.Gcal],
  setup:         D
)

object CalibrationSet:

  given [D](using Eq[D]): Eq[CalibrationSet[D]] =
    Eq.by(s => (s.kind, s.explicitCount.map(_.value), s.gcal, s.setup))

  private given HashBytes[StepConfig.Gcal] =
    HashBytes.forJsonEncoder

  given [D](using HashBytes[D]): HashBytes[CalibrationSet[D]] with
    def hashBytes(s: CalibrationSet[D]): Array[Byte] =
      Array.concat(
        HashBytes[CalibrationSetType].hashBytes(s.kind),
        HashBytes[Option[PosInt]].hashBytes(s.explicitCount),
        HashBytes[Option[StepConfig.Gcal]].hashBytes(s.gcal),
        HashBytes[D].hashBytes(s.setup)
      )

/**
 * The instrument setups a calibration set may carry.
 */
enum InstrumentSetup:
  case Flamingos2(config: Flamingos2DynamicConfig)
  case Ghost(config: GhostDynamicConfig)
  case GmosNorth(config: DynamicConfig.GmosNorth)
  case GmosSouth(config: DynamicConfig.GmosSouth)
  case Gnirs(config: GnirsDynamicConfig)
  case Igrins2(config: Igrins2DynamicConfig)

  def instrument: lucuma.core.enums.Instrument =
    this match
      case Flamingos2(_) => lucuma.core.enums.Instrument.Flamingos2
      case Ghost(_)      => lucuma.core.enums.Instrument.Ghost
      case GmosNorth(_)  => lucuma.core.enums.Instrument.GmosNorth
      case GmosSouth(_)  => lucuma.core.enums.Instrument.GmosSouth
      case Gnirs(_)      => lucuma.core.enums.Instrument.Gnirs
      case Igrins2(_)    => lucuma.core.enums.Instrument.Igrins2

  /** The mask of the setup's custom mask FPU, if it has one. */
  def customMask: Option[MaskDefinition] =
    this match
      case GmosNorth(c)  => c.fpu.collect { case GmosFpuMask.Custom(m, _) => m }
      case GmosSouth(c)  => c.fpu.collect { case GmosFpuMask.Custom(m, _) => m }
      case Flamingos2(c) => c.fpu.custom.map(_.mask)
      case _             => None

  /**
   * The setup with its custom mask slit width set to the allowed width nearest
   * the given one, or None if it has no custom mask or the width cannot be
   * expressed.  Flamingos 2 widths are in pixels, hence the pixel scale, in
   * arcseconds per pixel.  `Other` is never chosen: it has no Smart GCAL
   * equivalent.
   */
  def withNearestCustomSlitWidth(width: Angle, pixelScale: BigDecimal): Option[InstrumentSetup] =
    def gmos: GmosCustomSlitWidth =
      GmosCustomSlitWidth.values.minBy(w => (w.width.toMicroarcseconds - width.toMicroarcseconds).abs)

    def flamingos2: Option[Flamingos2CustomSlitWidth] =
      Option.when(pixelScale > 0):
        val pixels = BigDecimal(width.toMicroarcseconds) / 1_000_000 / pixelScale
        InstrumentSetup.Flamingos2PixelWidths.minBy((n, _) => (pixels - n).abs)._2

    this match
      case GmosNorth(c)  => c.fpu.collect { case GmosFpuMask.Custom(m, _) => GmosNorth(c.copy(fpu = GmosFpuMask.Custom(m, gmos).some)) }
      case GmosSouth(c)  => c.fpu.collect { case GmosFpuMask.Custom(m, _) => GmosSouth(c.copy(fpu = GmosFpuMask.Custom(m, gmos).some)) }
      case Flamingos2(c) => (c.fpu.custom, flamingos2).mapN((m, w) => Flamingos2(c.copy(fpu = Flamingos2FpuMask.Custom(m.mask, w))))
      case _             => None

  /**
   * The set kinds this instrument may be given.  The Flamingos 2, IGRINS-2,
   * GNIRS and GHOST lists are mirrored by the kind triggers in V1380, V1379,
   * V1389 and V1390, keep them in step.  With the acquisition mirror in GNIRS
   * also drops the arc kinds, which the trigger does not check.
   */
  def allowedKinds: List[CalibrationSetType] =
    this match
      case Flamingos2(_) | Igrins2(_)  => CalibrationSetType.values.toList.filterNot(k => k === CalibrationSetType.Bias || k === CalibrationSetType.DaytimeObserve)
      case Ghost(_)                    => CalibrationSetType.values.toList.filterNot(_ === CalibrationSetType.DaytimeObserve)
      case Gnirs(c)                    =>
        CalibrationSetType.values.toList.filterNot: k =>
          k === CalibrationSetType.Bias ||
          (c.acquisitionMirror === GnirsAcquisitionMirrorMode.In && (k === CalibrationSetType.Arc || k === CalibrationSetType.SmartArc))
      case GmosNorth(_) | GmosSouth(_) => CalibrationSetType.values.toList

  /** Whether the setup disperses light, which an arc requires. */
  def hasDisperser: Boolean =
    this match
      case Flamingos2(c) => c.disperser.isDefined
      case Ghost(_)      => true
      case GmosNorth(c)  => c.gratingConfig.isDefined
      case GmosSouth(c)  => c.gratingConfig.isDefined
      case Gnirs(c)      => c.acquisitionMirror =!= GnirsAcquisitionMirrorMode.In
      case Igrins2(_)    => true

object InstrumentSetup:

  // `Other` has no equivalent FPU, so it is never a candidate.
  private val Flamingos2PixelWidths: List[(Int, Flamingos2CustomSlitWidth)] =
    Flamingos2CustomSlitWidth.values.toList.flatMap(w => w.fpu.map(_.slitWidth.value -> w))

  given Eq[InstrumentSetup] =
    Eq.instance:
      case (Flamingos2(a), Flamingos2(b)) => a === b
      case (Ghost(a), Ghost(b))           => a === b
      case (GmosNorth(a), GmosNorth(b))   => a === b
      case (GmosSouth(a), GmosSouth(b))   => a === b
      case (Gnirs(a), Gnirs(b))           => a === b
      case (Igrins2(a), Igrins2(b))       => a === b
      case _                              => false

  private given HashBytes[Flamingos2DynamicConfig] = HashBytes.forJsonEncoder
  private given HashBytes[GhostDynamicConfig]      = HashBytes.forJsonEncoder
  private given HashBytes[DynamicConfig.GmosNorth] = HashBytes.forJsonEncoder
  private given HashBytes[DynamicConfig.GmosSouth] = HashBytes.forJsonEncoder
  private given HashBytes[GnirsDynamicConfig]      = HashBytes.forJsonEncoder
  private given HashBytes[Igrins2DynamicConfig]    = HashBytes.forJsonEncoder

  given HashBytes[InstrumentSetup] with
    def hashBytes(s: InstrumentSetup): Array[Byte] =
      s match
        case GmosNorth(c)  => Array.concat(Array[Byte](0), HashBytes[DynamicConfig.GmosNorth].hashBytes(c))
        case GmosSouth(c)  => Array.concat(Array[Byte](1), HashBytes[DynamicConfig.GmosSouth].hashBytes(c))
        case Flamingos2(c) => Array.concat(Array[Byte](2), HashBytes[Flamingos2DynamicConfig].hashBytes(c))
        case Igrins2(c)    => Array.concat(Array[Byte](3), HashBytes[Igrins2DynamicConfig].hashBytes(c))
        case Gnirs(c)      => Array.concat(Array[Byte](4), HashBytes[GnirsDynamicConfig].hashBytes(c))
        case Ghost(c)      => Array.concat(Array[Byte](5), HashBytes[GhostDynamicConfig].hashBytes(c))
