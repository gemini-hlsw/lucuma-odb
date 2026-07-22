// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Eq
import cats.Order
import cats.data.NonEmptyMap
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.data.Zipper
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GnirsAcquisitionType
import lucuma.core.enums.GnirsFilter
import lucuma.core.model.Target
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.itc.SignalToNoiseAt
import monocle.Prism
import monocle.macros.GenPrism

/**
 * A single ITC result for one target: the integration time (exposure time and
 * count) and, when available, the achieved signal-to-noise.  Shared by both the
 * science ([[ItcScience]]) and acquisition ([[ItcAcquisition]]) results.
 * Corresponds to `ItcResult` in the GraphQL schema.
 */
case class ItcResult(
  targetId:      Target.Id,
  value:         IntegrationTime,
  signalToNoise: Option[SignalToNoiseAt]
):
  def totalTime: Option[TimeSpan] =
    val total = BigInt(value.exposureTime.toMicroseconds) * value.exposureCount.value
    Option.when(total.isValidLong)(TimeSpan.fromMicroseconds(total.longValue)).flatten

object ItcResult:
  given Order[ItcResult] =
    Order.by(s => (s.totalTime, s.targetId))

/**
 * The acquisition ITC outcome for an observation.  Three genuinely distinct
 * states, kept explicit so a failure is never mistaken for "no acquisition":
 *
 *   - [[ItcAcquisition.NotApplicable]] — the observing mode has no acquisition
 *     sequence at all (imaging, GHOST, IGRINS-2).
 *   - [[ItcAcquisition.Failed]] — the mode has an acquisition sequence, but its
 *     ITC result could not be produced.  Only arises for acquisition-capable
 *     modes, so it means "expected but unavailable".
 *   - [[ItcAcquisition.Available]] — the acquisition ITC result.
 *
 * An acquisition result has a single shape regardless of instrument
 * (acquisition is always imaging through a filter): a set of per-target
 * integration times.  GNIRS is particular in that the acquisition type (Very
 * Bright / Bright / Faint) is classified in the ITC layer via a two-pass
 * calculation and pinned here, so the sequence generator does not re-derive
 * (and possibly misclassify) it.  See the two-pass acquisition ITC in
 * ItcService.
 *
 * The three cases map one-to-one onto the two `c_acquisition_*` columns of
 * `t_itc_result`: NotApplicable = both null, Failed = c_acquisition_error,
 * Available = c_acquisition_results.
 */
enum ItcAcquisition derives Eq:
  case NotApplicable
  case Failed(message: String)
  case Available(times: Zipper[ItcResult], gnirsAcqType: Option[GnirsAcquisitionType])

object ItcAcquisition:
  given Eq[Available] =
    Eq.by(a => (a.times, a.gnirsAcqType))

/**
 * Science ITC results.  Because the results differ depending upon whether it is
 * imaging or spectroscopy, and which particular instrument is in use, there are
 * specific types for each case.  The corresponding acquisition results, for the
 * modes that have an acquisition sequence, are held separately in
 * [[ItcAcquisition]].
 */
sealed trait ItcScience:

  // Used in the circe decoder.  Itc results are stored in a jsonb column.
  def dataType: ItcScience.Type

  /**
   * The expected total science exposure count.  This is used for limiting
   * sequence generation to reasonable values.
   */
  def scienceExposureCount: PosInt

object ItcScience:

  // ITC result type discriminator.
  enum Type(val tag: String) derives Enumerated:
    case Flamingos2Imaging   extends Type("flamingos_2_imaging")
    case GhostIfu            extends Type("ghost_ifu")
    case GmosNorthImaging    extends Type("gmos_north_imaging")
    case GmosSouthImaging    extends Type("gmos_south_imaging")
    case GnirsImaging        extends Type("gnirs_imaging")
    case Spectroscopy        extends Type("spectroscopy")

  case class Flamingos2Imaging(
    science: NonEmptyMap[Flamingos2Filter, Zipper[ItcResult]]
  ) extends ItcScience derives Eq:

    override def dataType: Type =
      Type.Flamingos2Imaging

    override def scienceExposureCount: PosInt =
      PosInt.unsafeFrom:
        science.foldLeft(0) { (cnt, z) =>
          cnt + z.focus.value.exposureCount.value
        }

  val flamingos2Imaging: Prism[ItcScience, Flamingos2Imaging] =
    GenPrism[ItcScience, Flamingos2Imaging]

  case class GhostIfu(
    red:  Zipper[ItcResult],
    blue: Zipper[ItcResult]
  ) extends ItcScience:

    override def dataType: Type =
      Type.GhostIfu

    override def scienceExposureCount: PosInt =
      red.focus.value.exposureCount max blue.focus.value.exposureCount

  object GhostIfu:
    given Eq[GhostIfu] =
      Eq.by(a => (a.red, a.blue))

  val ghostIfu: Prism[ItcScience, GhostIfu] =
    GenPrism[ItcScience, GhostIfu]

  /**
   * GMOS North imaging results.  There are results per-GMOS North filter.
   */
  case class GmosNorthImaging(
    science: NonEmptyMap[GmosNorthFilter, Zipper[ItcResult]]
  ) extends ItcScience:

    override def dataType: Type =
      Type.GmosNorthImaging

    override def scienceExposureCount: PosInt =
      PosInt.unsafeFrom:
        science.foldLeft(0) { (cnt, z) =>
          cnt + z.focus.value.exposureCount.value
        }

  object GmosNorthImaging:
    given Eq[GmosNorthImaging] =
      Eq.by(_.science)

  val gmosNorthImaging: Prism[ItcScience, GmosNorthImaging] =
    GenPrism[ItcScience, GmosNorthImaging]

  /**
   * GMOS South imaging results.  There are results per-GMOS South filter.
   */
  case class GmosSouthImaging(
    science: NonEmptyMap[GmosSouthFilter, Zipper[ItcResult]]
  ) extends ItcScience:

    override def dataType: Type =
      Type.GmosSouthImaging

    override def scienceExposureCount: PosInt =
      PosInt.unsafeFrom:
        science.foldLeft(0) { (cnt, z) =>
          cnt + z.focus.value.exposureCount.value
        }

  object GmosSouthImaging:
    given Eq[GmosSouthImaging] =
      Eq.by(_.science)

  val gmosSouthImaging: Prism[ItcScience, GmosSouthImaging] =
    GenPrism[ItcScience, GmosSouthImaging]

  /**
   * GNIRS imaging results.  There are results per-GNIRS filter.
   */
  case class GnirsImaging(
    science: NonEmptyMap[GnirsFilter, Zipper[ItcResult]]
  ) extends ItcScience:

    override def dataType: Type =
      Type.GnirsImaging

    override def scienceExposureCount: PosInt =
      PosInt.unsafeFrom:
        science.foldLeft(0) { (cnt, z) =>
          cnt + z.focus.value.exposureCount.value
        }

  object GnirsImaging:
    given Eq[GnirsImaging] =
      Eq.by(_.science)

  val gnirsImaging: Prism[ItcScience, GnirsImaging] =
    GenPrism[ItcScience, GnirsImaging]

  /**
   * Spectroscopy science results, shared by every spectroscopy mode (whether or
   * not it has an acquisition sequence — IGRINS-2, for instance, has none).  The
   * corresponding acquisition results, for the modes that do have one, are held
   * separately in [[ItcAcquisition]].
   */
  case class Spectroscopy(
    science: Zipper[ItcResult]
  ) extends ItcScience:

    override def dataType: Type =
      Type.Spectroscopy

    override def scienceExposureCount: PosInt =
      science.focus.value.exposureCount

  object Spectroscopy:
    given Eq[Spectroscopy] =
      Eq.by(_.science)

  val spectroscopy: Prism[ItcScience, Spectroscopy] =
    GenPrism[ItcScience, Spectroscopy]

  given Eq[ItcScience] =
    Eq.instance:
      case (a: Flamingos2Imaging, b: Flamingos2Imaging) => a === b
      case (a: GhostIfu,          b: GhostIfu)          => a === b
      case (a: GmosNorthImaging,  b: GmosNorthImaging)  => a === b
      case (a: GmosSouthImaging,  b: GmosSouthImaging)  => a === b
      case (a: GnirsImaging,      b: GnirsImaging)      => a === b
      case (a: Spectroscopy,      b: Spectroscopy)      => a === b
      case _                                            => false

/**
 * The complete ITC result for an observation: the science results (always
 * present) and, for the modes that have an acquisition sequence, the
 * acquisition results.  The two parts are stored, cached, and frozen
 * independently (science may be frozen at execution start while acquisition
 * stays live); this type is just the in-memory composite that pairs them, and
 * as such has no JSON codec of its own.
 */
case class Itc(
  acquisition: ItcAcquisition,
  science:     ItcScience
):
  def scienceExposureCount: PosInt =
    science.scienceExposureCount

object Itc:
  given Eq[Itc] =
    Eq.by(a => (a.acquisition, a.science))
