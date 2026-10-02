// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import lucuma.core.enums.SequenceType
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan

/**
 * Limits on a single exposure time.  An exposure shorter than `minimum` or
 * longer than `maximum` cannot be taken and is an error.  One merely outside
 * the recommended range is a warning.  Any limit may be absent, in which case
 * that check is skipped, so supplying a value later is all it takes to start
 * checking it.
 */
final case class ExposureTimeLimits(
  minimum:            Option[TimeSpan] = None,
  recommendedMinimum: Option[TimeSpan] = None,
  recommendedMaximum: Option[TimeSpan] = None,
  maximum:            Option[TimeSpan] = None
):

  /** The limit `time` violates, if any, worst first. */
  def classify(time: TimeSpan): Option[(ExposureTimeIssue.Kind, TimeSpan)] =
    import ExposureTimeIssue.Kind.*
    minimum.filter(time < _).map(BelowMinimum -> _)                       orElse
    maximum.filter(time > _).map(AboveMaximum -> _)                       orElse
    recommendedMinimum.filter(time < _).map(BelowRecommendedMinimum -> _) orElse
    recommendedMaximum.filter(time > _).map(AboveRecommendedMaximum -> _)

object ExposureTimeLimits:

  val Unlimited: ExposureTimeLimits =
    ExposureTimeLimits()

  def minimum(t: TimeSpan): ExposureTimeLimits =
    ExposureTimeLimits(minimum = t.some)

  /**
   * One exposure in a step: what to call its configuration in a message, its
   * time, and the limits that apply to it.
   */
  final case class Exposure(
    subject: String,
    time:    TimeSpan,
    limits:  ExposureTimeLimits
  )

  /** Extracts the exposures in a step's instrument configuration. */
  trait Exposures[D]:
    def exposures(d: D): List[Exposure]

  object Exposures:

    def apply[D](using ev: Exposures[D]): Exposures[D] = ev

    given Exposures[Flamingos2DynamicConfig] = d =>
      List(Exposure(s"Flamingos 2 ${d.readMode.longName} read mode", d.exposure, minimum(d.readMode.minimumExposureTime)))

    val GhostMinimum: TimeSpan = 100.msTimeSpan

    given Exposures[GhostDynamicConfig] = d =>
      List(
        Exposure("the GHOST red camera",  d.red.value.exposureTime,  minimum(GhostMinimum)),
        Exposure("the GHOST blue camera", d.blue.value.exposureTime, minimum(GhostMinimum))
      )

    val GmosMinimum: TimeSpan = 1.secTimeSpan

    given Exposures[DynamicConfig.GmosNorth] = d =>
      List(Exposure("GMOS North", d.exposure, minimum(GmosMinimum)))

    given Exposures[DynamicConfig.GmosSouth] = d =>
      List(Exposure("GMOS South", d.exposure, minimum(GmosMinimum)))

    given Exposures[GnirsDynamicConfig] = d =>
      List(Exposure(s"GNIRS ${d.readMode.shortName} read mode", d.exposure, minimum(d.readMode.minimumExposureTime)))

    given Exposures[Igrins2DynamicConfig] = d =>
      List(Exposure("IGRINS-2", d.exposure, minimum(lucuma.core.model.sequence.igrins2.MinExposureTime)))

/**
 * Steps of a sequence whose exposure times fall outside the limits, grouped by
 * the configuration and the limit they violate.
 *
 * @param stepCount how many steps violate the limit
 * @param extreme   the shortest (below a minimum) or longest (above a maximum)
 *                  offending exposure time
 */
final case class ExposureTimeIssue(
  sequenceType: SequenceType,
  kind:         ExposureTimeIssue.Kind,
  subject:      String,
  limit:        TimeSpan,
  stepCount:    PosInt,
  extreme:      TimeSpan
)

object ExposureTimeIssue:

  enum Kind(val tag: String, val isError: Boolean, val isBelow: Boolean):
    case BelowMinimum            extends Kind("below_minimum",             true,  true)
    case BelowRecommendedMinimum extends Kind("below_recommended_minimum", false, true)
    case AboveRecommendedMaximum extends Kind("above_recommended_maximum", false, false)
    case AboveMaximum            extends Kind("above_maximum",             true,  false)

  object Kind:
    def fromTag(s: String): Option[Kind] =
      values.find(_.tag === s)

  /**
   * Accumulates issues while folding over the atoms of a sequence.  Bias steps
   * are skipped since they take no exposure.
   */
  final case class Accumulator(
    issues: Map[(SequenceType, Kind, String, TimeSpan), (PosInt, TimeSpan)]
  ):
    def add[D: ExposureTimeLimits.Exposures](sequenceType: SequenceType, atom: Atom[D]): Accumulator =
      atom.steps.foldLeft(this): (acc, step) =>
        step.stepConfig match
          case StepConfig.Bias => acc
          case _               =>
            ExposureTimeLimits.Exposures[D].exposures(step.instrumentConfig).foldLeft(acc): (acc2, e) =>
              e.limits.classify(e.time).fold(acc2): (kind, limit) =>
                acc2.copy(issues =
                  acc2.issues.updatedWith((sequenceType, kind, e.subject, limit)):
                    case None         => (PosInt.unsafeFrom(1), e.time).some
                    case Some((n, x)) => (PosInt.unsafeFrom(n.value + 1), if kind.isBelow then x min e.time else x max e.time).some
                )

    def toList: List[ExposureTimeIssue] =
      issues
        .toList
        .map:
          case ((st, k, s, l), (n, x)) => ExposureTimeIssue(st, k, s, l, n, x)
        .sortBy(i => (i.sequenceType.tag, i.kind.ordinal, i.subject))

  object Accumulator:
    val Empty: Accumulator = Accumulator(Map.empty)

  // Stored with the obscalc result, so that a workflow computed without
  // generating the sequence sees the issues found when it was last generated.

  private def micros(t: TimeSpan): Json =
    Json.fromLong(t.toMicroseconds)

  given Encoder[ExposureTimeIssue] = i =>
    Json.obj(
      "sequenceType" -> Json.fromString(i.sequenceType.tag),
      "kind"         -> Json.fromString(i.kind.tag),
      "subject"      -> Json.fromString(i.subject),
      "limit"        -> micros(i.limit),
      "stepCount"    -> Json.fromInt(i.stepCount.value),
      "extreme"      -> micros(i.extreme)
    )

  given Decoder[ExposureTimeIssue] = c =>
    def timeSpan(field: String): Decoder.Result[TimeSpan] =
      c.downField(field).as[Long].flatMap: n =>
        TimeSpan.fromMicroseconds(n).toRight(DecodingFailure(s"Invalid time span for '$field': $n", c.history))

    for
      st <- c.downField("sequenceType").as[String].flatMap: s =>
              SequenceType.values.find(_.tag === s).toRight(DecodingFailure(s"Invalid sequence type: $s", c.history))
      k  <- c.downField("kind").as[String].flatMap: s =>
              Kind.fromTag(s).toRight(DecodingFailure(s"Invalid exposure time issue kind: $s", c.history))
      s  <- c.downField("subject").as[String]
      l  <- timeSpan("limit")
      n  <- c.downField("stepCount").as[Int].flatMap: n =>
              PosInt.from(n).leftMap(m => DecodingFailure(m, c.history))
      x  <- timeSpan("extreme")
    yield ExposureTimeIssue(st, k, s, l, n, x)
