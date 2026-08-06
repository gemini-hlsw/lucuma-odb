// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.Monad
import cats.syntax.all.*
import grackle.Query
import grackle.Query.Binding
import grackle.Query.Environment
import grackle.Query.Group
import grackle.Query.UntypedSelect
import grackle.Result
import grackle.Value
import grackle.Value.ObjectValue
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.HourAngle
import lucuma.core.math.RightAscension
import lucuma.core.model.ConfigurationRequest
import lucuma.core.util.Gid
import lucuma.odb.graphql.binding.AngleBinding
import lucuma.odb.graphql.binding.BigDecimalBinding
import lucuma.odb.graphql.binding.LongBinding
import lucuma.odb.graphql.binding.StringBinding

/** Pre-compilation rewrite that resolves the `WhereConfigurationRequest.targetCoordinates`
 *  cone filter out of band: the cone is parsed from the (untyped) WHERE value, the
 *  matching `ConfigurationRequest.Id`s are computed by a supplied effect, and the
 *  `targetCoordinates` field is replaced with `id: { IN: [...] }` so that grackle
 *  compiles the whole WHERE as a single fully-pushable SQL statement.
 *
 *  Resolved out of band because the candidate lookup is an `F` effect that grackle's
 *  elaborator (a pure `StateT[Result, ElabState, *]`) cannot run.
 */
object ConeElaboration:

  private val ConfigurationRequestsField = "configurationRequests"
  private val WhereArg                   = "WHERE"
  private val TargetCoordinatesField     = "targetCoordinates"
  private val IdField                    = "id"

  /** Rewrite every `configurationRequests` selection in `query` whose WHERE has a
   *  `targetCoordinates` cone, replacing it with `id IN (candidateIds)`. */
  def rewriteCones[F[_]: Monad](
    query:  Query
  )(compute: (Coordinates, Angle) => F[Result[List[ConfigurationRequest.Id]]]): F[Result[Query]] =
    val cones  = collectCones(query).distinct
    val parsed = cones.traverse(v => Result.fromEither(parseCone(v)))
    if parsed.isFailure then Monad[F].pure(parsed.asInstanceOf[Result[Query]])
    else
      val coordAngles = parsed.toOption.get
      coordAngles.traverse { case (c, d) => compute(c, d) }.map { rs =>
        rs.sequence.map { idsList => inject(query, cones.zip(idsList).toMap) }
      }

  // --- pure tree walks ---

  // Collects every `targetCoordinates` cone value found in a `configurationRequests`
  // WHERE. `Query.mapFields` does not descend into `UntypedSelect`, so recurse manually.
  private def collectCones(q: Query): List[Value] =
    q match
      case Group(qs)                       => qs.flatMap(collectCones)
      case Environment(_, child)           => collectCones(child)
      case UntypedSelect(ConfigurationRequestsField, _, args, _, child) =>
        collectCones(child) ++ args.flatMap(coneInBinding)
      case us: UntypedSelect               => collectCones(us.child)
      case other                           => Nil

  private def coneInBinding(b: Binding): List[Value] =
    b match
      case Binding(WhereArg, ObjectValue(fields)) => fields.collect { case (TargetCoordinatesField, v) => v }
      case _                                      => Nil

  // Replaces each `targetCoordinates` cone with `id: { IN: [...] }`.
  private def inject(q: Query, coneIds: Map[Value, List[ConfigurationRequest.Id]]): Query =
    q match
      case Group(qs)                  => Group(qs.map(inject(_, coneIds)))
      case Environment(env, child)    => Environment(env, inject(child, coneIds))
      case us @ UntypedSelect(ConfigurationRequestsField, _, args, dirs, child) =>
        us.copy(args = args.map(injectBinding(_, coneIds)), child = inject(child, coneIds))
      case us: UntypedSelect          => us.copy(child = inject(us.child, coneIds))
      case other                      => other

  private def injectBinding(b: Binding, coneIds: Map[Value, List[ConfigurationRequest.Id]]): Binding =
    b match
      case Binding(WhereArg, ObjectValue(fields)) =>
        val newFields = fields.flatMap {
          case (TargetCoordinatesField, coneVal) =>
            coneIds.get(coneVal).map(ids => (IdField, idInValue(ids))).toList
          case other                              => List(other)
        }
        Binding(WhereArg, ObjectValue(newFields))
      case other => other

  private def idInValue(ids: List[ConfigurationRequest.Id]): Value =
    ObjectValue(List(("IN", Value.ListValue(ids.map(id => Value.StringValue(Gid[ConfigurationRequest.Id].show(id)))))))

  /** Replaces the query of an `UntypedOperation`, preserving its kind (query/mutation/subscription). */
  def withQuery(op: grackle.UntypedOperation, query: Query): grackle.UntypedOperation = op match
    case uo: grackle.UntypedOperation.UntypedQuery       => uo.copy(query = query)
    case uo: grackle.UntypedOperation.UntypedMutation     => uo.copy(query = query)
    case uo: grackle.UntypedOperation.UntypedSubscription => uo.copy(query = query)

  // --- cone parsing (CoordinatesInput + AngleInput, both @oneOf families) ---

  private def parseCone(v: Value): Either[String, (Coordinates, Angle)] =
    v match
      case ObjectValue(fields) =>
        for
          centerVal   <- fields.collectFirst { case ("center", c) => c }.toRight("missing `center`")
          distanceVal <- fields.collectFirst { case ("distance", d) => d }.toRight("missing `distance`")
          center      <- parseCoordinates(centerVal)
          distance    <- parseAngle(distanceVal)
        yield (center, distance)
      case other => Left("expected an object with `center` and `distance`")

  private def parseCoordinates(v: Value): Either[String, Coordinates] =
    v match
      case ObjectValue(fields) =>
        for
          raVal  <- fields.collectFirst { case ("ra", ra) => ra }.toRight("missing `ra`")
          decVal <- fields.collectFirst { case ("dec", dec) => dec }.toRight("missing `dec`")
          ra     <- parseRightAscension(raVal)
          dec    <- parseDeclination(decVal)
        yield Coordinates(ra, dec)
      case other => Left("`center` must be a CoordinatesInput")

  private def parseRightAscension(v: Value): Either[String, RightAscension] =
    oneField(v).flatMap:
      case ("microseconds", vv) => LongBinding.validate(vv).map(µs => RightAscension(HourAngle.fromMicroseconds(µs)))
      case ("degrees", vv)      => bigDecimal(vv).map(d => RightAscension.fromDoubleDegrees(d))
      case ("hours", vv)        => bigDecimal(vv).map(h => RightAscension(HourAngle.fromDoubleHours(h)))
      case ("hms", vv)          => StringBinding.validate(vv).flatMap(s => RightAscension.fromStringHMS.getOption(s).toRight(s"invalid hms: $s"))
      case (n, _)               => Left(s"unsupported RightAscensionInput form `$n`")

  private def parseDeclination(v: Value): Either[String, Declination] =
    oneField(v).flatMap:
      case ("microarcseconds", vv) => LongBinding.validate(vv).flatMap(µas => Declination.fromAngle.getOption(Angle.fromMicroarcseconds(µas)).toRight("invalid declination"))
      case ("degrees", vv)         => bigDecimal(vv).flatMap(d => Declination.fromDoubleDegrees(d).toRight("invalid declination"))
      case ("dms", vv)             => StringBinding.validate(vv).flatMap(s => Declination.fromStringSignedDMS.getOption(s).toRight(s"invalid dms: $s"))
      case (n, _)                  => Left(s"unsupported DeclinationInput form `$n`")

  private def parseAngle(v: Value): Either[String, Angle] =
    oneField(v).flatMap:
      case ("microarcseconds", vv) => LongBinding.validate(vv).map(Angle.fromMicroarcseconds)
      case ("dms", vv)             => AngleBinding.Dms.validate(vv)
      case (n, vv)                 => angleFactor(n).flatMap { factor => bigDecimal(vv).map(d => Angle.fromDoubleDegrees(d * factor)) }

  // oneOf inputs carry exactly one field.
  private def oneField(v: Value): Either[String, (String, Value)] =
    v match
      case ObjectValue((n, vv) :: Nil) => Right((n, vv))
      case ObjectValue(Nil)            => Left("expected exactly one field, found none")
      case ObjectValue(other)          => Left(s"expected exactly one field, found ${other.map(_._1)}")
      case other                       => Left("expected a single-field input object")

  // Conversion factor: multiply a value in this unit to obtain degrees. Covers
  // both arc units and time units (a time-second subtends 15 arc-seconds).
  private def angleFactor(unit: String): Either[String, Double] =
    val TimeArcsecPerSec = 15.0
    unit match
      case "milliarcseconds" => Right(1e-3 / 3600.0)
      case "arcseconds"      => Right(1.0 / 3600.0)
      case "arcminutes"      => Right(1.0 / 60.0)
      case "degrees"         => Right(1.0)
      case "microseconds"    => Right(TimeArcsecPerSec / 3600.0 / 1e6)
      case "milliseconds"    => Right(TimeArcsecPerSec / 3600.0 / 1e3)
      case "seconds"         => Right(TimeArcsecPerSec / 3600.0)
      case "minutes"         => Right(TimeArcsecPerSec / 60.0)
      case "hours"           => Right(TimeArcsecPerSec)
      case other             => Left(s"unsupported AngleInput form `$other`")

  private def bigDecimal(v: Value): Either[String, Double] =
    BigDecimalBinding.validate(v).map(_.toDouble)
