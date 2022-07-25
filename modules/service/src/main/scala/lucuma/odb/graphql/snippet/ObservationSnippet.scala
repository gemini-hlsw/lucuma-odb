// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet

import cats.effect.MonadCancelThrow
import cats.effect.kernel.Resource
import cats.syntax.all._
import edu.gemini.grackle.Cursor.Env
import edu.gemini.grackle.Path.UniquePath
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Result
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.Value._
import edu.gemini.grackle.skunk.SkunkMapping
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.{Encoder, Json}
import lucuma.core.enums.{CloudExtinction, ImageQuality, SkyBackground, WaterVapor}
import lucuma.core.model.ElevationRange.AirMass.DecimalValue
import lucuma.core.model.ElevationRange.HourAngle.DecimalHour
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.Existence
import lucuma.odb.data.ObsActiveStatus
import lucuma.odb.data.ObsStatus
import lucuma.odb.graphql.util._
import lucuma.odb.service.{ConstraintSetService, ObservationService}
import lucuma.odb.service.ObservationService.InsertObservationResponse.NotAuthorized
import lucuma.odb.service.ObservationService.InsertObservationResponse.Success
import lucuma.odb.util.Codecs._
import skunk.Session

object ObservationSnippet {

  final case class Services[F[_]](
    obs: ObservationService[F],
    cs:  ConstraintSetService[F]
  )

  def apply[F[_]: MonadCancelThrow](
    m:      SnippetMapping[F] with SkunkMapping[F] with MutationCompanionOps[F],
    dbPool: Resource[F, Session[F]],
    user:   User
  ): m.Snippet = {

    import m.{ TableDef, ObjectMapping, Join, Snippet, SqlField, SqlObject, Mutation, MutationCompanionOps, SqlRoot, LeafMapping, col, schema }

    // The types that we're going to map.
    // val QueryType         = schema.ref("Query")
    val MutationType        = schema.ref("Mutation")
    // val SubscriptionType  = schema.ref("Subscription")
    val ObservationType     = schema.ref("Observation")
    val ObservationIdType   = schema.ref("ObservationId")
    val ObsStatusType       = schema.ref("ObsStatus")
    val ObsActiveStatusType = schema.ref("ObsActiveStatus")

    val ConstraintSetType   = schema.ref("ConstraintSet")
    val CloudExtinctionType = schema.ref("CloudExtinction")
    val ImageQualityType    = schema.ref("ImageQuality")
    val SkyBackgroundType   = schema.ref("SkyBackground")
    val WaterVaporType      = schema.ref("WaterVapor")
    val ElevationRangeType  = schema.ref("ElevationRange")
    val AirMassRangeType    = schema.ref("AirMassRange")
    val HourAngleRangeType  = schema.ref("HourAngleRange")

    val pool = dbPool.map { s =>
      Services[F](
        ObservationService.fromUserAndSession(user, s),
        ConstraintSetService.fromSession(user, s)
      )
    }

    object Predicates {

//      def includeDeleted(b: Boolean): Predicate =
//        if (b) True else Eql(UniquePath(List("existence")), Const[Existence](Existence.Present))

      def hasObservationId(oid: Observation.Id): Predicate =
        Eql(UniquePath(List("id")), Const(oid))

    }

    // Column references for our mapping.
    object ObservationTable extends TableDef("t_observation") {
      val ProgramId: m.ColumnRef    = col("c_program_id", program_id)
      val Id: m.ColumnRef           = col("c_observation_id", observation_id)
      val Existence: m.ColumnRef    = col("c_existence", existence)
      val Name: m.ColumnRef         = col("c_name", text_nonempty.opt)
//      val Instrument: m.ColumnRef   = col("c_instrument", tag.opt)
      val Status: m.ColumnRef       = col("c_status", obs_status)
      val ActiveStatus: m.ColumnRef = col("c_active_status", obs_active_status)
    }

    object ConstraintSetTable extends TableDef("t_constraint_set") {
      val ObservationId: m.ColumnRef   = col("c_observation_id", observation_id)
      val CloudExtinction: m.ColumnRef = col("c_cloud_extinction", cloud_extinction)
      val ImageQuality: m.ColumnRef    = col("c_image_quality", image_quality)
      val SkyBackground: m.ColumnRef   = col("c_sky_background", sky_background)
      val WaterVapor: m.ColumnRef      = col("c_water_vapor", water_vapor)

      val AirMassMin: m.ColumnRef      = col("c_air_mass_min", air_mass_range_value)
      val AirMassMax: m.ColumnRef      = col("c_air_mass_max", air_mass_range_value)
      val HourAngleMin: m.ColumnRef    = col("c_hour_angle_min", hour_angle_range_value)
      val HourAngleMax: m.ColumnRef    = col("c_hour_angle_max", hour_angle_range_value)
    }

    // Column references for our mapping.
    object ProgramTable extends TableDef("t_program") {
      val Id: m.ColumnRef = col("c_program_id", program_id)
    }

    def uniqueObservationNoFiltering(id: Observation.Id, child: Query): Result[Query] =
      Result(Unique(Filter(Predicates.hasObservationId(id), child)))

    val insertObservation: Mutation =
      Mutation.simple { (child, env) =>
        (
          env.getR[Program.Id]("programId"),
          env.getR[Option[NonEmptyString]]("name"),
          env.getR[Existence]("existence"),
          env.getR[ObsStatus]("obsStatus"),
          env.getR[ObsActiveStatus]("obsActiveStatus"),
        ).parTupled.flatTraverse { case (pid, name, existence, os, as) =>
          pool.use { svc =>
            svc.obs.insertObservation(pid, name, existence, os, as).map {
              case NotAuthorized(user) => Result.failure(s"User ${user.id} is not authorized to perform this action")
              case Success(id)         => uniqueObservationNoFiltering(id, child)
            }
          }
        }
      }

    implicit val EncoderDecimalValue: Encoder[DecimalValue] =
      (a: DecimalValue) => Json.fromBigDecimal(a.value)

    implicit val EncoderDecimalHour: Encoder[DecimalHour] =
      (a: DecimalHour) => Json.fromBigDecimal(a.value)

    val typeMappings =
      List(
        ObjectMapping(
          tpe = ObservationType,
          fieldMappings = List(
            SqlField("id", ObservationTable.Id, key = true),
            SqlField("programId", ObservationTable.ProgramId, hidden=true),
            SqlField("existence", ObservationTable.Existence, hidden = true),
            SqlField("name", ObservationTable.Name),
            SqlField("status", ObservationTable.Status),
            SqlField("activeStatus", ObservationTable.ActiveStatus),
            SqlObject("program", Join(ObservationTable.ProgramId, ProgramTable.Id)),
          ),
        ),
        ObjectMapping(
          tpe = ConstraintSetType,
          fieldMappings = List(
            SqlField("observationId",   ConstraintSetTable.ObservationId, key = true),
            SqlField("cloudExtinction", ConstraintSetTable.CloudExtinction),
            SqlField("imageQuality",    ConstraintSetTable.ImageQuality),
            SqlField("skyBackground",   ConstraintSetTable.SkyBackground),
            SqlField("waterVapor",      ConstraintSetTable.WaterVapor),
            SqlObject("elevationRange")
          )
        ),
        ObjectMapping(
          tpe = ElevationRangeType,
          fieldMappings = List(
            SqlObject("airMassRange"),
            SqlObject("hourAngleRange")
          )
        ),
        ObjectMapping(
          tpe = AirMassRangeType,
          fieldMappings = List(
            SqlField("min", ConstraintSetTable.AirMassMin),
            SqlField("max", ConstraintSetTable.AirMassMax),
          )
        ),
        ObjectMapping(
          tpe = HourAngleRangeType,
          fieldMappings = List(
            SqlField("minHours", ConstraintSetTable.HourAngleMin),
            SqlField("maxHours", ConstraintSetTable.HourAngleMax)
          )
        ),
        ObjectMapping(
          tpe = MutationType,
          fieldMappings = List(
            SqlRoot("createObservation", mutation = insertObservation),
          )
        ),
        LeafMapping[Observation.Id](ObservationIdType),
        LeafMapping[ObsStatus](ObsStatusType),
        LeafMapping[ObsActiveStatus](ObsActiveStatusType),
        LeafMapping[CloudExtinction](CloudExtinctionType),
        LeafMapping[ImageQuality](ImageQualityType),
        LeafMapping[SkyBackground](SkyBackgroundType),
        LeafMapping[WaterVapor](WaterVaporType),
        LeafMapping[DecimalValue](AirMassRangeType),
        LeafMapping[DecimalHour](HourAngleRangeType)
      )

    val elaborator = Map[TypeRef, PartialFunction[Select, Result[Query]]](
      MutationType -> {
        case Select("createObservation", List(
          Binding("input", ObjectValue(List(
            ProgramIdBinding("programId", rPid),
            NonEmptyStringBinding.Option("name", rName),
            ObsStatusBinding.Option("status", rStatus),
            ObsActiveStatusBinding.Option("activeStatus", rActiveStatus),
          )))
        ), child) =>
          (rPid, rName, rStatus, rActiveStatus).parMapN { (pid, name, status, activeStatus) =>
            Environment(
              Env(
                "programId"       -> pid,
                "name"            -> name,
                "existence"       -> Existence.Present,
                "obsStatus"       -> status.getOrElse(ObsStatus.Default),
                "obsActiveStatus" -> activeStatus.getOrElse(ObsActiveStatus.Default),
              ),
              Select("createObservation", Nil, child)
            )
          }
      },
    )

    // Done.
    Snippet(typeMappings, elaborator)

  }


}
