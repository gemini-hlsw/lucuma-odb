// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet

import cats.effect.Sync
import cats.effect.kernel.Resource
import cats.syntax.all._
import edu.gemini.grackle.Cursor.Env
import edu.gemini.grackle.Path.{ListPath, UniquePath}
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Result
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.core.enums.CloudExtinction
import lucuma.core.enums.ImageQuality
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.model.Access._
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.odb.data.Existence
import lucuma.odb.data.ObsActiveStatus
import lucuma.odb.data.ObsStatus
import lucuma.odb.data.UpdateResult
import lucuma.odb.graphql.snippet.input.CreateObservationInput
import lucuma.odb.graphql.snippet.input.ObservationPropertiesInput
import lucuma.odb.graphql.snippet.input.UpdateObservationsInput
import lucuma.odb.graphql.snippet.input.WhereObservation
import lucuma.odb.graphql.util.Bindings.BooleanBinding
import lucuma.odb.graphql.util._
import lucuma.odb.service.ObservationService
import lucuma.odb.service.ObservationService.CreateResult.NotAuthorized
import lucuma.odb.service.ObservationService.CreateResult.Success
import lucuma.odb.util.Codecs._
import natchez.Trace
import skunk.Session

object ObservationSnippet {

  def apply[F[_]: Sync: Trace](
    m:      SnippetMapping[F] with SkunkMapping[F] with MutationCompanionOps[F],
    dbPool: Resource[F, Session[F]],
    user:   User
  ): m.Snippet = {

    import m.{ ColumnRef, TableDef, ObjectMapping, Join, Snippet, SqlField, SqlObject, Mutation, MutationCompanionOps, SqlRoot, LeafMapping, col, schema }

    // The types that we're going to map.
    val QueryType           = schema.ref("Query")
    val MutationType        = schema.ref("Mutation")
    val ObservationType     = schema.ref("Observation")
    val ObservationIdType   = schema.ref("ObservationId")
    val ObsStatusType       = schema.ref("ObsStatus")
    val ObsActiveStatusType = schema.ref("ObsActiveStatus")
    val CreateObservationResultType  = schema.ref("CreateObservationResult")

    val ConstraintSetType   = schema.ref("ConstraintSet")
    val CloudExtinctionType = schema.ref("CloudExtinction")
    val ImageQualityType    = schema.ref("ImageQuality")
    val SkyBackgroundType   = schema.ref("SkyBackground")
    val WaterVaporType      = schema.ref("WaterVapor")
    val ElevationRangeType  = schema.ref("ElevationRange")
    val AirMassRangeType    = schema.ref("AirMassRange")
    val HourAngleRangeType  = schema.ref("HourAngleRange")

    val pool = dbPool.map(ObservationService.fromSessionAndUser(_, user))

    // TODO: Can we share the common predicates somewhere?
    object Predicates {

      def includeDeleted(b: Boolean): Predicate =
        if (b) True else Eql(UniquePath(List("existence")), Const[Existence](Existence.Present))

      def hasObservationId(oid: Observation.Id): Predicate =
        Eql(UniquePath(List("id")), Const(oid))

      def inObservationIds(oids: List[Observation.Id]): Predicate =
        In(UniquePath(List("id")), oids)

      def isVisibleTo(user: User): Predicate =
        user.role.access match {
          case Guest | Pi =>
            Or(
              Contains(ListPath(List("program", "users", "userId")), Const(user.id)), // user is linked, or
              Eql(UniquePath(List("program", "piUserId")), Const(user.id))            // user is the PI
            )
          case Ngo => ???
          case Staff | Admin | Service => True
        }
    }

    // Column references for our mapping.
    object ObservationView extends TableDef("v_observation") {
      val ProgramId: ColumnRef    = col("c_program_id",          program_id)
      val Id: ColumnRef           = col("c_observation_id",      observation_id)
      val Existence: ColumnRef    = col("c_existence",           existence)
      val Subtitle: ColumnRef     = col("c_subtitle",            text_nonempty.opt)
//      val Instrument: m.ColumnRef   = col("c_instrument", tag.opt)
      val Status: ColumnRef       = col("c_status",              obs_status)
      val ActiveStatus: ColumnRef = col("c_active_status",       obs_active_status)
      object ConstraintSet {
        val CloudExtinction: ColumnRef = col("c_cloud_extinction", cloud_extinction.embedded)
        val ImageQuality: ColumnRef    = col("c_image_quality",    image_quality.embedded)
        val SkyBackground: ColumnRef   = col("c_sky_background",   sky_background.embedded)
        val WaterVapor: ColumnRef      = col("c_water_vapor",      water_vapor.embedded)
        object ElevationRange {
          object AirMassRange {
            val SyntheticId: ColumnRef = col("c_air_mass_id",  observation_id.embedded)
            val AirMassMin: ColumnRef  = col("c_air_mass_min", air_mass_range_value.embedded)
            val AirMassMax: ColumnRef  = col("c_air_mass_max", air_mass_range_value.embedded)
          }
          object HourAngleRange {
            val SyntheticId: ColumnRef  = col("c_hour_angle_id",  observation_id.embedded)
            val HourAngleMin: ColumnRef = col("c_hour_angle_min", hour_angle_range_value.embedded)
            val HourAngleMax: ColumnRef = col("c_hour_angle_max", hour_angle_range_value.embedded)
          }
        }
      }
    }

    // Column references for our mapping.
    object ProgramTable extends TableDef("t_program") {
      val Id: ColumnRef = col("c_program_id", program_id)
    }

    def uniqueObservationNoFiltering(id: Observation.Id, child: Query): Query =
      Unique(Filter(Predicates.hasObservationId(id), child))

    def observationListNoFiltering(ids: List[Observation.Id], child: Query): Query =
      Filter(Predicates.inObservationIds(ids), child)

    val createObservation: Mutation =
      Mutation.simple { (child, env) =>
        env.getR[CreateObservationInput]("input").flatTraverse { input =>
          pool.use { svc =>
            svc.createObservation(input.programId, input.SET.getOrElse(ObservationPropertiesInput.DefaultCreate)).map {
              case NotAuthorized(user) => Result.failure(s"User ${user.id} is not authorized to perform this action")
              case Success(id)         => Result(uniqueObservationNoFiltering(id, child))
            }
          }
        }
      }

    // TODO: probably delete
//    val updateObservationsOneByOne: Mutation =
//      Mutation.simple { (child, env) =>
//        env.getR[UpdateObservationsInput]("input").flatTraverse { input =>
//          pool.use { svc =>
//
//            val updateResults: F[List[(Observation.Id, UpdateResult[Observation.Id])]] =
//              input.WHERE.toList.flatten.traverse { oid =>
//                svc.updateObservation(oid, input.SET).tupleLeft(oid)
//              }
//
//            updateResults.map { lst =>
//              lst.foldLeft(List.empty[Observation.Id].rightIor[String].toIorNes) {
//                case (ior, (_, UpdateResult.NothingToBeDone)) => ior.addLeft(NonEmptySet.one("No updates specified."))
//                case (ior, (oid, UpdateResult.NoSuchObject )) => ior.addLeft(NonEmptySet.one(s"Observation $oid does not exist or is not editable by user ${user.id}."))
//                case (ior, (_,   UpdateResult.Success(oid) )) => ior.addRight(List(oid))
//              }.bimap(
//                ms  => NonEmptyChain.fromNonEmptyList(ms.toNonEmptyList.map(m => Problem(m))),
//                oids => observationListNoFiltering(oids, child))
//            }
//          }
//        }
//      }

    // TODO: This seems closer to what we want than the one-by-one approach above?
    val updateObservations: Mutation =
      Mutation.simple { (child, env) =>
        env.getR[UpdateObservationsInput]("input").flatTraverse { input =>
          pool.use { svc =>
            svc
              .updateObservations(input.WHERE.toList.flatten, input.SET)
              .map {
                case UpdateResult.NothingToBeDone => Result.failure("No updates specified.")
                case UpdateResult.NoSuchObject    => Result.failure(s"No matching editable observations for user ${user.id}")
                case UpdateResult.Success(ids)    => Result(observationListNoFiltering(ids, child))
              }
          }
        }
      }

    val typeMappings =
      List(
        ObjectMapping(
          tpe = ObservationType,
          fieldMappings = List(
            SqlField("id", ObservationView.Id, key = true),
            SqlField("programId", ObservationView.ProgramId, hidden=true),
            SqlField("existence", ObservationView.Existence, hidden = true),
            SqlField("subtitle", ObservationView.Subtitle),
            SqlField("status", ObservationView.Status),
            SqlField("activeStatus", ObservationView.ActiveStatus),
            SqlObject("constraintSet"),
            SqlObject("program", Join(ObservationView.ProgramId, ProgramTable.Id))
          ),
        ),
        ObjectMapping(
          tpe = ConstraintSetType,
          fieldMappings = List(
            SqlField("id", ObservationView.Id, key = true, hidden = true),
            SqlField("cloudExtinction", ObservationView.ConstraintSet.CloudExtinction),
            SqlField("imageQuality",    ObservationView.ConstraintSet.ImageQuality),
            SqlField("skyBackground",   ObservationView.ConstraintSet.SkyBackground),
            SqlField("waterVapor",      ObservationView.ConstraintSet.WaterVapor),
            SqlObject("elevationRange")
          )
        ),
        ObjectMapping(
          tpe = ElevationRangeType,
          fieldMappings = List(
            SqlField("id", ObservationView.Id, key = true, hidden = true),
            SqlObject("airMass"),
            SqlObject("hourAngle")
          )
        ),
        ObjectMapping(
          tpe = AirMassRangeType,
          fieldMappings = List(
            SqlField("synthetic_id", ObservationView.ConstraintSet.ElevationRange.AirMassRange.SyntheticId, key = true, hidden = true),
            SqlField("min", ObservationView.ConstraintSet.ElevationRange.AirMassRange.AirMassMin),
            SqlField("max", ObservationView.ConstraintSet.ElevationRange.AirMassRange.AirMassMax)
          )
        ),
        ObjectMapping(
          tpe = HourAngleRangeType,
          fieldMappings = List(
            SqlField("synthetic_id", ObservationView.ConstraintSet.ElevationRange.HourAngleRange.SyntheticId, key = true, hidden = true),
            SqlField("minHours", ObservationView.ConstraintSet.ElevationRange.HourAngleRange.HourAngleMin),
            SqlField("maxHours", ObservationView.ConstraintSet.ElevationRange.HourAngleRange.HourAngleMax)
          )
        ),
        ObjectMapping(
          tpe = MutationType,
          fieldMappings = List(
            SqlRoot("createObservation", mutation = createObservation),
            SqlRoot("updateObservations", mutation = updateObservations)
          )
        ),
        ObjectMapping(
          tpe = CreateObservationResultType,
          fieldMappings = List(
            SqlField("id", ObservationView.Id, key = true, hidden = true),
            SqlObject("observation"),
          )
        ),
        ObjectMapping(
          tpe = QueryType,
          fieldMappings = List(
            SqlRoot("observation"),
            SqlRoot("observations")
          )
        ),
        LeafMapping[Observation.Id](ObservationIdType),
        LeafMapping[ObsStatus](ObsStatusType),
        LeafMapping[ObsActiveStatus](ObsActiveStatusType),
        LeafMapping[CloudExtinction](CloudExtinctionType),
        LeafMapping[ImageQuality](ImageQualityType),
        LeafMapping[SkyBackground](SkyBackgroundType),
        LeafMapping[WaterVapor](WaterVaporType)
      )

    val elaborator = Map[TypeRef, PartialFunction[Select, Result[Query]]](
      QueryType -> {
        case Select("observation", List(
          ObservationIdBinding("observationId", rOid)
        ), child) =>
          rOid.map { oid =>
            Select("observation", Nil,
              Unique(
                Filter(
                  And(
                    Predicates.hasObservationId(oid),
                    Predicates.isVisibleTo(user)
                  ),
                  child
                )
              )
            )
          }

        case Select("observations", List(
          WhereObservation.Binding.Option("WHERE", rWHERE),
          ObservationIdBinding.Option("OFFSET", rOFFSET),
          NonNegIntBinding.Option("LIMIT", rLIMIT),
          BooleanBinding("includeDeleted", rIncludeDeleted)
        ), child) =>
          (rWHERE, rOFFSET, rLIMIT, rIncludeDeleted).parMapN { (WHERE, OFFSET, _, includeDeleted) =>
            Select("observations", Nil,
              Filter(
                And.all(
                  OFFSET.map(oid => GtEql(UniquePath(List("id")), Const(oid))).getOrElse(True),
                  Predicates.includeDeleted(includeDeleted),
                  Predicates.isVisibleTo(user),
                  WHERE.getOrElse(True)
                ),
                child
                // Limit(
                //    LIMIT.foldLeft(1000)(_ min _.value),
                //   child
                // )
              )
            )
          }

      },

      MutationType -> {
        case Select("createObservation", List(CreateObservationInput.Binding("input", rInput)), child) =>
          rInput.map { input =>
            Environment(Env("input" -> input), Select("createObservation", Nil, child))
          }

        case Select("updateObservations", List(UpdateObservationsInput.Binding("input", rInput)), child) =>
          rInput.map { input =>
            Environment(Env("input" -> input), Select("updateObservations", Nil, child))
          }
      }
    )

    // Done.
    Snippet(typeMappings, elaborator)

  }


}
