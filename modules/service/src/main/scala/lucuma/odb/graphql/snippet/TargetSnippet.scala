package lucuma.odb.graphql
package snippet

import cats.effect.MonadCancelThrow
import cats.effect.kernel.Resource
import cats.syntax.all._
import edu.gemini.grackle.Cursor.Env
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Result
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.graphql.snippet.input.CreateTargetInput
import lucuma.odb.graphql.util._
import lucuma.odb.service.TargetService
import lucuma.odb.util.Codecs._
import skunk.Session
import skunk.circe.codec.json.jsonb
import skunk.codec.all._
import lucuma.core.model.Target
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Path.UniquePath
import lucuma.odb.data.Existence
import lucuma.core.math.Epoch
import io.circe.Encoder
import io.circe.Json
import scala.reflect.ClassTag
import lucuma.core.math.Angle
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Parallax
import java.math.MathContext
import lucuma.core.enums.EphemerisKeyType

object TargetSnippet {
  import TargetService.CreateTargetResponse._

  def apply[F[_]: MonadCancelThrow](
    m: SnippetMapping[F] with SkunkMapping[F] with MutationCompanionOps[F],
    sessionPool: Resource[F, Session[F]],
    user: User,
  ): m.Snippet = {

    import m.CursorField
    import m.Join
    import m.Mutation
    import m.MutationCompanionOps
    import m.ObjectMapping
    import m.Snippet
    import m.SqlField
    import m.SqlJson
    import m.SqlObject
    import m.SqlRoot
    import m.TableDef
    import m.LeafMapping
    import m.col
    import m.schema

    val QueryType                   = schema.ref("Query")
    val MutationType                = schema.ref("Mutation")
    val TargetType                  = schema.ref("Target")
    val SiderealType                = schema.ref("Sidereal")
    val NonsiderealType             = schema.ref("Nonsidereal")
    val CatalogInfoType             = schema.ref("CatalogInfo")
    val RightAscensionType          = schema.ref("RightAscension")
    val DeclinationType             = schema.ref("Declination")
    val ProperMotionType            = schema.ref("ProperMotion")
    val ProperMotionDeclinationType = schema.ref("ProperMotionDeclination")
    val ProperMotionRAType          = schema.ref("ProperMotionRA")
    val RadialVelocityType          = schema.ref("RadialVelocity")
    val ParallaxType                = schema.ref("Parallax")
    val TargetIdType                = schema.ref("TargetId")
    val EpochStringType             = schema.ref("EpochString")
    val EphemerisKeyTypeType        = schema.ref("EphemerisKeyType")

    val servicePool: Resource[F, TargetService[F]] =
      sessionPool.map(TargetService.fromSession(_, user))

    object TargetView extends TableDef("v_target") {
      val Id            = col("c_program_id", program_id)
      val TargetId      = col("c_target_id", target_id)
      val Name          = col("c_name", text_nonempty)
      val Existence     = col("c_existence", existence)
      val SourceProfile = col("c_source_profile", jsonb)
      object Sidereal {
        val SyntheticId    = col("c_sidereal_id", target_id.embedded)
        val Ra             = col("c_sid_ra", right_ascension.embedded)
        val Dec            = col("c_sid_dec", declination.embedded)
        val Epoch          = col("c_sid_epoch", epoch.embedded)
        object RadialVelocity {
          val SyntheticId = col("c_sid_rv_id", target_id.embedded)
          val Value       = col("c_sid_rv", radial_velocity.embedded)
        }
        object Parallax {
          val SyntheticId = col("c_sid_parallax_id", target_id.embedded)
          val Value = col("c_sid_parallax", parallax.embedded)
        }
        object Catalog {
          val SyntheticId = col("c_sid_catalog_info_id", target_id.embedded)
          val Name        = col("c_sid_catalog_name", catalog_name.embedded)
          val Id          = col("c_sid_catalog_id", varchar.embedded)
          val ObjectType  = col("c_sid_catalog_object_type", varchar.embedded)
        }
        object ProperMotion {
          val SyntheticId = col("c_sid_pm_id", target_id.embedded)
          val Ra  = col("c_sid_pm_ra", angle_µas.embedded)
          val Dec = col("c_sid_pm_dec", angle_µas.embedded)
        }
      }
      object Nonsidereal {
        val SyntheticId    = col("c_nonsidereal_id", target_id.embedded)
        val Des     = col("c_nsid_des", varchar.embedded)
        val KeyType = col("c_nsid_key_type", ephemeris_key_type.embedded)
        val Key     = col("c_nsid_key", varchar.embedded)
      }
    }

    object ProgramTable extends TableDef("t_program") {
      val Id  = col("c_program_id", program_id)
    }

    object Predicates {

      def includeDeleted(b: Boolean): Predicate =
        if (b) True else Eql(UniquePath(List("existence")), Const[Existence](Existence.Present))

      def hasTargetId(oid: Target.Id): Predicate =
        Eql(UniquePath(List("id")), Const(oid))

    }

    def uniqueTargetNoFiltering(id: Target.Id, child: Query): Result[Query] =
      Result(Unique(Filter(Predicates.hasTargetId(id), child)))

    val createTarget: Mutation =
      Mutation.simple { (child, env) =>
        (env.getR[Program.Id]("pid"),
         env.getR[CreateTargetInput]("input")
        ).parTupled.flatTraverse { case (pid, input) =>
          servicePool.use { ts =>
            ts.createTarget(pid, input).map {
              case NotAuthorized(user)  => Result.failure(s"User ${user.id} is not authorized to perform this action")
              case ProgramNotFound(pid) => Result.failure(s"Program ${pid} was not found")
              case Success(id)          => uniqueTargetNoFiltering(id, child)
            }
          }
        }
      }

    // this is required by the leaf mapping
    implicit val EncoderEpoch: Encoder[Epoch] =
      e => Json.fromString(Epoch.fromString.reverseGet(e))

    object FieldRef {
      def apply[A](underlyingField: String) = new Partial[A](underlyingField)
      class Partial[A](underlyingField: String) {
        def as[B: Encoder](field: String, f: A => B)(implicit ev: ClassTag[A]): CursorField[B] =
          CursorField(field, c => c.field(underlyingField, None).flatMap(_.as[A].map(f)), List(underlyingField))
      }
    }

    val typeMappings = List(
      LeafMapping[Target.Id](TargetIdType),
      LeafMapping[Epoch](EpochStringType),
      LeafMapping[EphemerisKeyType](EphemerisKeyTypeType),
      ObjectMapping(
        tpe = TargetType, // top-level type
        fieldMappings = List(
          SqlField("id", TargetView.Id, key = true),
          SqlField("existence", TargetView.Existence),
          SqlField("name", TargetView.Name),
          SqlObject("program", Join(TargetView.Id, ProgramTable.Id)),
          SqlJson("sourceProfile", TargetView.SourceProfile),
          SqlObject("sidereal"),
          SqlObject("nonsidereal"),
        ),
      ),
      ObjectMapping(
        tpe = SiderealType,
        fieldMappings = List(
          SqlField("synthetic_id", TargetView.Sidereal.SyntheticId, key = true, hidden = true),
          SqlObject("ra"),
          SqlObject("dec"),
          SqlField("epoch", TargetView.Sidereal.Epoch),
          SqlObject("properMotion"),
          SqlObject("radialVelocity"),
          SqlObject("parallax"),
          SqlObject("catalogInfo"),
        ),
      ),
      ObjectMapping(
        tpe = RightAscensionType,
        fieldMappings = List(
          SqlField("synthetic_id", TargetView.Sidereal.SyntheticId, key = true, hidden = true),
          SqlField("value", TargetView.Sidereal.Ra, hidden = true),
          FieldRef[RightAscension]("value").as("hms", RightAscension.fromStringHMS.reverseGet),
          FieldRef[RightAscension]("value").as("hours", c => BigDecimal(c.toHourAngle.toDoubleHours)),
          FieldRef[RightAscension]("value").as("degrees", c => BigDecimal(c.toAngle.toDoubleDegrees)),
          FieldRef[RightAscension]("value").as("microarcseconds", _.toAngle.toMicroarcseconds),
        ),
      ),
      ObjectMapping(
        tpe = DeclinationType,
        fieldMappings = List(
          SqlField("synthetic_id", TargetView.Sidereal.SyntheticId, key = true, hidden = true),
          SqlField("value", TargetView.Sidereal.Dec, hidden = true),
          FieldRef[Declination]("value").as("dms", Declination.fromStringSignedDMS.reverseGet),
          FieldRef[Declination]("value").as("degrees", c => BigDecimal(c.toAngle.toDoubleDegrees)),
          FieldRef[Declination]("value").as("microarcseconds", _.toAngle.toMicroarcseconds),
        ),
      ),
      ObjectMapping(
        tpe = ProperMotionType,
        fieldMappings = List(
          SqlField("synthetic_id", TargetView.Sidereal.ProperMotion.SyntheticId, key = true, hidden = true),
          SqlObject("ra"),
          SqlObject("dec"),
        )
      ),
      ObjectMapping(
        tpe = ProperMotionRAType,
        fieldMappings = List(
          SqlField("synthetic_id", TargetView.Sidereal.ProperMotion.SyntheticId, key = true, hidden = true),
          SqlField("value", TargetView.Sidereal.ProperMotion.Ra, hidden = true),
          FieldRef[Angle]("value").as("microarcsecondsPerYear", _.toMicroarcseconds),
          FieldRef[Angle]("value").as("milliarcsecondsPerYear", a => BigDecimal(a.toMicroarcseconds) / BigDecimal(1000)),
        )
      ),
      ObjectMapping(
        tpe = ProperMotionDeclinationType,
        fieldMappings = List(
          SqlField("synthetic_id", TargetView.Sidereal.ProperMotion.SyntheticId, key = true, hidden = true),
          SqlField("value", TargetView.Sidereal.ProperMotion.Dec, hidden = true),
          FieldRef[Angle]("value").as("microarcsecondsPerYear", _.toMicroarcseconds),
          FieldRef[Angle]("value").as("milliarcsecondsPerYear", a => BigDecimal(a.toMicroarcseconds) / BigDecimal(1000)),
        )
      ),
      ObjectMapping(
        tpe = RadialVelocityType,
        fieldMappings = List(
          SqlField("synthetic_id", TargetView.Sidereal.RadialVelocity.SyntheticId, key = true, hidden = true),
          SqlField("value", TargetView.Sidereal.RadialVelocity.Value, hidden = true),
          FieldRef[RadialVelocity]("value").as("metersPerSecond", _.rv.value),
          FieldRef[RadialVelocity]("value").as("kilometersPerSecond", _.rv.value / BigDecimal(1000)),
          FieldRef[RadialVelocity]("value").as("centimetersPerSecond", _.rv.value.toLong * 100L),
        )
      ),
      ObjectMapping(
        tpe = ParallaxType,
        fieldMappings = List(
          SqlField("synthetic_id", TargetView.Sidereal.Parallax.SyntheticId, key = true, hidden = true),
          SqlField("value", TargetView.Sidereal.Parallax.Value, hidden = true),
          FieldRef[Parallax]("value").as("microarcseconds", _.μas.value.value),
          FieldRef[Parallax]("value").as("milliarcseconds", a => a.mas.value.toBigDecimal(MathContext.DECIMAL128)),
        )
      ),
      ObjectMapping(
        tpe = CatalogInfoType,
        fieldMappings = List(
          SqlField("synthetic_id", TargetView.Sidereal.Catalog.SyntheticId, key = true, hidden = true),
          SqlField("name", TargetView.Sidereal.Catalog.Name),
          SqlField("id", TargetView.Sidereal.Catalog.Id),
          SqlField("objectType", TargetView.Sidereal.Catalog.ObjectType),
        )
      ),
      ObjectMapping(
        tpe = NonsiderealType,
        fieldMappings = List(
          SqlField("synthetic_id", TargetView.Nonsidereal.SyntheticId, key = true, hidden = true),
          SqlField("des", TargetView.Nonsidereal.Des),
          SqlField("keyType", TargetView.Nonsidereal.KeyType),
          SqlField("key", TargetView.Nonsidereal.Key),
        )
      ),
      ObjectMapping(
        tpe = MutationType,
        fieldMappings = List(
          SqlRoot("createTarget", mutation = createTarget),
        )
      ),
      ObjectMapping(
        tpe = QueryType,
        fieldMappings = List(
          // TODO
        )
      ),
    )

    val elaborator = Map[TypeRef, PartialFunction[Select, Result[Query]]](
      MutationType -> {
        case Select("createTarget", List(
          ProgramIdBinding("programId", rPid),
          CreateTargetInput.Binding("input", rInput),
        ), child) =>
          (rPid, rInput).parMapN { (pid, input) =>
            Environment(
              Env("pid" -> pid, "input" -> input),
              Select("createTarget", Nil, child)
            )
          }
      }
    )

    Snippet(typeMappings, elaborator)

  }

}
