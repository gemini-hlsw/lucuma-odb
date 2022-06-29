package lucuma.odb.graphql
package snippet

import cats.effect.MonadCancelThrow
import cats.effect.kernel.Resource
import cats.syntax.all._
import edu.gemini.grackle.Cursor
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

object TargetSnippet {
  import TargetService.CreateTargetResponse._

  val schema = unsafeLoadSchema(this)

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

    val servicePool: Resource[F, TargetService[F]] =
      sessionPool.map(TargetService.fromSession(_, user))

    object TargetView extends TableDef("v_target") {
      val Id            = col("c_program_id", program_id)
      val TargetId      = col("c_target_id", target_id)
      val Name          = col("c_name", text_nonempty)
      val Existence     = col("c_existence", existence)
      val SourceProfile = col("c_source_profile", jsonb)
      object Sidereal {
        val SyntheticId    = col("c_sidereal_id", target_id.embedded)   // synthetic; non-null only if type = 'sidereal'
        val Ra             = col("c_sid_ra", right_ascension.embedded)  // logically never null if id is non-null
        val Dec            = col("c_sid_dec", declination.embedded)     // logically never null if id is non-null
        val Epoch          = col("c_sid_epoch", epoch.embedded)         // logically never null if id is non-null
        object RadialVelocity {
          val SyntheticId = col("c_sid_rv_id", target_id.embedded)   // synthetic
          val Value       = col("c_sid_rv", numeric.embedded)
        }
        val Parallax       = col("c_sid_parallax", angle_µas.opt)
        object Catalog {
          val SyntheticId = col("c_sid_catalog_info_id", target_id.embedded)   // synthetic; non-null only if c_sid_catalog_name is defined
          val Name        = col("c_sid_catalog_name", catalog_name.embedded)   // logically never null if id is non-null
          val Id          = col("c_sid_catalog_id", varchar.embedded)          // logically never null if id is non-null
          val ObjectType  = col("c_sid_catalog_object_type", varchar.embedded) // logically never null if id is non-null
        }
        object ProperMotion {
          val SyntheticId = col("c_sid_pm_id", target_id.embedded)            // synthetic; non-null only if c_sid_pm_ra is defined
          val Ra  = col("c_sid_pm_ra", angle_µas.embedded)                    // logically never null if id is non-null
          val Dec = col("c_sid_pm_dec", angle_µas.embedded)                   // logically never null if id is non-null
        }
      }
      object Nonsidereal {
        val SyntheticId    = col("c_nonsidereal_id", target_id.embedded)         // synthetic; non-null only if type = 'sidereal'
        val Des     = col("c_nsid_des", varchar.embedded)
        val KeyType = col("c_nsid_key_type", varchar.embedded) // todo: ephemeris_key_type
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

    // TODO: ergonomics

    def fromDeclination[A](f: Declination => A): Cursor => Result[A] = c =>
      c.field("microarcseconds", None).flatMap(_.as[Declination]).map(f)

    // this is required by the leaf mapping
    implicit val EncoderEpoch: Encoder[Epoch] =
      e => Json.fromString(Epoch.fromString.reverseGet(e))

    object UnderlyingField {
      def apply[A](underlyingField: String) = new Partial[A](underlyingField)
      class Partial[A](underlyingField: String) {
        def as[B: Encoder](field: String, f: A => B)(implicit ev: ClassTag[A]): CursorField[B] =
          CursorField(field, c => c.field(underlyingField, None).flatMap(_.as[A].map(f)), List(underlyingField))
      }
    }

    val typeMappings = List(
      LeafMapping[Target.Id](TargetIdType),
      LeafMapping[Epoch](EpochStringType),
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
          SqlField("ra", TargetView.Sidereal.Ra, hidden = true),
          UnderlyingField[RightAscension]("ra").as("hms", RightAscension.fromStringHMS.reverseGet),
          UnderlyingField[RightAscension]("ra").as("hours", c => BigDecimal(c.toHourAngle.toDoubleHours)),
          UnderlyingField[RightAscension]("ra").as("degrees", c => BigDecimal(c.toAngle.toDoubleDegrees)),
          UnderlyingField[RightAscension]("ra").as("microarcseconds", _.toAngle.toMicroarcseconds),
        ),
      ),
      ObjectMapping(
        tpe = DeclinationType,
        fieldMappings = List(
          SqlField("synthetic_id", TargetView.Sidereal.SyntheticId, key = true, hidden = true),
          SqlField("dec", TargetView.Sidereal.Dec, hidden = true),
          UnderlyingField[Declination]("dec").as("dms", Declination.fromStringSignedDMS.reverseGet),
          UnderlyingField[Declination]("dec").as("degrees", c => BigDecimal(c.toAngle.toDoubleDegrees)),
          UnderlyingField[Declination]("dec").as("microarcseconds", _.toAngle.toMicroarcseconds),
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
          SqlField("microarcsecondsPerYear", TargetView.Sidereal.ProperMotion.Ra),
          // TODO: kilometersPerSecond
        )
      ),
      ObjectMapping(
        tpe = ProperMotionDeclinationType,
        fieldMappings = List(
          SqlField("synthetic_id", TargetView.Sidereal.ProperMotion.SyntheticId, key = true, hidden = true),
          SqlField("microarcsecondsPerYear", TargetView.Sidereal.ProperMotion.Dec),
          // TODO: milliarcsecondsPerYear
        )
      ),
      ObjectMapping(
        tpe = RadialVelocityType,
        fieldMappings = List(
          SqlField("synthetic_id", TargetView.Sidereal.RadialVelocity.SyntheticId, key = true, hidden = true),
          SqlField("value", TargetView.Sidereal.RadialVelocity.Value, hidden = true),
          // CursorField[Long]("centimetersPerSecond", ???),
          // CursorField[BigDecimal]("metersPerSecond", ???),
          // CursorField[BigDecimal]("kilometersPerSecond", ???),
        )
      ),
      ObjectMapping(
        tpe = ParallaxType,
        fieldMappings = List(
          // TODO
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

    Snippet(schema, typeMappings, elaborator)

  }

}
