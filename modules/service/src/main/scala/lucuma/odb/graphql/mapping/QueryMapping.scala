// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.effect.kernel.Par
import cats.syntax.all._
import edu.gemini.grackle.Cursor
import edu.gemini.grackle.Cursor.Env
import edu.gemini.grackle.Cursor.ListTransformCursor
import edu.gemini.grackle.Path
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Result
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.skunk.SkunkMapping
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.ProgramType
import lucuma.core.model.User
import lucuma.odb.data.Tag
import lucuma.odb.graphql.binding._
import lucuma.odb.graphql.input.WhereObservation
import lucuma.odb.graphql.input.WhereProgram
import lucuma.odb.graphql.input.WhereTargetInput
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.instances.given

import scala.reflect.ClassTag

trait QueryMapping[F[_]] extends Predicates[F] {
  this: SkunkMapping[F]
   with TargetMapping[F]
   with FilterTypeMetaMapping[F]
   with PartnerMetaMapping[F]
   with ProgramMapping[F]
   with ObservationMapping[F] =>

  lazy val QueryType = schema.ref("Query")

  lazy val QueryMapping =
    ObjectMapping(
      tpe = QueryType,
      fieldMappings = List(
        SqlObject("filterTypeMeta"),
        SqlObject("observation"),
        SqlObject("observations"),
        SqlObject("partnerMeta"),
        SqlObject("program"),
        SqlObject("programs"),
        SqlObject("target"),
        SqlObject("targets"),
      )
    )

  lazy val QueryElaborator: Map[TypeRef, PartialFunction[Select, Result[Query]]] =
    List(
      FilterTypeMeta,
      Observation,
      Observations,
      PartnerMeta,
      Program,
      Programs,
      Target,
      Targets,
    ).foldMap(pf => Map(QueryType -> pf))

  def user: User

  // Elaborators below

  private lazy val FilterTypeMeta: PartialFunction[Select, Result[Query]] =
    case Select("filterTypeMeta", Nil, child) =>
      Result(Select("filterTypeMeta", Nil,
        OrderBy(OrderSelections(List(OrderSelection[Tag](FilterTypeMetaType / "tag"))), child)
      ))

  private lazy val Observation: PartialFunction[Select, Result[Query]] =
    case Select("observation", List(
      ObservationIdBinding("observationId", rOid)
    ), child) =>
      rOid.map { oid =>
        Select("observation", Nil,
          Unique(
            Filter(
              And(
                Predicates.observation.id.eql(oid),
                Predicates.observation.program.isVisibleTo(user)
              ),
              child
            )
          )
        )
      }

  private lazy val Observations: PartialFunction[Select, Result[Query]] = {
    val WhereObservationBinding = WhereObservation.binding(Path.from(ObservationType))
    {
      case Select("observations", List(
        WhereObservationBinding.Option("WHERE", rWHERE),
        ObservationIdBinding.Option("OFFSET", rOFFSET),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
        BooleanBinding("includeDeleted", rIncludeDeleted)
      ), child) =>
        (rWHERE, rOFFSET, rLIMIT, rIncludeDeleted).parMapN { (WHERE, OFFSET, LIMIT, includeDeleted) =>
          Select("observations", Nil,
            Limit(
              LIMIT.foldLeft(1000)(_ min _.value),  // TODO: we need a common place for the max limit
              Filter(
                and(List(
                  OFFSET.map(Predicates.observation.id.gtEql).getOrElse(True),
                  Predicates.observation.existence.includeDeleted(includeDeleted),
                  Predicates.observation.program.isVisibleTo(user),
                  WHERE.getOrElse(True)
                )),
                child
              )
            )
          )
        }
      }
    }

  private lazy val PartnerMeta: PartialFunction[Select, Result[Query]] =
    case Select("partnerMeta", Nil, child) =>
      Result(Select("partnerMeta", Nil,
        OrderBy(OrderSelections(List(OrderSelection[Tag](PartnerMetaType / "tag"))), child)
      ))

  private lazy val Program: PartialFunction[Select, Result[Query]] =
    case Select("program", List(
      ProgramIdBinding("programId", rPid),
    ), child) =>
      rPid.map { pid =>
        Select("program", Nil,
          Unique(
            Filter(
              And(
                Predicates.program.id.eql(pid),
                Predicates.program.isVisibleTo(user),
              ),
              child
            )
          )
        )
      }

  private lazy val Programs: PartialFunction[Select, Result[Query]] = {
    val WhereProgramBinding = WhereProgram.binding(Path.from(ProgramType))
    {
      case Select("programs", List(
        WhereProgramBinding.Option("WHERE", rWHERE),
        ProgramIdBinding.Option("OFFSET", rOFFSET),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
        BooleanBinding("includeDeleted", rIncludeDeleted)
      ), child) =>
        (rWHERE, rOFFSET, rLIMIT, rIncludeDeleted).parTupled.flatMap { (WHERE, OFFSET, LIMIT, includeDeleted) =>
          val limit = LIMIT.foldLeft(1000)(_ min _.value)
          selectResultTransform("programs", child, limit) { child =>           
            FilterOrderByOffsetLimit(
              pred = Some(
                and(List(
                  OFFSET.map(Predicates.program.id.gtEql).getOrElse(True),
                  Predicates.program.existence.includeDeleted(includeDeleted),
                  Predicates.program.isVisibleTo(user),
                  WHERE.getOrElse(True)
                ))
              ),
              oss = Some(List(
                OrderSelection[lucuma.core.model.Program.Id](ProgramType / "id")
              )),
              offset = None,
              limit = Some(limit + 1), // Select one extra row here.
              child = child
            )
          }
        }
      }
  }

  private lazy val Target: PartialFunction[Select, Result[Query]] =
    case Select("target", List(
      TargetIdBinding("targetId", rPid),
    ), child) =>
      rPid.map { pid =>
        Select("target", Nil,
          Unique(
            Filter(
              And(
                Predicates.target.id.eql(pid),
                Predicates.target.program.isVisibleTo(user),
              ),
              child
            )
          )
        )
      }

 
  def selectResultTransform(field: String, child: Query, limit: Int)(transformMatchesChild: Query => Query): Result[Query] = {

    // Transform the cursor to remove the last element from the list if the over-select
    // revealed that there are more rows available.
    def removeOverfetch(c: Cursor): Result[Cursor] =
      (c.listSize, c.asList(Seq)).mapN { (size, elems) =>
        if size <= limit then c
        else ListTransformCursor(c, size - 1, elems.init)
      }
 
    // Find the "matches" node under the main "targets" query and add all our filtering
    // and whatnot down in there.
    def transformMatches(q: Query): Result[Query] =
      Query.mapSomeFields(q) {
        case Select("matches", Nil, child) =>
          Result(
            Select(
              "matches", 
              Nil, 
              TransformCursor(
                removeOverfetch, 
                transformMatchesChild(child)                  
              )
            )
          )
      }

    // If we're selecting "matches" then continue by transforming the child query, otherwise
    // punt because there's really no point in doing such a selection.
    if Query.hasField(child, "matches") then 
      transformMatches(child).map { child =>
        Select(field, Nil,
          Environment(
            Env(
              SelectResultMapping.LimitKey -> limit,
              SelectResultMapping.AliasKey -> Query.fieldAlias(child, "matches"),
            ),
            child
          )
        )
      }
    else Result.failure("Field `matches` must be selected.") // meh

  }


  private lazy val Targets: PartialFunction[Select, Result[Query]] = {
    val WhereTargetInputBinding = WhereTargetInput.binding(Path.from(TargetType))
    {
      case Select("targets", List(
        WhereTargetInputBinding.Option("WHERE", rWHERE),
        TargetIdBinding.Option("OFFSET", rOFFSET),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
        BooleanBinding("includeDeleted", rIncludeDeleted)
      ), child) =>
        (rWHERE, rOFFSET, rLIMIT, rIncludeDeleted).parTupled.flatMap { (WHERE, OFFSET, LIMIT, includeDeleted) =>
          val limit = LIMIT.foldLeft(1000)(_ min _.value)
          selectResultTransform("targets", child, limit) { q =>
            FilterOrderByOffsetLimit(
              pred = Some(
                and(List(
                  OFFSET.map(Predicates.target.id.gtEql).getOrElse(True),
                  Predicates.target.existence.includeDeleted(includeDeleted),
                  Predicates.target.program.isVisibleTo(user),
                  WHERE.getOrElse(True)
                )
              )),
              oss = Some(List(
                OrderSelection[lucuma.core.model.Target.Id](TargetType / "id")
              )),
              offset = None,
              limit = Some(limit + 1), // Select one extra row here.
              child = q
            )          
          }
        }
      }        
  }

}