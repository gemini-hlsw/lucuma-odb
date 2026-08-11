// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.Monad
import cats.syntax.all.*
import grackle.Cursor
import grackle.Path
import grackle.Predicate
import grackle.Predicate.And
import grackle.Predicate.In
import grackle.Predicate.Not
import grackle.Predicate.Or
import grackle.Query
import grackle.Query.Count
import grackle.Query.Environment
import grackle.Query.Filter
import grackle.Query.Group
import grackle.Query.Introspect
import grackle.Query.Limit
import grackle.Query.Narrow
import grackle.Query.Offset
import grackle.Query.OrderBy
import grackle.Query.Select
import grackle.Query.TransformCursor
import grackle.Query.Unique
import grackle.Result
import grackle.Term
import lucuma.core.model.ConfigurationRequest
import lucuma.odb.data.Cone
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.asFailure

/** Resolves `targetCoordinates` cone filters, which grackle cannot evaluate on its own:
 *  finding the configuration requests inside a cone is an `F` effect, and the elaborator
 *  that turns a WHERE input into a predicate is a pure `StateT[Result, ElabState, *]`.
 *
 *  {{{
 *  request with a targetCoordinates cone
 *    │ parse, substitute variables
 *    ▼
 *  WhereConfigurationRequest binding ──(mutation: allowCone = false)──▶ InvalidArgument
 *    │ query: allowCone = true
 *    ▼
 *  compiled Query with a ConePredicate placeholder in its WHERE tree
 *    │
 *    ▼
 *  ConeFilter.resolve, in F ──(no cones)──▶ query untouched
 *    │ per distinct cone
 *    ▼
 *  ConfigurationService.coneCandidates:                                   [SQL 1]
 *  box prefilter (indexed), exact great-circle trim, visibility, limit max+1
 *    │ ids                  ──(over the cap)──▶ fail: narrow the cone
 *    ▼
 *  ConePredicate ⟶ id IN (ids), in place; an empty list compiles to false
 *    │
 *    ▼
 *  grackle executes: the whole WHERE pushable, a single SQL statement     [SQL 2]
 *  }}}
 *
 *  Splitting it this way lets grackle do the parsing first: variables are substituted and
 *  the input is validated by the ordinary `WhereCone` binding, so the cone arrives here as
 *  a parsed `Cone` whether it was written inline or passed as a variable. Resolution then
 *  happens in `F`, where the candidate lookup (SQL 1) can run, and substitutes the ids
 *  *in place*, so a cone nested under `AND` / `OR` / `NOT` keeps its position and meaning.
 *  What grackle finally executes is an ordinary WHERE that pushes down to a single SQL
 *  statement (SQL 2).
 *
 *  Driven by `GraphQLRoutes`, which resolves each compiled operation before running it.
 *  That reaches every cone the `configurationRequests` query can produce, because its
 *  WHERE becomes a `Filter` in the compiled query. It would *not* reach one in the
 *  `updateConfigurationRequests` mutation, which keeps its bound input in an `Env` and
 *  builds the `Filter` at execution time -- so that mutation's WHERE binding refuses
 *  `targetCoordinates` outright (see `WhereConfigurationRequest.binding`).
 */
object ConeFilter:
  /** Placeholder for a `WhereConfigurationRequest.targetCoordinates` cone: created by the
   *  WHERE binding, swapped for `In(idPath, candidateIds)` by `ConeFilter.resolve` before
   *  execution.
   *
   *  Evaluating it means the swap was missed: `resolve` only walks the compiled query
   *  tree, so a cone stored anywhere else (an `Env`, an `Effect`/`Component` child) would
   *  escape it. `apply` fails loudly rather than silently matching, and the binding's
   *  `allowCone` flag keeps cones out of those unreachable spots.
   */
  case class ConePredicate(idPath: Path, cone: Cone) extends Predicate:
    def apply(c: Cursor): Result[Boolean] = Result.internalError("Unresolved targetCoordinates cone.")
    def children: List[Term[?]]           = Nil

  /** Cap on distinct cones per operation. Each one costs a candidate scan and can inject
   *  up to `ConfigurationService.MaxConeCandidates` ids into the final statement, so an
   *  unbounded `OR` list would be a cheap amplification lever.
   */
  val MaxConesPerOperation: Int = 5

  /** Replaces each `ConePredicate` in `query` with the ids `compute` finds for its cone.
   *  Queries without cones -- nearly all of them -- are returned untouched.
   */
  def resolve[F[_]: Monad](
    query: Query
  )(compute: Cone => F[Result[List[ConfigurationRequest.Id]]]): F[Result[Query]] =
    collect(query).distinct match
      case Nil   => Result.success(query).pure[F]
      case cones if cones.sizeIs > MaxConesPerOperation =>
        OdbError.InvalidArgument(s"A query may use at most $MaxConesPerOperation distinct targetCoordinates filters.".some)
          .asFailure.pure[F]
      case cones =>
        cones.traverse(c => compute(c.cone)).map: rs =>
          rs.sequence.map(ids => substitute(query, cones.zip(ids).toMap))

  /** The one walk over the query tree: rewrites the predicate of every reachable
   *  `Filter` node. Collection reuses it with a predicate-preserving `f` that records
   *  what it sees, discarding the rebuilt query.
   */
  private def mapFilterPredicates(q: Query)(f: Predicate => Predicate): Query =
    q match
      case Filter(pred, child)       => Filter(f(pred), mapFilterPredicates(child)(f))
      case Group(qs)                 => Group(qs.map(mapFilterPredicates(_)(f)))
      case s: Select                 => s.copy(child = mapFilterPredicates(s.child)(f))
      case Unique(child)             => Unique(mapFilterPredicates(child)(f))
      case e: Environment            => e.copy(child = mapFilterPredicates(e.child)(f))
      case Narrow(tpe, child)        => Narrow(tpe, mapFilterPredicates(child)(f))
      case Limit(n, child)           => Limit(n, mapFilterPredicates(child)(f))
      case Offset(n, child)          => Offset(n, mapFilterPredicates(child)(f))
      case OrderBy(sels, child)      => OrderBy(sels, mapFilterPredicates(child)(f))
      case Count(child)              => Count(mapFilterPredicates(child)(f))
      case t: TransformCursor        => t.copy(child = mapFilterPredicates(t.child)(f))
      case Introspect(schema, child) => Introspect(schema, mapFilterPredicates(child)(f))
      case other                     => other

  private def collect(q: Query): List[ConePredicate] =
    val found = List.newBuilder[ConePredicate]
    mapFilterPredicates(q): p =>
      found ++= p.fold(List.empty[ConePredicate]): (acc, t) =>
        t match
          case c: ConePredicate => c :: acc
          case _                => acc
      p
    found.result()

  private type Ids = Map[ConePredicate, List[ConfigurationRequest.Id]]

  private def substitute(q: Query, ids: Ids): Query =
    mapFilterPredicates(q)(substitutePred(_, ids))

  private def substitutePred(p: Predicate, ids: Ids): Predicate =
    p match
      case c: ConePredicate => ids.get(c).fold(c: Predicate)(In(c.idPath, _))
      case And(x, y)        => And(substitutePred(x, ids), substitutePred(y, ids))
      case Or(x, y)         => Or(substitutePred(x, ids), substitutePred(y, ids))
      case Not(x)           => Not(substitutePred(x, ids))
      case other            => other
