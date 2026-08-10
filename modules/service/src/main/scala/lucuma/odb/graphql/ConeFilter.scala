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
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.model.ConfigurationRequest

/** Stands in for a `WhereConfigurationRequest.targetCoordinates` cone until the matching
 *  ids are known. Produced by the WHERE binding during elaboration and replaced by
 *  `ConeFilter.resolve` with `In(idPath, candidateIds)` before the query is executed.
 *
 *  Should not be evaluated: reaching `apply` means a cone escaped resolution, which is a
 *  bug rather than a user error, so it reports an internal error instead of a silent
 *  `true`. Escaping is possible -- resolution only sees predicates reachable in the
 *  compiled query tree, so a cone held anywhere else (in an `Env`, or under the `Effect`
 *  and `Component` nodes the walk does not enter) would survive to here. The WHERE
 *  binding's `allowCone` flag is what keeps that from happening.
 */
case class ConePredicate(idPath: Path, center: Coordinates, distance: Angle) extends Predicate:
  def apply(c: Cursor): Result[Boolean] = Result.internalError("Unresolved targetCoordinates cone.")
  def children: List[Term[?]]           = Nil

/** Resolves `targetCoordinates` cone filters, which grackle cannot evaluate on its own:
 *  finding the configuration requests inside a cone is an `F` effect, and the elaborator
 *  that turns a WHERE input into a predicate is a pure `StateT[Result, ElabState, *]`.
 *
 *  {{{
 *  parse ──▶ compile ──▶ Query ──▶ resolve ──▶ Query ──▶ execute ──▶ Json
 *               │                     │
 *      cone ⟶ ConePredicate    ConePredicate ⟶ id IN (…)
 *  }}}
 *
 *  Splitting it this way lets grackle do the parsing first: variables are substituted and
 *  the input is validated by the ordinary `WhereCone` binding, so the cone arrives here as
 *  parsed `Coordinates` and `Angle` whether it was written inline or passed as a variable.
 *  Resolution then happens in `F`, where the candidate lookup can run, and substitutes the
 *  ids *in place*, so a cone nested under `AND` / `OR` / `NOT` keeps its position and
 *  meaning. What grackle finally executes is an ordinary WHERE that pushes down to a
 *  single SQL statement.
 *
 *  Driven by `GraphQLRoutes`, which resolves each compiled operation before running it.
 *  That reaches every cone the `configurationRequests` query can produce, because its
 *  WHERE becomes a `Filter` in the compiled query. It would *not* reach one in the
 *  `updateConfigurationRequests` mutation, which keeps its bound input in an `Env` and
 *  builds the `Filter` at execution time -- so that mutation's WHERE binding refuses
 *  `targetCoordinates` outright (see `WhereConfigurationRequest.binding`).
 */
object ConeFilter:

  /** Replaces each `ConePredicate` in `query` with the ids `compute` finds for it.
   *  Queries without cones -- nearly all of them -- are returned untouched.
   */
  def resolve[F[_]: Monad](
    query: Query
  )(compute: (Coordinates, Angle) => F[Result[List[ConfigurationRequest.Id]]]): F[Result[Query]] =
    collect(query).distinct match
      case Nil   => Result.success(query).pure[F]
      case cones =>
        cones.traverse(c => compute(c.center, c.distance)).map: rs =>
          rs.sequence.map(ids => substitute(query, cones.zip(ids).toMap))

  // --- collection ---

  private def collect(q: Query): List[ConePredicate] =
    q match
      case Filter(pred, child)       => collectPred(pred) ++ collect(child)
      case Group(qs)                 => qs.flatMap(collect)
      case s: Select                 => collect(s.child)
      case Unique(child)             => collect(child)
      case Environment(_, child)     => collect(child)
      case Narrow(_, child)          => collect(child)
      case Limit(_, child)           => collect(child)
      case Offset(_, child)          => collect(child)
      case OrderBy(_, child)         => collect(child)
      case Count(child)              => collect(child)
      case TransformCursor(_, child) => collect(child)
      case Introspect(_, child)      => collect(child)
      case _                         => Nil

  private def collectPred(p: Predicate): List[ConePredicate] =
    p.fold(List.empty[ConePredicate]): (acc, t) =>
      t match
        case c: ConePredicate => c :: acc
        case _                => acc

  // --- substitution ---

  private type Ids = Map[ConePredicate, List[ConfigurationRequest.Id]]

  private def substitute(q: Query, ids: Ids): Query =
    q match
      case Filter(pred, child)       => Filter(substitutePred(pred, ids), substitute(child, ids))
      case Group(qs)                 => Group(qs.map(substitute(_, ids)))
      case s: Select                 => s.copy(child = substitute(s.child, ids))
      case Unique(child)             => Unique(substitute(child, ids))
      case e @ Environment(_, child) => e.copy(child = substitute(child, ids))
      case Narrow(tpe, child)        => Narrow(tpe, substitute(child, ids))
      case Limit(n, child)           => Limit(n, substitute(child, ids))
      case Offset(n, child)          => Offset(n, substitute(child, ids))
      case OrderBy(sels, child)      => OrderBy(sels, substitute(child, ids))
      case Count(child)              => Count(substitute(child, ids))
      case t @ TransformCursor(_, c) => t.copy(child = substitute(c, ids))
      case Introspect(schema, child) => Introspect(schema, substitute(child, ids))
      case other                     => other

  private def substitutePred(p: Predicate, ids: Ids): Predicate =
    p match
      case c: ConePredicate => ids.get(c).fold(c: Predicate)(In(c.idPath, _))
      case And(x, y)        => And(substitutePred(x, ids), substitutePred(y, ids))
      case Or(x, y)         => Or(substitutePred(x, ids), substitutePred(y, ids))
      case Not(x)           => Not(substitutePred(x, ids))
      case other            => other
