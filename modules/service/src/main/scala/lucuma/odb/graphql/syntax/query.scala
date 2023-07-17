// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.syntax

import cats.syntax.option.*
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query.*

import scala.annotation.tailrec

trait QueryOps {

  extension(q: Query) {

    def children: List[Query] =
      q match {
        case Select(_, _, child)       => List(child)
        case Group(queries)            => queries
        case Unique(child)             => List(child)
        case Filter(_, child)          => List(child)
        case Component(_, _, child)    => List(child)
        case Effect(_, child)          => List(child)
        case Introspect(_, child)      => List(child)
        case Environment(_, child)     => List(child)
        case Wrap(_, child)            => List(child)
        case Rename(_, child)          => List(child)
        case UntypedNarrow(_, child)   => List(child)
        case Narrow(_, child)          => List(child)
        case Skip(_, _, child)         => List(child)
        case Limit(_, child)           => List(child)
        case Offset(_, child)          => List(child)
        case OrderBy(_, child)         => List(child)
        case Count(_, child)           => List(child)
        case TransformCursor(_, child) => List(child)
        case Query.Skipped             => Nil
        case Query.Empty               => Nil
      }

    def find(f: Query => Boolean): Option[Query] = {

      @tailrec
      def go(qs: List[Query]): Option[Query] =
        qs match {
          case Nil    => none[Query]
          case h :: t => if (f(h)) h.some else go(h.children ::: t)
        }

      go(List(q))
    }

    def exists(p: Query => Boolean): Boolean =
      find(p).isDefined

    def forall(p: Query => Boolean): Boolean =
      !exists(q => !p(q))

  }

}

object query extends QueryOps

