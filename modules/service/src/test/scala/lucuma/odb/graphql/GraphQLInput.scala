// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.Contravariant
import cats.syntax.all.*
import io.circe.Encoder
import io.circe.syntax.*
import lucuma.core.data.PerSite
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Offset
import lucuma.core.math.RightAscension
import lucuma.core.model.Ephemeris
import lucuma.core.util.Timestamp
import lucuma.odb.json.offset

/** A typeclass for GraphQL input values, based on the schema. */
trait GraphQLInput[A]:
  def encode(a: A): String

extension [A](a: A)(using gi: GraphQLInput[A]) def asGraphQL: String =
  gi.encode(a)

object GraphQLInput:
  
  def apply[A](using ev: GraphQLInput[A]): ev.type = ev

  def jsonInstance[A: Encoder]: GraphQLInput[A] = a => a.asJson.noSpaces

  given Contravariant[GraphQLInput] with
    def contramap[A, B](fa: GraphQLInput[A])(f: B => A): GraphQLInput[B] = b => fa.encode(f(b))


  // Helpers
  def obj(kv: (String, String)*): String =
    kv.map((k, v) => s"$k: $v").mkString("{", " ", "}")

  // Some base instances are derived from their JSON instances
  given GraphQLInput[String] = jsonInstance
  given GraphQLInput[Long] = a => if a <= Int.MaxValue then a.toString() else s"\"$a\""
  given GraphQLInput[Timestamp] = jsonInstance


  // More base instances
  given GraphQLInput[Angle] = a => obj("microarcseconds" -> a.toMicroarcseconds.asGraphQL)
  given GraphQLInput[RightAscension] = a => obj("microseconds" -> a.toHourAngle.toMicroseconds.asGraphQL)
  given GraphQLInput[Declination] = GraphQLInput[Angle].contramap(_.toAngle)
  given GraphQLInput[Coordinates] = a => obj("ra" -> a.ra.asGraphQL, "dec" -> a.dec.asGraphQL)  
  given [A]: GraphQLInput[Offset.Component[A]] = a => obj("microarcseconds" -> offset.µas.get(a).asGraphQL)
  given GraphQLInput[Offset] = a => obj("p" -> a.p.asGraphQL, "q" -> a.q.asGraphQL)

  given GraphQLInput[Ephemeris.UserSupplied.Element] = a => 
    obj(
      "when" -> Timestamp.fromInstantTruncatedAndBounded(a.when).asGraphQL,
      "coordinates" -> a.coordinates.asGraphQL,
      "velocity" -> a.velocity.asGraphQL,
    )

  // Inductive Instances
  given [A: GraphQLInput]: GraphQLInput[List[A]] = as => as.map(_.asGraphQL).mkString("[", " ", "]")
  given [A: GraphQLInput]: GraphQLInput[PerSite[A]] = ps => obj("gn" -> ps.gn.asGraphQL, "gs" -> ps.gs.asGraphQL)
  
  