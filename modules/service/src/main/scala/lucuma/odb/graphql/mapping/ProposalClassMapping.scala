// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.Cursor
import grackle.Predicate
import grackle.Result
import grackle.Type
import grackle.skunk.SkunkMapping
import lucuma.odb.data.Tag
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.ProposalTable

trait ProposalClassMapping[F[_]] extends ProposalTable[F] with Predicates[F] {

  lazy val ProposalClassMappings =
    List(

      SqlInterfaceMapping(ProposalClassType, ProposalClassDiscriminator)(
        SqlField("synthetic-id", ProposalTable.ProgramId, key = true, hidden = true),
        SqlField("discriminator", ProposalTable.Clazz, discriminator = true, hidden = true),
        SqlField("minPercentTime", ProposalTable.MinPercent),
      ),

      // These have no extra fields.
      ObjectMapping(ClassicalType)(),
      ObjectMapping(DemoScienceType)(),
      ObjectMapping(DirectorsTimeType)(),
      ObjectMapping(ExchangeType)(),
      ObjectMapping(FastTurnaroundType)(),
      ObjectMapping(PoorWeatherType)(),
      ObjectMapping(QueueType)(),
      ObjectMapping(SystemVerificationType)(),

      // These do.
      ObjectMapping(LargeProgramType)(
        SqlField("minPercentTotalTime", ProposalTable.MinPercentTotal),
        SqlObject("totalTime"),
      ),
      ObjectMapping(IntensiveType)(
        SqlField("minPercentTotalTime", ProposalTable.MinPercentTotal),
        SqlObject("totalTime"),
      ),

    )

  val ProposalClassDiscriminator: SqlDiscriminator =
    new SqlDiscriminator {

      def discriminate(cursor: Cursor): Result[Type] =
        cursor.fieldAs[Tag]("discriminator").flatMap {
          case Tag("classical")           => Result(ClassicalType)
          case Tag("demo_science")        => Result(DemoScienceType)
          case Tag("directors_time")      => Result(DirectorsTimeType)
          case Tag("exchange")            => Result(ExchangeType)
          case Tag("fast_turnaround")     => Result(FastTurnaroundType)
          case Tag("intensive")           => Result(IntensiveType)
          case Tag("large_program")       => Result(LargeProgramType)
          case Tag("poor_weather")        => Result(PoorWeatherType)
          case Tag("queue")               => Result(QueueType)
          case Tag("system_verification") => Result(SystemVerificationType)
          case other                      => Result.internalError("ProposalClassMapping: cannot determine type for $other")
        }

      def narrowPredicate(tpe: Type): Option[Predicate] = {
        def pred(tagName: String) = Some(Predicates.proposalClass.discriminator.eql(Tag(tagName)))
        tpe match
          case ClassicalType          => pred("classical")
          case DemoScienceType        => pred("demo_science")
          case DirectorsTimeType      => pred("directors_time")
          case ExchangeType           => pred("exchange")
          case FastTurnaroundType     => pred("fast_turnaround")
          case IntensiveType          => pred("intensive")
          case LargeProgramType       => pred("large_program")
          case PoorWeatherType        => pred("poor_weather")
          case QueueType              => pred("queue")
          case SystemVerificationType => pred("system_verification")
          case _                      => None
        }

    }

}

