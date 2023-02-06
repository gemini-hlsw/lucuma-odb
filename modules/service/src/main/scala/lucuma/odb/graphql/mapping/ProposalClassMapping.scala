// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import edu.gemini.grackle.Cursor
import edu.gemini.grackle.Path
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Result
import edu.gemini.grackle.Type
import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.data.Tag
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.PartnerSplitTable
import lucuma.odb.graphql.table.ProposalTable
import lucuma.odb.graphql.util.MappingExtras

import table.TargetView
import table.ProgramTable

trait ProposalClassMapping[F[_]] extends ProposalTable[F] with Predicates[F] {

  lazy val ProposalClassMappings =
    List(

      SqlInterfaceMapping(
        tpe = ProposalClassType,
        fieldMappings = List(
          SqlField("synthetic-id", ProposalTable.ProgramId, key = true, hidden = true),
          SqlField("discriminator", ProposalTable.Clazz, discriminator = true, hidden = true),
          SqlField("minPercentTime", ProposalTable.MinPercent),
        ),
        discriminator = ProposalClassDiscriminator,
      ),

      // These have no extra fields.
      ObjectMapping(ClassicalType, Nil),
      ObjectMapping(DemoScienceType, Nil),
      ObjectMapping(DirectorsTimeType, Nil),
      ObjectMapping(ExchangeType, Nil),
      ObjectMapping(FastTurnaroundType, Nil),
      ObjectMapping(PoorWeatherType, Nil),
      ObjectMapping(QueueType, Nil),
      ObjectMapping(SystemVerificationType, Nil),

      // These do.
      ObjectMapping(
        tpe = LargeProgramType,
        fieldMappings = List(
          SqlField("minPercentTotalTime", ProposalTable.MinPercentTotal),
          SqlObject("totalTime"),
        )
      ),
      ObjectMapping(
        tpe = IntensiveType,
        fieldMappings = List(
          SqlField("minPercentTotalTime", ProposalTable.MinPercentTotal),
          SqlObject("totalTime"),
        )
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
          case other                      => Result.failure("ProposalClassMapping: cannot determine type for $other")
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


