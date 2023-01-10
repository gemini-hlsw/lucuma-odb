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
          case Tag("classical_type")           => Result(ClassicalType)
          case Tag("demo_science_type")        => Result(DemoScienceType)
          case Tag("directors_time_type")      => Result(DirectorsTimeType)
          case Tag("exchange_type")            => Result(ExchangeType)
          case Tag("fast_turnaround_type")     => Result(FastTurnaroundType)
          case Tag("intensive_type")           => Result(IntensiveType)
          case Tag("large_program_type")       => Result(LargeProgramType)
          case Tag("poor_weather_type")        => Result(PoorWeatherType)
          case Tag("queue_type")               => Result(QueueType)
          case Tag("system_verification_type") => Result(SystemVerificationType)
          case other                           => Result.failure("ProposalClassMapping: cannot determine type for $other")
        }

      def narrowPredicate(tpe: Type): Option[Predicate] = {
        def pred(tagName: String) = Some(Predicates.proposalClass.discriminator.eql(Tag(tagName)))
        tpe match
          case ClassicalType          => pred("classical_type")
          case DemoScienceType        => pred("demo_science_type")
          case DirectorsTimeType      => pred("directors_time_type")
          case ExchangeType           => pred("exchange_type")
          case FastTurnaroundType     => pred("fast_turnaround_type")
          case IntensiveType          => pred("intensive_type")
          case LargeProgramType       => pred("large_program_type")
          case PoorWeatherType        => pred("poor_weather_type")
          case QueueType              => pred("queue_type")
          case SystemVerificationType => pred("system_verification_type")
          case _                      => None
        }

    }

}


