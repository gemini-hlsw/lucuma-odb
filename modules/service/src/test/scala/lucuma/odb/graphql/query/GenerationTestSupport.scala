// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.ExecutionConfig
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.Step
import lucuma.odb.data.OdbError
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.logic.Generator
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.Services

import scala.collection.immutable.ListMap

trait GenerationTestSupport extends OdbSuite:


  /**
   * Generates the sequence for the given observation.
   *
   * @param limit the future limit, which must be in the range [0, 100]
   * @param when the timestamp to pass the generator. in other words generate as
   *             if asked at this time
   */
  def generateAs(
    user:  User,
    oid:   Observation.Id,
    limit: Option[Int] = None,  // [0, 100]
  ): IO[Either[OdbError, InstrumentExecutionConfig]] =
    withSession: session =>
      servicesFor(user).map(_(session)).use: s =>
        given Services[IO] = s

        for
          future <- limit.traverse(lim => IO.fromOption(Generator.FutureLimit.from(lim).toOption)(new IllegalArgumentException("Specify a future limit from 0 to 100")))
          enums  <- Enums.load(session)
          tec    <- TimeEstimateCalculatorImplementation.fromSession(session, enums)
          gen     = Generator.instantiate[IO](CommitHash.Zero, tec)
          res    <- Services.asSuperUser(
                      gen.generate(oid, future.getOrElse(Generator.FutureLimit.Default))
                    )
        yield res

  /**
   * Generates the sequence but fails if it produces an error.
   *
   * @param limit the future limit, which must be in the range [0, 100]
   * @param when the timestamp to pass the generator. in other words generate as
   *             if asked at this time
   */
  def generateOrFailAs(
    user:  User,
    oid:   Observation.Id,
    limit: Option[Int] = None
  ): IO[InstrumentExecutionConfig] =
    generateAs(user, oid, limit).flatMap: res =>
      IO.fromEither(res.leftMap(e => new RuntimeException(s"Failed to generate the sequence: ${e.message}")))

  def generateFlamingos2OrFail(
    user: User,
    oid:  Observation.Id
  ): IO[InstrumentExecutionConfig.Flamingos2] =
    generateOrFailAs(user, oid).flatMap:
      case f2 @ InstrumentExecutionConfig.Flamingos2(_) => f2.pure
      case i                                            => IO.raiseError(new RuntimeException(s"Expected GMOS North, but got ${i.instrument.longName}"))

  def generateGmosNorthOrFail(
    user: User,
    oid:  Observation.Id
  ): IO[InstrumentExecutionConfig.GmosNorth] =
    generateOrFailAs(user, oid).flatMap:
      case gn @ InstrumentExecutionConfig.GmosNorth(_) => gn.pure
      case i                                           => IO.raiseError(new RuntimeException(s"Expected GMOS North, but got ${i.instrument.longName}"))

  def generateGmosSouthOrFail(
    user: User,
    oid:  Observation.Id
  ): IO[InstrumentExecutionConfig.GmosSouth] =
    generateOrFailAs(user, oid).flatMap:
      case gs @ InstrumentExecutionConfig.GmosSouth(_) => gs.pure
      case i                                           => IO.raiseError(new RuntimeException(s"Expected GMOS North, but got ${i.instrument.longName}"))

  private def executionConfig(
    user: User,
    oid:  Observation.Id
  ): IO[ExecutionConfig[?, ?]] =
    generateOrFailAs(user, oid).map:
      case InstrumentExecutionConfig.Flamingos2(e) => e
      case InstrumentExecutionConfig.GmosNorth(e)  => e
      case InstrumentExecutionConfig.GmosSouth(e)  => e
      case InstrumentExecutionConfig.Igrins2(e)    => e

  def acquisitionSequenceIds(
    user: User,
    oid:  Observation.Id
  ): IO[ListMap[Atom.Id, List[Step.Id]]] =
    executionConfig(user, oid).map: ec =>
      ListMap.from:
        ec
          .acquisition
          .toList
          .flatMap: s =>
            (s.nextAtom :: s.possibleFuture).map: a =>
              a.id -> a.steps.toList.map(_.id)

  def scienceSequenceIds(
    user: User,
    oid:  Observation.Id
  ): IO[ListMap[Atom.Id, List[Step.Id]]] =
    executionConfig(user, oid).map: ec =>
      ListMap.from:
        ec
          .science
          .toList
          .flatMap: s =>
            (s.nextAtom :: s.possibleFuture).map: a =>
              a.id -> a.steps.toList.map(_.id)

  def scienceAtomIds(
    user: User,
    oid:  Observation.Id
  ): IO[List[Atom.Id]] =
    scienceSequenceIds(user, oid).map(_.keys.toList)

  def firstScienceAtomStepIds(
    user: User,
    oid:  Observation.Id
  ): IO[List[Step.Id]] =
    scienceSequenceIds(user, oid).map(_.head._2)

  def firstScienceAtomId(
    user: User,
    oid:  Observation.Id
  ): IO[Atom.Id] =
    scienceAtomIds(user, oid).map(_.head)

  def firstScienceStepId(
    user: User,
    oid:  Observation.Id
  ): IO[Step.Id] =
    firstScienceAtomStepIds(user, oid).map(_.head)