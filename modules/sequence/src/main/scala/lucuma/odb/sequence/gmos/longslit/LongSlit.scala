// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos.longslit

import cats.Applicative
import cats.data.State
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import fs2.Pipe
import fs2.Stream
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.GcalLampType
import lucuma.core.enums.StepType
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.itc.IntegrationTime
import lucuma.odb.sequence.data.CalLocation
import lucuma.odb.sequence.data.Completion
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoExecutionConfig
import lucuma.odb.sequence.data.ProtoStep

/**
 * GMOS Long Slit generators.
 */
object LongSlit {

  case class ScienceTargetGenerator[F[_]: Applicative, S, D](
    pureGen:  PureLongSlit[S, D],
    expander: SmartGcalExpander[F, D]
  ) extends Generator[F, S, D] {

    // Creates a smart arc as a single step in an atom with a given description
    def smartArc(description: String, d: D): ProtoAtom[ProtoStep[D]] =
      ProtoAtom.one(description, ProtoStep.smartArc(d))

    // An arc step as a singleton atom at a particular location (before or after
    // the corresponding science step).
    case class Arc[D](atom: ProtoAtom[ProtoStep[D]], location: CalLocation)

    // Zips each atom from the pure generator with the corresponding `Arc`.
    // Most of these arcs will never be incorporated into the actual sequence,
    // and instead will be filted out.  Only the required arc steps will
    // actually make it into the generated sequence.  Note, the `Either`s in the
    // input and output are introduced by smart gcal expansion.  If there was no
    // matching smart flat in the input, it will be a Left.  If there is no
    // matching smart arc in the output (or if the input was a Left), the
    // combined result is a Left.
    val zipWithArc: Pipe[
      F,
      Either[String, ProtoAtom[ProtoStep[D]]],
      Either[String, (ProtoAtom[ProtoStep[D]], Arc[D])]
    ] =
      _.evalMapAccumulate(Map.empty[D, ProtoAtom[ProtoStep[D]]]) { (cache, eAtom) =>
        eAtom.flatTraverse { atom =>
          val arcTitle = atom.description.fold("Arc")(s => s"Arc: $s")

          val scienceConfig: Either[String, (D, CalLocation)] =
            atom
              .steps
              .zipWithIndex
              .find(_._1.stepConfig.stepType === StepType.Science)
              .map((s, l) => (s.value, if (l === 0) CalLocation.After else CalLocation.Before))
              .toRight("Atom contains no science step!")

          scienceConfig.flatTraverse { (d, loc) =>
            cache
              .get(d)
              .fold(expander.expandAtom(smartArc(arcTitle, d)))(_.asRight.pure)
              .map(_.map { arc => (cache.updated(d, arc), atom, Arc(arc, loc)) })
          }
        }
        .map {
          case Left(error)             => (cache, error.asLeft)
          case Right(cache, atom, arc) => (cache, (atom, arc).asRight)
        }
      }
      .map(_._2)

    // Processes the stream of science atoms from the pure generator, expanding
    // smart flats, adding expanded arcs where necessary, combining with the
    // atom index, and filtering out executed atoms.
    def scienceSequence(
      comp: Completion.SequenceMatch[D]
    ): Pipe[F, ProtoAtom[ProtoStep[D]], Either[String, (ProtoAtom[ProtoStep[D]], Long)]] =
      (s: Stream[F, ProtoAtom[ProtoStep[D]]]) => {

        // A set of instrument configs for which a matching arc was found in the
        // current visit.
        val matchedArcs: Set[D] = comp.current.toList.flatMap(_._2.toList.flatMap(_._1)).collect {
          case (d, StepConfig.Gcal(lamp,_,_,_)) if lamp.lampType === GcalLampType.Arc => d
        }.toSet

        case class Accumulator(
          comp:          Completion.SequenceMatch[D],
          index:         Long,
          generatedArcs: Set[D] // inst configs for arcs that we've already generated
        ) {
          def shouldGenerateArc(d: D): Boolean =
            !(matchedArcs(d) || generatedArcs(d))
        }

        type MatchState[A] = State[Completion.SequenceMatch[D], A]

        s.through(expander.expandSequence)  // smart-flat expansion
         .through(zipWithArc)               // tuple with corresponding arc
         .mapAccumulate(Accumulator(comp, 1L, Set.empty)) {

           case (acc, Left(error))        =>
             (acc, error.asLeft)

           case (acc, Right((atom, arc))) =>
             val arcConfig = arc.atom.steps.head.value

             val past: MatchState[(Int, Option[D], List[(ProtoAtom[ProtoStep[D]], Long)])] =
               (0, none[D], List.empty[(ProtoAtom[ProtoStep[D]], Long)]).pure[MatchState]

             val current: MatchState[(Int, Option[D], List[(ProtoAtom[ProtoStep[D]], Long)])] =
               Completion.SequenceMatch.matchCurrent(atom).map { vid =>
                 val genAtom = Option.unless(vid.isDefined)(atom)
                 val genArc  = Option.when(acc.shouldGenerateArc(arcConfig))(arc.atom)
                 // The atoms tupled with a consistent unique "index".  This will be folded into the
                 // atom id generation. The indices have to stay the same regardless of which atoms
                 // are executed (and therefore not emitted again in the stream).  We don't care
                 // about the actual number or whether they increment by exactly one for each atom,
                 // but rather that they are unique and consistent from run to run.
                 val atoms = arc.location match {
                   case CalLocation.Before => genArc.tupleRight(acc.index + 1).toList ++ genAtom.tupleRight(acc.index + 2).toList
                   case CalLocation.After  => genAtom.tupleRight(acc.index + 1).toList ++ genArc.tupleRight(acc.index + 2).toList
                 }
                 (2, arcConfig.some, atoms)
               }

             val update = for {
               vid  <- Completion.SequenceMatch.matchPast(atom)
               skip <- vid.fold(false.pure[MatchState])(Completion.SequenceMatch.contains(arc.atom)) // same visit must contain the arc
               m    <- if (skip) past else current
             } yield m

             val (compʹ, (inc, genArc, steps)) = update.run(acc.comp).value
             val accʹ = Accumulator(compʹ, acc.index + inc, acc.generatedArcs ++ genArc)

             (accʹ, steps.asRight)
         }
         .flatMap {
           case (_, Left(error))  => Stream.emit(Left(error))
           case (_, Right(steps)) => Stream.emits(steps.map(_.asRight))
         }
      }

    def generate(
      acquisitionItc: IntegrationTime,
      scienceItc:     IntegrationTime,
      completion:     Completion.Matcher[D]
    ): ProtoExecutionConfig[F, S, Either[String, (ProtoAtom[ProtoStep[D]], Long)]] =
      pureGen
        .scienceTarget(acquisitionItc, scienceItc)
        .mapSequences(
          defaultExpandAndFilter(expander, completion.acq), // acquisition
          scienceSequence(completion.sci)                   // science
        )
  }

  case class SpectroPhotometricGenerator[F[_]: Applicative, S, D](
    pureGen:  PureLongSlit[S, D],
    expander: SmartGcalExpander[F, D]
  ) extends Generator[F, S, D] {
    def generate(
      acquisitionItc: IntegrationTime,
      scienceItc:     IntegrationTime,
      completion:     Completion.Matcher[D]
    ): ProtoExecutionConfig[F, S, Either[String, (ProtoAtom[ProtoStep[D]], Long)]] =
      pureGen
        .spectroPhotometric(acquisitionItc, scienceItc)
        .mapSequences(
          defaultExpandAndFilter(expander, completion.acq), // acquisition
          defaultExpandAndFilter(expander, completion.sci)  // science
        )
  }

  private def instantiate[F[_]: Applicative, S, D](
    pureGen:  PureLongSlit[S, D],
    expander: SmartGcalExpander[F, D],
    calRole:  Option[CalibrationRole]
  ): Option[Generator[F, S, D]] =
    calRole match {
      case None                                     => ScienceTargetGenerator(pureGen, expander).some
      case Some(CalibrationRole.SpectroPhotometric) => SpectroPhotometricGenerator(pureGen, expander).some
      case _                                        => none
    }

  def gmosNorth[F[_]: Applicative](
    config:   Config.GmosNorth,
    expander: SmartGcalExpander[F, DynamicConfig.GmosNorth],
    calRole:  Option[CalibrationRole]
  ): Option[Generator[F, StaticConfig.GmosNorth, DynamicConfig.GmosNorth]] =
    instantiate(PureLongSlit.gmosNorth(config), expander, calRole)

  def gmosSouth[F[_]: Applicative](
    config:   Config.GmosSouth,
    expander: SmartGcalExpander[F, DynamicConfig.GmosSouth],
    calRole:  Option[CalibrationRole]
  ): Option[Generator[F, StaticConfig.GmosSouth, DynamicConfig.GmosSouth]] =
    instantiate(PureLongSlit.gmosSouth(config), expander, calRole)

}