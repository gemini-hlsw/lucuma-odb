// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package imaging

import cats.Order
import cats.data.NonEmptyList
import cats.syntax.all.*
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.SequenceType
import lucuma.core.enums.WavelengthOrder
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.Atom
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.util.AtomBuilder

import java.util.UUID

/**
 * Shared Imaging sequence functions.
 */
object ImagingSequence:

  /**
   * Builds a `SequenceGenerator` from a stream of proto atoms
   * parameterized by the static and dynamic config types
   */
  def makeGenerator[S, D](
    estimator:  StepTimeEstimateCalculator[S, D],
    static:     S,
    namespace:  UUID,
    protoAtoms: Stream[Pure, ProtoAtom[ProtoStep[D]]]
  ): SequenceGenerator[D] =

    case class ImagingGenerator(
      recordedSteps: Map[ProtoStep[D], Int]
    ) extends SequenceGenerator[D]:

      override def generate: Stream[Pure, Atom[D]] =
        val build = AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Science)

        protoAtoms
          .zipWithIndex
          .mapAccumulate(StepTimeEstimateCalculator.Last.empty[D]):
            case (cs, (protoAtom, idx)) =>
              build.build(protoAtom.description, idx.toInt, 0, protoAtom.steps).run(cs).value
          .map(_._2)
          .mapAccumulate(recordedSteps): (rec, nextAtom) =>
            val (recʹ, optSteps) = nextAtom.steps.toList.mapAccumulate(rec): (rec, step) =>
              val protoStep = ProtoStep.fromStep(step)
              (
                rec.updatedWith(protoStep): count =>
                  count.map(_ - 1).orElse(none).filter(_ > 0),
                Option.when(!rec.contains(protoStep))(step)
              )
            (
              recʹ,
              NonEmptyList.fromList(optSteps.flatten).map(steps => nextAtom.copy(steps = steps))
            )
          .collect { case (_, Some(atom)) => atom }

    ImagingGenerator(Map.empty)

  def wavelengthOrder[L](variant: Variant)(f: L => Wavelength): Order[L] =
    import WavelengthOrder.*
    variant.fold(_.order, _ => Increasing, _ => Increasing) match
      case Increasing => Order.by(f)
      case Decreasing => Order.reverse(Order.by(f))
