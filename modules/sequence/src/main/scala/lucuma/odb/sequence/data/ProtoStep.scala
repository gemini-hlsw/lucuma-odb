// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import cats.Applicative
import cats.Eq
import cats.Eval
import cats.Traverse
import cats.syntax.functor.*
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SmartGcalType
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.TelescopeConfig
import monocle.Focus
import monocle.Lens

case class ProtoStep[A](
  value:           A,
  stepConfig:      StepConfig,
  telescopeConfig: TelescopeConfig,
  observeClass:    ObserveClass,
  breakpoint:      Breakpoint = Breakpoint.Disabled
):

  def withoutBreakpoint: ProtoStep[A] =
    copy(breakpoint = Breakpoint.Disabled)

  def withBreakpoint: ProtoStep[A] =
    copy(breakpoint = Breakpoint.Enabled)

object ProtoStep:

  def fromStep[A](s: Step[A]): ProtoStep[A] =
    ProtoStep(
      s.instrumentConfig,
      s.stepConfig,
      s.telescopeConfig,
      s.observeClass,
      s.breakpoint
    )

  def smartGcal[A](a: A, s: SmartGcalType, t: TelescopeConfig): ProtoStep[A] =
    ProtoStep(a, StepConfig.SmartGcal(s), t, ObserveClass.NightCal)

  def smartArc[A](a: A, t: TelescopeConfig): ProtoStep[A]  = smartGcal(a, SmartGcalType.Arc, t)
  def smartFlat[A](a: A, t: TelescopeConfig): ProtoStep[A] = smartGcal(a, SmartGcalType.Flat, t)

  /** @group Optics */
  def value[A]: Lens[ProtoStep[A], A] =
    Focus[ProtoStep[A]](_.value)

  /** @group Optics */
  def stepConfig[A]: Lens[ProtoStep[A], StepConfig] =
    Focus[ProtoStep[A]](_.stepConfig)

  /** @group Optics */
  def telescopeConfig[A]: Lens[ProtoStep[A], TelescopeConfig] =
    Focus[ProtoStep[A]](_.telescopeConfig)

  /** @group Optics */
  def observeClass[A]: Lens[ProtoStep[A], ObserveClass] =
    Focus[ProtoStep[A]](_.observeClass)

  /** @group Optics */
  def breakpoint[A]: Lens[ProtoStep[A], Breakpoint] =
    Focus[ProtoStep[A]](_.breakpoint)

  given [A](using Eq[A]): Eq[ProtoStep[A]] =
    Eq.by { x => (
      x.value,
      x.stepConfig,
      x.telescopeConfig,
      x.observeClass,
      x.breakpoint
    )}

  given Traverse[ProtoStep] with {
    override def traverse[G[_]: Applicative, A, B](fa: ProtoStep[A])(f: A => G[B]): G[ProtoStep[B]] =
      f(fa.value).map(ProtoStep(_, fa.stepConfig, fa.telescopeConfig, fa.observeClass, fa.breakpoint))

    override def foldLeft[A, B](fa:  ProtoStep[A], b:  B)(f:  (B, A) => B): B =
      f(b, fa.value)

    override def foldRight[A, B](fa:  ProtoStep[A], lb:  Eval[B])(f:  (A, Eval[B]) => Eval[B]): Eval[B] =
      f(fa.value, lb)
  }

end ProtoStep