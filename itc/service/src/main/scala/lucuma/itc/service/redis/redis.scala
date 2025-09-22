// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.redis

import boopickle.DefaultBasic.*
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyMap
import eu.timepit.refined.*
import eu.timepit.refined.api.*
import lucuma.core.data.Zipper
import lucuma.core.math.SignalToNoise
import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN
import lucuma.core.math.Wavelength
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.itc.*

import scala.collection.immutable.SortedMap
import scala.concurrent.duration.*

// --------------------------------------------------
// Pickler to store results in binary with boopickle
// The data is further gzipped
// --------------------------------------------------
given [A: Pickler, B](using Validate[A, B]): Pickler[A Refined B] =
  new Pickler[A Refined B] {
    override def pickle(a: A Refined B)(using state: PickleState): Unit = {
      state.pickle(a.value)
      ()
    }
    override def unpickle(using state: UnpickleState): A Refined B      = {
      val value = state.unpickle[A]
      refineV[B](value).getOrElse(sys.error("Cannot unpickle"))
    }
  }

given [A: Enumerated]: Pickler[A] =
  transformPickler((a: String) => Enumerated[A].fromTag(a).getOrElse(sys.error("Cannot unpickle")))(
    Enumerated[A].tag(_)
  )

given [A: Pickler]: Pickler[NonEmptyChain[A]] =
  transformPickler(Chain.fromSeq[A].andThen(NonEmptyChain.fromChainUnsafe[A]))(_.toChain.toList)

given [K: Pickler: Ordering, V: Pickler]: Pickler[SortedMap[K, V]] =
  transformPickler((m: Map[K, V]) => SortedMap.from(m))(_.toMap)

given [K: Pickler: Ordering, V: Pickler]: Pickler[NonEmptyMap[K, V]] =
  transformPickler(NonEmptyMap.fromMapUnsafe[K, V])(_.toSortedMap)

given Pickler[ItcSeries]      =
  transformPickler(Function.tupled(ItcSeries.apply))(x => (x.title, x.seriesType, x.data))
given Pickler[FiniteDuration] =
  transformPickler(n => new FiniteDuration(n, NANOSECONDS))(_.toNanos)

given Pickler[SignalToNoise] =
  transformPickler((bd: BigDecimal) =>
    SignalToNoise.FromBigDecimalExact
      .getOption(bd)
      .getOrElse(sys.error("cannot unpickle"))
  )(_.toBigDecimal)

given Pickler[TimeSpan] =
  transformPickler((d: Long) =>
    TimeSpan
      .fromMicroseconds(d)
      .getOrElse(sys.error("cannot unpickle"))
  )(_.toMicroseconds)

given Pickler[Wavelength] =
  transformPickler((i: Int) =>
    Wavelength.intPicometers
      .getOption(i)
      .getOrElse(sys.error("cannot unpickle"))
  )(_.toPicometers.value.value)

given Pickler[ItcGraph]                = generatePickler
given Pickler[ItcGraphGroup]           = generatePickler
given Pickler[ItcWarning]              = generatePickler
given Pickler[ItcCcd]                  = generatePickler
given Pickler[TotalSN]                 = transformPickler((s: SignalToNoise) => TotalSN(s))(_.value)
given Pickler[SingleSN]                = transformPickler((s: SignalToNoise) => SingleSN(s))(_.value)
given Pickler[TargetGraphsCalcResult]  = generatePickler
given Pickler[SignalToNoiseAt]         = generatePickler
given Pickler[IntegrationTime]         = generatePickler
given Pickler[Zipper[IntegrationTime]] =
  transformPickler[
    Zipper[IntegrationTime],
    (List[IntegrationTime], IntegrationTime, List[IntegrationTime])
  ]((l, a, r) => Zipper(l, a, r))(z => (z.lefts, z.focus, z.rights))
given Pickler[TargetIntegrationTime]   = generatePickler
