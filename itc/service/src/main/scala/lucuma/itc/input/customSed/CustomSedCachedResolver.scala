// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input.customSed

import boopickle.DefaultBasic.*
import cats.Order.given
import cats.data.NonEmptyMap
import cats.syntax.all.*
import lucuma.core.math.Wavelength
import lucuma.core.model.Attachment
import lucuma.itc.cache.BinaryEffectfulCache
import lucuma.itc.service.hashes.given
import lucuma.itc.service.redis.given

import scala.concurrent.duration.FiniteDuration

class CustomSedCachedResolver[F[_]](
  wrapped: CustomSed.Resolver[F],
  cache:   BinaryEffectfulCache[F],
  ttl:     FiniteDuration
) extends CustomSed.Resolver[F]:
  def resolve(id: Attachment.Id): F[NonEmptyMap[Wavelength, BigDecimal]] =
    cache.getOrInvokeBinary(id, wrapped.resolve(id), ttl.some, "itc:customSed:")
