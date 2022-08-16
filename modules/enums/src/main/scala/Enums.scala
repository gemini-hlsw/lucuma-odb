// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.enums

import cats.Functor
import cats.syntax.all._
import clue.TransactionalClient
import lucuma.core.model.Partner
import lucuma.core.util.Enumerated

case class Enums(
  partner: Enumerated[Partner]
  // ...
)

object Enums {

  def fetch[F[_]: Functor](client: TransactionalClient[F, Nothing]): F[Enums] =
    (client.request(PartnerQuery).apply).map(apply(_))

}

given (using es: Enums): Enumerated[Partner] = es.partner
// ...