// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.instances

import cats.kernel.Order
import org.http4s.Header

trait HeaderInstances {

  implicit def orderHeader[A, B <: Header.Type](implicit ev: Header[A, B]): Order[A] =
    Order.by(ev.value)

}

object HeaderInstances extends HeaderInstances