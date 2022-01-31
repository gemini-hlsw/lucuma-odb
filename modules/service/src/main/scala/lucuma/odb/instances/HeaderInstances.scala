package lucuma.odb.instances

import cats.kernel.Order
import org.http4s.Header

trait HeaderInstances {

  implicit def orderHeader[A](implicit ev: Header[A, _]): Order[A] =
    Order.by(ev.value)

}

object HeaderInstances extends HeaderInstances