// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.syntax.eq.*
import cats.syntax.traverse.*
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalacheck.Prop.*

def conversionTest[A](keys: Set[String] = Set.empty)(using Arbitrary[A], Decoder[A], Encoder[A]): Prop =
  forAll { (a: A) =>
    Encoder[A].apply(a).asObject.exists { o =>

      // Leave only the keys referring to equivalent values in distinct units
      val o2 = if (keys.isEmpty) o else o.filterKeys(keys.contains)

      // Now all values should decode to the same thing.
      o2.toList.traverse { case (key, json) =>
        Decoder[A].apply(Json.obj(key -> json).hcursor)
      }.exists(_.toSet.size === 1)
    }
  }
