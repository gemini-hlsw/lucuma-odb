// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.syntax.eq.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import io.circe.Codec
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalacheck.Prop.*

def conversionTest[A: Arbitrary : Decoder : Encoder](keys: Set[String] = Set.empty): Prop =
  forAll { (a: A, i: Int) =>
    Encoder[A].apply(a).asObject.exists { o =>

      // Leave only the keys referring to equivalent values in distinct units
      val o2 = if (keys.isEmpty) o else o.filterKeys(keys.contains)

      // Now all values should decode to the same thing.
      o2.toList.traverse { case (key, json) =>
        Decoder[A].apply(Json.obj(key -> json).hcursor)
      }.exists(_.toSet.size === 1)
    }
  }
