// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.Id
import lucuma.core.model.Observation
import lucuma.core.syntax.timespan.*
import lucuma.odb.sequence.data.GeneratorParams.GmosNorthLongSlit
import lucuma.odb.sequence.data.arb.ArbGeneratorParams
import lucuma.odb.sequence.util.CommitHash
import org.scalacheck.Prop.forAll
import org.scalacheck.*

final class GeneratorGmosSuite extends munit.ScalaCheckSuite {

  import ArbGeneratorParams.given

  val oid = Observation.Id.fromLong(100L).get
  val itc = TestItcClient.withSuccessResult[Id](1200.secondTimeSpan, 2, 750.0)
  val gen = Generator.fromClient(CommitHash.Zero, itc)

  test("all ids are unique") {
    forAll { (params: GmosNorthLongSlit) =>
      gen.generate(oid, params, useCache = true) match {

        case Generator.Result.Success(_, _, c) =>
          val atoms   = (c.acquisition.nextAtom :: c.acquisition.possibleFuture) :::
                        (c.science.nextAtom     :: c.science.possibleFuture)
          val atomIds = atoms.map(_.id)
          val stepIds = atoms.flatMap(_.steps).map(_.id)

          assertEquals(atomIds.distinct.size, atomIds.size, s"Reused atom ids: ${atomIds.mkString(", ")}")
          assertEquals(stepIds.distinct.size, stepIds.size, s"Reused step ids: ${stepIds.mkString(", ")}")

        case result =>
          fail(s"Expected success, got $result")

      }
    }
  }

}
