// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.client

import cats.kernel.Eq
import io.circe.testing.CodecTests
import io.circe.testing.instances.arbitraryJson
import lucuma.core.model.*
import lucuma.core.model.arb.ArbOrcidProfile.given
import lucuma.core.model.arb.ArbStandardRole.*
import lucuma.core.model.arb.ArbUser.given
import lucuma.core.util.arb.ArbGid.given
import lucuma.sso.client.codec.orcidProfile.given
import lucuma.sso.client.codec.role.*
import lucuma.sso.client.codec.user.*
import munit.DisciplineSuite
import munit.ScalaCheckSuite

class CodecSuite extends ScalaCheckSuite with DisciplineSuite {

  // TODO: move to core
  implicit val EqStandardRole: Eq[StandardRole] =
    Eq.fromUniversalEquals

  // TODO: move to core
  implicit val EqUser: Eq[User] =
    Eq.fromUniversalEquals

  checkAll("GidCodec", CodecTests[User.Id].unserializableCodec)
  checkAll("OrcidProfileCodec", CodecTests[OrcidProfile].unserializableCodec)
  checkAll("StandardRoleCodec", CodecTests[StandardRole].unserializableCodec)
  checkAll("UserCodec", CodecTests[User].unserializableCodec)

}


