// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service

import cats.effect.IO
import cats.syntax.all.*
import weaver.Expectations

trait FlakyTests {

  // Copied from https://dimitarg.github.io/pure-testing-scala/#flake
  def flaky(attempts: Int = 3)(x: IO[Expectations]): IO[Expectations] = {
    if(attempts < 1) {
      x
    } else {
      x.attempt.flatMap(
        _.fold[IO[Expectations]](
          _ => flaky(attempts-1)(x),
          result => {
            if(result.run.isValid) {
              result.pure[IO]
            } else {
              flaky(attempts-1)(x)  
            }  
          }  
        )
      )
    }
  }
}