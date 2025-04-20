// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.binding

val ShortBinding: Matcher[Short] =
  IntBinding.emap { n =>
    if n.isValidShort then Right(n.toShort)
    else Left(s"Short: expected value in [${Short.MinValue} .. ${Short.MaxValue}], found $n")
  }
