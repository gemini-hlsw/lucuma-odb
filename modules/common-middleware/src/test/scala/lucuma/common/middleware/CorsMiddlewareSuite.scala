// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.common.middleware

import org.http4s.Uri

final class CorsMiddlewareSuite extends munit.FunSuite:

  private val domains = List("gemini.edu")

  // --- Allowed: exact match and real subdomains ---

  test("exact host match is allowed"):
    assert(CorsMiddleware.isAllowed(Uri.Host.unsafeFromString("gemini.edu"), domains))

  test("direct subdomain is allowed"):
    assert(CorsMiddleware.isAllowed(Uri.Host.unsafeFromString("www.gemini.edu"), domains))

  test("deep subdomain is allowed"):
    assert(CorsMiddleware.isAllowed(Uri.Host.unsafeFromString("a.b.gemini.edu"), domains))

  test("host that merely ends with the domain (no dot boundary) is rejected"):
    // Old buggy `endsWith("gemini.edu")` accepted these as trusted origins.
    assert(!CorsMiddleware.isAllowed(Uri.Host.unsafeFromString("notgemini.edu"), domains))
    assert(!CorsMiddleware.isAllowed(Uri.Host.unsafeFromString("evil-gemini.edu"), domains))
    assert(!CorsMiddleware.isAllowed(Uri.Host.unsafeFromString("xgemini.edu"), domains))

  test("unrelated domain is rejected"):
    assert(!CorsMiddleware.isAllowed(Uri.Host.unsafeFromString("evil.com"), domains))

  test("domain as a substring in a different TLD is rejected"):
    assert(!CorsMiddleware.isAllowed(Uri.Host.unsafeFromString("gemini.edu.com"), domains))
    assert(!CorsMiddleware.isAllowed(Uri.Host.unsafeFromString("gemini.edu.evil.com"), domains))

  // --- Edge cases ---

  test("empty host is rejected"):
    assert(!CorsMiddleware.isAllowed(Uri.Host.unsafeFromString(""), domains))

  test("empty allowed-domain list rejects everything"):
    assert(!CorsMiddleware.isAllowed(Uri.Host.unsafeFromString("gemini.edu"), Nil))

  // --- Multiple configured domains ---

  private val multiDomains = List("gemini.edu", "lucuma.xyz")

  test("both configured domains are allowed (exact match)"):
    assert(CorsMiddleware.isAllowed(Uri.Host.unsafeFromString("gemini.edu"), multiDomains))
    assert(CorsMiddleware.isAllowed(Uri.Host.unsafeFromString("lucuma.xyz"), multiDomains))

  test("subdomains of each configured domain are allowed"):
    assert(CorsMiddleware.isAllowed(Uri.Host.unsafeFromString("www.gemini.edu"), multiDomains))
    assert(CorsMiddleware.isAllowed(Uri.Host.unsafeFromString("app.gemini.edu"), multiDomains))
    assert(CorsMiddleware.isAllowed(Uri.Host.unsafeFromString("www.lucuma.xyz"), multiDomains))
    assert(CorsMiddleware.isAllowed(Uri.Host.unsafeFromString("odb.lucuma.xyz"), multiDomains))

  test("an unrelated third domain is rejected under multi-domain config"):
    assert(!CorsMiddleware.isAllowed(Uri.Host.unsafeFromString("evil.com"), multiDomains))

  test("the suffix-bypass attack fails against each configured domain"):
    // Borrowing the gemini.edu suffix without a dot boundary:
    assert(!CorsMiddleware.isAllowed(Uri.Host.unsafeFromString("notgemini.edu"), multiDomains))
    // Borrowing the lucuma.xyz suffix without a dot boundary:
    assert(!CorsMiddleware.isAllowed(Uri.Host.unsafeFromString("notlucuma.xyz"), multiDomains))
    assert(!CorsMiddleware.isAllowed(Uri.Host.unsafeFromString("evil-lucuma.xyz"), multiDomains))

  test("a host cannot bypass by combining one domain's name with another's TLD"):
    // Not a subdomain of either configured domain, despite reusing parts of each name.
    assert(!CorsMiddleware.isAllowed(Uri.Host.unsafeFromString("gemini.xyz"), multiDomains))
    assert(!CorsMiddleware.isAllowed(Uri.Host.unsafeFromString("lucuma.edu"), multiDomains))

  test("a real subdomain of one domain is allowed even if it mentions the other"):
    // `gemini.lucuma.xyz` is a legitimate subdomain of lucuma.xyz, so it must be allowed.
    assert(CorsMiddleware.isAllowed(Uri.Host.unsafeFromString("gemini.lucuma.xyz"), multiDomains))
    assert(CorsMiddleware.isAllowed(Uri.Host.unsafeFromString("lucuma.gemini.edu"), multiDomains))

  test("multi-domain matching is case-insensitive"):
    assert(CorsMiddleware.isAllowed(Uri.Host.unsafeFromString("WWW.GEMINI.EDU"), multiDomains))
    assert(CorsMiddleware.isAllowed(Uri.Host.unsafeFromString("App.Lucuma.XYZ"), multiDomains))
    // And the suffix attack still fails regardless of case.
    assert(!CorsMiddleware.isAllowed(Uri.Host.unsafeFromString("NOTLUCUMA.XYZ"), multiDomains))

  // --- Case-insensitivity (both sides) ---

  test("host matching is case-insensitive"):
    assert(CorsMiddleware.isAllowed(Uri.Host.unsafeFromString("WWW.GEMINI.EDU"), domains))
    assert(CorsMiddleware.isAllowed(Uri.Host.unsafeFromString("Www.Gemini.Edu"), domains))

  test("configured domain matching is case-insensitive"):
    assert(
      CorsMiddleware.isAllowed(
        Uri.Host.unsafeFromString("www.gemini.edu"),
        List("GEMINI.EDU")
      )
    )
    // And the suffix attack still fails regardless of case.
    assert(
      !CorsMiddleware.isAllowed(
        Uri.Host.unsafeFromString("NOTGEMINI.EDU"),
        List("gemini.edu")
      )
    )

end CorsMiddlewareSuite
