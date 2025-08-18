# Backend Server Example

This is an example back-end service that accepts JWT and API keys and exposes the associated user
(if any) to the application's `HttpRoutes`. It also supports distributed tracing for incoming
requests *and* outgoing requests associated with API key exchange.

### The Desired Outcome

We want our `HttpRoutes` definition to be straightforward and provide access to everything we
need, with distributed tracing set up and available if we want to use it. Adding this stuff to an
existing app should not be a hassle.

So In this example `Main.routes` defines our http service, which has access to the user via an
`SSOClient`, and access to tracing via the `Trace[F]` instance if we need it. All incoming and
outgoing requests are traced via middleware, set up as described below.

### The Setup

The `SSOClient` provides access to the user associated with a request. It handles all the JWT
decoding and API key exchange. It is wraps an http4s `Client` that communicates with the SSO
server.

```
  SSOClient
    └─ NatchezMiddleware.client  // Adds tracing to outgoing requests, given `Trace[F]`.
         └─ Client               // Ember client
```

The `HttpRoutes` has two layers of tracing.

```
  HttpRoutes
    └─ NatchezMiddleware.server  // Continues incoming traces, adds url, etc., to request root trace.
         └─ SSOMiddleware        // Adds current user to the request root trace.
              └─ HttpRoutes      // Our original routes without middleware.
```

If you have no `Trace[F]` available then the tracing middlewares can be removed and it will all still
work, but without any visibility. _This is why tracing isn't integrated more deeply_: we apps to be
able to use `SSOClient` even if tracing is unavailable.