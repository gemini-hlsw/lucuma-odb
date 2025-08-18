

# Lucuma Security Overview

The design goal is distributed trust/verification via token-passing, which lets us service most requests on behalf of users without having to consult a central authority.

## JWTs

SSO issues JSON Web Tokens (JWTs), which are cryptographically signed tokens that contain (among other things) an expiration date and user identity.

API servers require that requests include a valid JWT in an `Authorization: Bearer` header. If the JWT is malformed, expired, or fails signature verification the request is rejected; otherwise the request is serviced under the encoded identity.

Lucuma JWTs expire after ten minutes and must be renewed (see **JWT Refresh** below).

#### Discussion

The Lucuma JWT is visible to Javascript code and is in principle vulnerable to theft via scripting attack. The short lifetime mitigates this threat somewhat.

## JWT Refresh

SSO issues a non-expiring HTTP-Only cookie representing the user's persistent session. This cookie allows retrieval of a (new) JWT from SSO at any time. By this mechanism web applications can resume user sessions on load, and can replace JWTs before they expire.

When a user logs out, the session is deleted from SSO and the (now invalid) cookie is forgotten.

#### Discussion

HTTP Only cookies are not accessible from Javascript and are only sent with secure requests, so they can only be stolen by a thief who has physical access to a user's machine.

## API Keys

Advanced users can request an API key, which functions much like a JWT but does not expire and must be revoked explicitly. It is otherwise equivalent from the user's point of view and is passed in the same `Authorization: Bearer` header.

API servers validate API keys with SSO, receiving user identities in response. These results are cached locally for 3h.

Users can revoke API keys at any time. Cached sessions on API servers will remain active until they expire.

#### Discussion

SSO maintains a table of API key identifiers and hashes. The full API key is unrecoverable.

Users accept responsibility for protecting API keys.

## Service Accounts

Services which need to communicate directly (outside forwarded requests on behalf of a user) use service accounts that are created via a one-off program that is unavailable through any web API and must be invoked via `heroku run -a <sso-app> create-service-user <service-name>`. This program responds with a non-expiring JWT that can be provided to the API service on startup.

#### Discussion

Service JWTs are all-powerful, and like the SSO private key they exist only in service configuration.

## SSO Signing Keys

All API services are given the SSO private key as part of their configuration. If this key changes they all need to be reconfigured and restarted.

#### Discussion

The SSO private key is known only to SSO, and exists only as part of the service configuration on Heroku. Theft would require access to the organization's Heroku account.

A DNS attack could allow someone to spoof SSO, but without the private key no API server would accept JWTs it issued. This is why API servers do not ask SSO for its key on startup, and instead have it hardcoded into their configuration.

## ORCID Authentication

SSO delegates authentication to ORCID via an OAuth2 token exchange. User profile information is updated in the SSO cache on login and will propagate to use JWTs and API key sessions as they expire.