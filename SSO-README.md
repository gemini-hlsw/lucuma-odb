# LUCUMA-SSO

Single sign-on service and support libries for Lucuma.

## Server Configuration


SSO requires the following configuration in staging/production.

##### SSO App Config

| Variable | Value |
|----------|-------|
| `LUCUMA_ORCID_CLIENT_ID` | ORCID Client Id |
| `LUCUMA_ORCID_CLIENT_SECRET` | ORCID secret |
| `LUCUMA_SSO_COOKIE_DOMAIN` | Domain for refresh token cookie (`gemini.edu`) |
| `LUCUMA_SSO_ENVIRONMENT` | Constant value `staging` (will go away) |
| `LUCUMA_SSO_HOSTNAME` | External hostname for this service. |

##### GPG Information

SSO signs JWTs with a private key. Client applications verify JWTs with a public key.

| Variable | Value |
|----------|-------|
| `GPG_SSO_PUBLIC_KEY` | GPG ASCII-armored public key. |
| `GPG_SSO_PRIVATE_KEY` | GPG ASCII-armored private key. |
| `GPG_SSO_PASSPHRASE` | Passphrase to read private key. |


##### Heroku Information

The following configuration is provided by Heroku. You must enable the [`runtime-dyno-metadata`
extension](https://devcenter.heroku.com/articles/dyno-metadata) to get the full set.

| Variable |
|----|
| `DATABASE_URL` |
| `HEROKU_APP_ID` |
| `HEROKU_APP_NAME` |
| `HEROKU_DYNO_ID` |
| `HEROKU_RELEASE_CREATED_AT` |
| `HEROKU_RELEASE_VERSION` |
| `HEROKU_SLUG_COMMIT` |
| `HEROKU_SLUG_DESCRIPTION` |




## Web Client Flow

### Initialization

- Post to `/api/v1/refresh-token`
  - If you get a `403 Forbidden` you are not logged in.
    - Continue with **Login** below.
  - If you get a `200 Ok`
    - You are logged in.
    - The response body will contain a new JWT.
    - Set a timer to run **Initialization** again one minute before the JWT expires.
    - Continue with **Normal Operation** below.

### Login

The user must be allowed to choose to log in with ORCID (as a standard user) or log in as a guest.
Service users are not interactive users and cannot log in.

#### Login Roles for Standard Users
Standard users will always be logged in
under a PI role. If the user has no such role (possible but unusual) it will be created. The user can later switch to a different role (see **Set Role** below) and this choice will be peristed in the refresh token. Associating the role with the refresh tokan allows a user to be logged in under several roles at the same time, in different browser sessions.

#### Guest Login

- Post to `/api/v1/auth-as-guest`
  - The response will be `201 Created`.
  - The body will contain a JWT.
  - An http-only refresh cookie will be set.
  - Continue with **Normal Operation** below.

#### ORCID Login

- Perform a client-side redirect to `/auth/v1/stage1?state=APP_URI`
  - On success the user will be redirected to `APP_URI`.
    - An http-only refresh cookie will be set.
    - Continue with **Initialization** above.

### Normal Operation

- Store the JWT in a variable and pass it to all API requests in the header as `Authorization: Bearer <jwt>`.
- Decode the JWT body as a `lucuma.core.model.User`. A circe codec is provided by the `lucuma-sso-frontend-client` artifact.
- If the user has insufficient privileges to view the application, there are three possibilities that should be presented.
    - If the user is currently a Guest
      - allow the user to upgrade their guest account (**ORCID Login** above).
    - If the user has other roles that _are_ sufficient (via the `otherRoles` member on `StandardUser`)
      - Provide a menu that allows the user to select one of these roles
      - Continue with **Set Role** below.
    - Offer the option to log out.
      - Continue with **Log Out** below.
- Display a user menu with the user's `displayName` shown.
  - If the user is a guest, offer an option to log in via ORCID.
  - If the user is a standard user, offer the option to change role.
  - Offer an option to log out.

### Log Out

- POST to `https://sso.lucuma.gemini.edu/api/v1/logout`
- POST to `https://orcid.org/userStatus.json?logUserOut=true` (we need to test this)
- Continue with **Login** above.

### Set Role

- GET `/auth/v1/set-role?role=<role-id>` to start a new session with the same user in a different role (taken from the user's `otherRoles` collection).
  - This will set a new session cookie.
  - The response body will contain a new JWT.
  - Continue with **Normal Operation** above.

## Back-End Service Workflow

- See the `backend-client-example` module for an example of what follows.
- Add `lucuma-sso-backend-client` as a dependency.
- Ensure that your app is provided with the following configuration information:
  - Your SSO server's root URL (`https://sso.foo.com` for example)
  - Your SSO server's public key in GPG ASCII-armored format. For now you can grab this from the SSO server's Heroku configuration. We may change this to use a keyserver.
  - Your service JWT (see below).
- Construct an `SsoClient` using this configuration and make it available to your HTTP routes.
- Use the `SsoClient` to extract the requesting user from the http `Request` as needed.

### Obtaining a Service JWT

Each back-end service must have its own service JWT for communicating with other services. `SsoClient` must communicate with SSO to exchange API tokens, so `SsoClient` users need a service token. You can obtain one by running a one-off Heroku dyno command from your SSO server. The `service-name` argument is arbitrary but should identify your application for logging purposes (`observing-database`, `facility-service`, etc).

```
heroku run -a <sso-app-name> create-service-user <service-name>
```

### Obtaining a User JWT

For testing purposes it may sometimes be helpful to generate a JWT for a specific user. Generated JWTs are valid for one hour.

```
heroku run -a <sso-app-name> create-jwt <role-id>
```

### Discussion

It is possible to implement authentication as a middleware, but this makes composition of routes via `<+>` difficult because the middleware can either (a) reject unauthorized requests with `403 Forbidden`, which means no following routes can possibly match; or (b) ignore unauthorized requests, which means the user will see a `404 Not Found` instead of `403 Forbidden`. So the recommended strategy for now is to check authorization on each route as described above.



## Local Development QuickStart

- Step 1 is `chmod 0600 test-cert/*`
- Edit `/etc/hosts` to add `local.lucuma.xyz` as an alias of localhost.

```
127.0.0.1       localhost local.lucuma.xyz
```

Use `docker-compose` to wrangle a dev database. It's way easier than dealing with a real installation.

| Command                                                               | Description                                    |
|-----------------------------------------------------------------------|------------------------------------------------|
| `docker-compose up`                                                   | start up the test database                     |
| `docker-compose up -d`                                                | start up the test database in the background   |
| `docker-compose run postgres psql -h postgres -d lucuma-sso -U jimmy` | start up a `psql` shell (password is `banana`) |
| `docker-compose stop`                                                 | stop the test database                         |
| `docker-compose down`                                                 | destroy the database                           |

To run locally you need to clean out the default database. If you're just running tests the default db is fine.

```
psql -h localhost -d postgres -U jimmy -c 'drop database "lucuma-sso"'
psql -h localhost -d postgres -U jimmy -c 'create database "lucuma-sso"'
```

Docker-compose also starts up a local nginx server that serves an example client application at:

- http://local.lucuma.xyz:8081/playground.html

### Using reStart

Alternatively, you can run the app from within SBT with `service/reStart`
(stopping with `service/reStop`).  By default, this command will fail after
running `docker-compose` `down` and then `up` as described above.  You can
supply optional arguments to simplify development though:

* `--reset` - Drops then creates the database for you. Do this after cycling
`docker-compose` `down`, `up` to give flyway a chance to run the migration and
update its schema table. 
* `--skip-migration` - Skips the database migration.  This assumes that the 
database has been initialized already.  Usually this won't be necessary since
flyway already skips migrations that have previously run. 

### Working on the Schema

The app runs the migrations in `/modules/service/src/main/resources/db/migration` on startup.

### Connecting to the Database

You can connect to youe dev database with locally-installed tools like `pgAdmin` and `psql` as follows. Note that it's important to explicitly specify `localhost` as the host, for example `psql -h localhost -d lucuma-sso -U jimmy`.

| Parameter | Value        |
|-----------|--------------|
| host      | `localhost`  |
| port      | `5432`       |
| database  | `lucuma-sso` |
| user      | `jimmy`      |
| password  | `banana`     |

### Setting up ORCID Credentials

If you try to run `Main` you will find that it barfs because it needs some ORCID configuration. To set this up, sign into [ORCID](http://orcid.org) as yourself, go to **Developer Tools** under the name menu and create an API key with redirect URL `http://localhost:8080/auth/v1/stage2`. This will allow you to test ORCID authentication locally.

You will need to provide `LUCUMA_ORCID_CLIENT_ID` and `LUCUMA_ORCID_CLIENT_SECRET` either as environment variables or system properties when you run `Main`. To do this in VS-Code you can hit F5 and then set up a run configuration as follows after which hitting F5 again should run SSO locally. Output will be in the Debug Console window.

```json
{
  "version": "0.2.0",
  "configurations": [
    {
      "type"       : "scala",
      "request"    : "launch",
      "name"       : "Lucuma SSO",
      "mainClass"  : "lucuma.sso.service.Main",
      "args"       : [],
      "buildTarget": "service",
      "jvmOptions" : [
        "-DLUCUMA_ORCID_CLIENT_ID=<client-id>",
        "-DLUCUMA_ORCID_CLIENT_SECRET=<client-secret>",
      ],
    }
  ]
}
```
