# Lucuma Observing Database

This is the Postgres-backed observing database for GPP, under construction.

## Working on the Database Schema

The highlights:

- Step 1 is `chmod 0600 test-cert/*`
- To start up a local database and a simple web UI: `docker-compose up -d` ... if stuff doesn't seem to work drop the `-d` and see what's going on. You can then `^C` out of it and try again.
- The database will be created and initialized if it doesn't yet exist.
- The Schema is defined by `.sql` files in `modules/service/src/main/resources/db/migration/`.
- To inspect the database using a web UI, go [here](http://localhost:8686)
- The password for user `jimmy` is `banana`.
- To connect using `psql` (if you have it): `psql -h localhost -U jimmy -d lucuma-odb`
- To connect using `psql` (if you don't have it): `docker-compose run db psql -h db -U jimmy -d lucuma-odb`
- To build a nice documentation website, look in `schemaapy/`
- To stop the database (and the web UI): `docker-compose stop`
- To stop **and delete** the database (and the web UI): `docker-compose down`

To work on the schema I just do `docker-compose down; docker-compose up` to wipe it and rebuild from the schema. Any errors will be reported on the console. For now we're not worried about migrations so files can be edited arbitrarily. Once we're deployed migrations will need to be append-only.

## Running the App

The application assumes that you have an empty database initially and by default
runs migrations to set it up.  This will fail if you have followed the instructions
above and used `docker-compose up` because [Flyway](https://flywaydb.org), the database
migration tool, has no record of having run before and tries to create tables
that already exist.

To get an empty database you can start the docker container as above and then do this:

```
psql -h localhost -U jimmy -d postgres -e -c 'drop database "lucuma-odb"'
psql -h localhost -U jimmy -d postgres -e -c 'create database "lucuma-odb"'
```

You can now run the app, and you can do `docker-compose stop`.  If you do
`down` and then `up` though you'll need to clear out the db again.

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

## S3/Cloudcube
We are using the Cloudcube heroku addon for accessing S3. In Heroku, the addon sets
the environment variables for us. However, when running locally these are what needs
to be set. If you want to actually upload/download attachments, you can get the
real values from the staging app in Heroku. Otherwise, you can use these as
dummy values

- CLOUDCUBE_ACCESS_KEY_ID = <Any string>
- CLOUDCUBE_SECRET_ACCESS_KEY = <Any string>
- CLOUDCUBE_URL = https://cube.wherever.com/lucuma-staging

There is also a required environment variable that sets the maximum file upload size
- FILE_UPLOAD_MAX_MB = 20

## Testing Migrations

If you're adding migration scripts you should ensure that they will run correctly on a populated
database.  There is a `populate-db-from-dev.sh` script, but currently it will fail in the restore
step due to `pg_dump` wiping out the search path.  To work around this, the full procedure is:


1. Create the binary database dump.
```
heroku pg:backups:capture --app lucuma-postgres-odb-dev
heroku pg:backups:download --app lucuma-postgres-odb-dev --output /tmp/lucuma-postgres-odb-dev.dump
```

2. Convert the dump to text SQL commands.
```
pg_restore --verbose --clean --no-acl --no-owner --if-exists --no-comments -f ~/dump.sql /tmp/lucuma-postgres-odb-dev.dump
```

3. Edit the `.sql` file commenting out the following line:
```
SELECT pg_catalog.set_config('search_path', '', false);
```

This permits functions to find types that haven't been prefixed with the schema name.

4. Clean the database.
```
docker-compose down
docker-compose up
```

5. Connect with psql (to postgres database) and drop and recreate the lucuma-odb database
```
psql -h localhost -U jimmy -d postgres
```

then

```
# drop database "lucuma-odb";
# create database "lucuma-odb";
^C
```

6. Use `psql` to restore the database.
```
psql -h localhost -U jimmy -d lucuma-odb -f ~/dump.sql
```
At this point the database is running locally with the data as it exists in main and any new
migrations are ready to run when the application starts.

7. Start the application in `sbt`.  This will cause the new migration to run and any errors
will be revealed.
```
service/reStart
```

## Mailgun

We are using the Mailgun heroku addon for sending and tracking emails.

If you need to install Mailgun in a new app, you can do so via:

`heroku addons:create mailgun:Starter --app <APP NAME>`

The addon manages the mailgun users so that anyone with access to the app on heroku can go to
the mailgun dashboard by clicking on `Mailgun` in the app resources. It also sets some of the
required config variables. However, some will need to be added/updated as described below.

The setup a custom domain in mailgun, go to the mailgun dashboard and navigate to `Sending > Domains`,
click the `Add Domain` button and follow the directions. Note that only the `TXT` records (`SPF` and `DKIM`)
need to be set as we are not receiving mail or trying to track user opens and clicks.

You will need to change the `MAILGUN_DOMAIN` config variable to point to the new domain. Like:

`MAILGUN_DOMAIN=mail.odb-dev.lucuma.xyz`

In order to receive the webhook events that track the status of the email (Accepted, Delivered, Permanent Failure, Temporary Failure), do the following:

- In the mailgun dashboard go to `Sending > Webhooks` and create a new webhook signing key.
- Set the `MAILGUN_WEBHOOK_SIGNING_KEY` config variable in the herok app to the key you created.
- Back in the dashboard, with the proper domain selected in the upper right, use the `Add webhook` button to add webhooks for `accepted`, `delivered`, `permanent_fail`, and `temporary_fail`. The url is the url for the heroku app, plus `/mailgun`. For example `https://lucuma-postgres-odb-dev.herokuapp.com/mailgun`.

Two more config variables are required:

- INVITATION_SENDER_EMAIL - this should be some user in the MAILGUN_DOMAIN. For example `explore@mail.odb-dev.lucuma.xyz`. This will be the sender of the invitation emails.
- EXPLORE_URL - The url of the Explore instance for this version of the ODB. For example `https://explore-dev.lucuma.xyz`. This is used to put a link to explore in the invitation emails.

To run the odb locally, the environment variables in this section must be set or the odb will fail when
loading the configuration. Here is the complete list with some example values.

- MAILGUN_API_KEY=<Any string> (A valid key can be obtained from the heroku app if you want to be able to send email)
- MAILGUN_DOMAIN=mail.odb-dev.lucuma.xyz
- MAILGUN_WEBHOOK_SIGNING_KEY=<Any string> (Can be anything, since webhooks won't be received locally)
- INVITATION_SENDER_EMAIL=explore@mail.odb-dev.lucuma.xyz
- EXPLORE_URL=https://explore-dev.lucuma.xyz

Mailgun sets other variables in the Heroku app, but they are for things we are not using.

## SOPS Setup for Nix Users

If using Nix flake for development, secrets are managed via SOPS:

1. **Get team key**: Obtain `odb-keys.txt` from team lead (see `KEY_DISTRIBUTION.md`)
2. **run direnv**: Run `direnv allow`
3. **Git hook** (recommended): Run `cp .githooks/pre-commit .git/hooks/pre-commit && chmod +x .git/hooks/pre-commit` to prevent accidental key commits

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



## SSO Local Development QuickStart

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

To run SSO locally you need to clean out the default database. If you're just running tests the default db is fine.

```
psql -h localhost -d postgres -U jimmy -c 'drop database "lucuma-sso"'
psql -h localhost -d postgres -U jimmy -c 'create database "lucuma-sso"'
```

Docker-compose also starts up a local nginx server that serves an example client application at:

- http://local.lucuma.xyz:8081/playground.html

### Using reStart for SSO

Alternatively, you can run the SSO app from within SBT with `ssoService/reStart`
(stopping with `ssoService/reStop`).  By default, this command will fail after
running `docker-compose` `down` and then `up` as described above.  You can
supply optional arguments to simplify development though:

* `--reset` - Drops then creates the database for you. Do this after cycling
`docker-compose` `down`, `up` to give flyway a chance to run the migration and
update its schema table.
* `--skip-migration` - Skips the database migration.  This assumes that the
database has been initialized already.  Usually this won't be necessary since
flyway already skips migrations that have previously run.

### Working on the SSO Schema

The SSO app runs the migrations in `/modules/sso-service/src/main/resources/db/migration` on startup.

### Connecting to the SSO Database

You can connect to your dev SSO database with locally-installed tools like `pgAdmin` and `psql` as follows. Note that it's important to explicitly specify `localhost` as the host, for example `psql -h localhost -d lucuma-sso -U jimmy`.

| Parameter | Value        |
|-----------|--------------|
| host      | `localhost`  |
| port      | `5432`       |
| database  | `lucuma-sso` |
| user      | `jimmy`      |
| password  | `banana`     |

### Setting up ORCID Credentials

If you try to run `ssoService/reStart` you will find that it barfs because it needs some ORCID configuration. To set this up, sign into [ORCID](http://orcid.org) as yourself, go to **Developer Tools** under the name menu and create an API key with redirect URL `http://localhost:8080/auth/v1/stage2`. This will allow you to test ORCID authentication locally.

You will need to provide `LUCUMA_ORCID_CLIENT_ID` and `LUCUMA_ORCID_CLIENT_SECRET` either as environment variables or system properties when you run the SSO service. To do this in VS-Code you can hit F5 and then set up a run configuration as follows after which hitting F5 again should run SSO locally. Output will be in the Debug Console window.

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
      "buildTarget": "ssoService",
      "jvmOptions" : [
        "-DLUCUMA_ORCID_CLIENT_ID=<client-id>",
        "-DLUCUMA_ORCID_CLIENT_SECRET=<client-secret>",
      ],
    }
  ]
}
```
>>>>>>> 2514fdff2 (cleanup)
