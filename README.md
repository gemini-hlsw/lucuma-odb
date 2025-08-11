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

