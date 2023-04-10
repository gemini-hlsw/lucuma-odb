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
