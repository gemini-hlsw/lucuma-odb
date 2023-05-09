#!/bin/bash
heroku pg:backups:capture --app lucuma-postgres-odb-staging
heroku pg:backups:download --app lucuma-postgres-odb-staging --output /tmp/lucuma-postgres-odb-staging.dump
pg_restore --verbose --clean --no-acl --no-owner -h localhost -U jimmy -d lucuma-odb /tmp/lucuma-postgres-odb-staging.dump
rm /tmp/lucuma-postgres-odb-staging.dump


