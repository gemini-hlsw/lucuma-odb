#!/bin/bash
heroku pg:backups:capture --app lucuma-postgres-odb-dev
heroku pg:backups:download --app lucuma-postgres-odb-dev --output /tmp/lucuma-postgres-odb-dev.dump
pg_restore --verbose --clean --no-acl --no-owner -h localhost -U jimmy -d lucuma-odb /tmp/lucuma-postgres-odb-dev.dump
rm /tmp/lucuma-postgres-odb-dev.dump


