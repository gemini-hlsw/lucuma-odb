# Build a base image for test containers, with the database already initialized and migrated
# This file is in modules/service/src because it needs a shared path with `modules/service/src/main/resources/db/migration` so it cannot be put in `modules/service/src/test/resources`
FROM postgres:15

ARG POSTGRES_USER
ARG POSTGRES_PASSWORD
ARG POSTGRES_DB

ENV POSTGRES_USER=$POSTGRES_USER
ENV POSTGRES_PASSWORD=$POSTGRES_PASSWORD
ENV POSTGRES_DB=$POSTGRES_DB

# Custom PGDATA directory to avoid volume mount issues
ENV PGDATA=/var/lib/postgresql/odb-data

# The database is throwaway, so durability is not needed: skip fsync during initdb
ENV POSTGRES_INITDB_ARGS="--no-sync"

# Copy migration scripts into the image
COPY ./main/resources/db/migration/ /docker-entrypoint-initdb.d/

# Run initdb and the init scripts (migrations) without leaving a server running.
# The server args disable durability to speed up the migrations.
RUN docker-ensure-initdb.sh -c fsync=off -c synchronous_commit=off -c full_page_writes=off

# Also disable durability at test time, to speed up database writes in tests
CMD ["postgres", "-c", "fsync=off", "-c", "synchronous_commit=off", "-c", "full_page_writes=off"]
