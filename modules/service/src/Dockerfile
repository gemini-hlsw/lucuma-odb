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

# Copy migration scripts into the image
COPY ./main/resources/db/migration/ /docker-entrypoint-initdb.d/

# Remove the Postgres daemon from the entrypoint script to only run the init scripts
RUN sed -i 's/exec "$@"/echo "skipping..."/' /usr/local/bin/docker-entrypoint.sh && \
  # Run the init scripts
  /usr/local/bin/docker-entrypoint.sh postgres && \
  # Put everything back again
  sed -i 's/echo "skipping..."/exec "$@"/' /usr/local/bin/docker-entrypoint.sh
