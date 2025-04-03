# Build a base image for test containers, with the database already initialized and migrated
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

RUN sed -i 's/exec "$@"/echo "skipping..."/' /usr/local/bin/docker-entrypoint.sh && \
  /usr/local/bin/docker-entrypoint.sh postgres && \
  sed -i 's/echo "skipping..."/exec "$@"/' /usr/local/bin/docker-entrypoint.sh
