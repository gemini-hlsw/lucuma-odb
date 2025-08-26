#!/bin/bash
set -e

# Create SSO database
psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "$POSTGRES_DB" <<-EOSQL
    CREATE DATABASE "lucuma-sso";
EOSQL
