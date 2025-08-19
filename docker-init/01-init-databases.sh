#!/bin/bash
set -e

echo "Creating lucuma-sso database..."

# Create SSO database (lucuma-odb is created by POSTGRES_DB)
psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "$POSTGRES_DB" <<-EOSQL
    CREATE DATABASE "lucuma-sso";
EOSQL

echo "lucuma-sso database created. lucuma-odb migrations will be applied automatically."