#!/bin/bash
set -e

echo "Applying SSO migrations to lucuma-sso database..."

# Apply SSO migrations in order
SSO_MIGRATIONS_DIR="/docker-entrypoint-initdb.d/sso-migrations"

if [ -d "$SSO_MIGRATIONS_DIR" ]; then
    # Apply migrations in version order
    while IFS= read -r migration_file; do
        if [ -f "$migration_file" ]; then
            psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "lucuma-sso" < "$migration_file"
        fi
    done < <(ls "$SSO_MIGRATIONS_DIR"/V*.sql | sort -V)
else
    echo "Warning: SSO migrations directory $SSO_MIGRATIONS_DIR does not exist"
fi

