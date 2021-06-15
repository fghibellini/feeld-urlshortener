#!/bin/bash
docker run --name feeld-shortener-postgres -e POSTGRES_PASSWORD=mysecretpassword -e POSTGRES_USER=postgres -p 5432:5432 -d postgres

export PGUSER=postgres
export PGPASSWORD=mysecretpassword
export PGHOST=localhost
export PGPORT=5432

echo "connecting"
while ! psql -q -c 'SELECT 1;' 2>/dev/null 1>&2; do
  echo "waiting for postgres"
  sleep 0.5
done
echo "postgres ready!"

echo "creating tables"
psql -f ./init.sql
echo "all done!"
