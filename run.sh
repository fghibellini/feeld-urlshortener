#!/usr/bin/env bash
export PGUSER=postgres
export PGPASSWORD=mysecretpassword
export PGHOST=localhost
export PGPORT=5432
stack run feeld-shortener-exe --
