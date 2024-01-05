#!/bin/bash
set -e

psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "$POSTGRES_DB" <<-EOSQL
	CREATE EXTENSION tsm_system_rows;
	CREATE USER mxssy with encrypted password 'allyourmutationsarebelongtous';
	GRANT ALL PRIVILEGES ON DATABASE mxssy TO mxssy;
EOSQL
