include .env

PORT ?= 8999
PG_HOST ?= localhost
PG_PORT ?= 5432
PG_USER ?= myusername
PG_PASS ?= mypassword
PG_DB ?= blockchain


create-db:
	PGPASSWORD=$(PG_PASS) psql -U $(PG_USER) -d postgres -c "CREATE DATABASE $(PG_DB);"

init-db:
	PGPASSWORD=$(PG_PASS) psql -U $(PG_USER) -d $(PG_DB) -c "CREATE TABLE IF NOT EXISTS users (id SERIAL PRIMARY KEY, email VARCHAR NOT NULL);"
	PGPASSWORD=$(PG_PASS) psql -U $(PG_USER) -d $(PG_DB) -c "CREATE TABLE IF NOT EXISTS blocks (id SERIAL PRIMARY KEY, index INTEGER NOT NULL, timestamp TIMESTAMP NOT NULL, data TEXT NOT NULL, previous_hash TEXT NOT NULL, hash TEXT NOT NULL);"


set_env:
	@export PATH="C:\ghcup\bin:C:\Program Files\PostgreSQL\16\bin:$$PATH" && \
	export GHC_PACKAGE_PATH="" && \
	echo "Environment variables set"

build: set_env
	@ghcup set ghc 8.8.4
	@echo $$PATH
	@echo $$GHC_PACKAGE_PATH
	PG_HOST=$(PG_HOST) PG_PORT=$(PG_PORT) PG_DB=$(PG_DB) PG_USER=$(PG_USER) PG_PASS=$(PG_PASS) cabal build

run:
	PG_HOST=$(PG_HOST) PG_PORT=$(PG_PORT) PG_DB=$(PG_DB) PG_USER=$(PG_USER) PG_PASS=$(PG_PASS) cabal run

