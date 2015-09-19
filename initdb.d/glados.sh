#!/bin/bash

gosu postgres postgres --single -jE <<-EOSQL
  CREATE DATABASE "glados" ;
EOSQL

gosu postgres postgres --single -jE <<-EOSQL
  CREATE USER "glados" PASSWORD 'glados' ;
EOSQL
