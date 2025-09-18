#!/bin/bash

npm ci

node itc/schemastitcher.js modules/schema/src/main/resources/lucuma/odb/graphql/OdbSchema.graphql itc/service/src/main/resources/graphql/itc_base.graphql itc/service/src/main/resources/graphql/itc.graphql
