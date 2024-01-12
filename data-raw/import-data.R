# TODO:   Import example data for specimens package
#
# Author: Miguel Alvarez
################################################################################

library(vegtableDB)
library(specimensDB)
library(RPostgreSQL)

# Restore PostgreSQL database and connect
DB <- "vegetation_v3"

do_restore(
  dbname = DB,
  user = "miguel",
  filepath = file.path("../../db-dumps/00_dumps", DB)
)

conn <- connect_db(DB, user = "miguel")
adm <- connect_db("gadm_v3", user = "miguel")

# Import specimens and check for bryophytes
churo_survey <- read_spec(conn, adm, bulk = 2)

# TODO: Filter to minimum information
save(churo_survey, file = "data/churo_survey.rda")
