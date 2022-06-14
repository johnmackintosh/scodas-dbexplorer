library(dbplyr)
library(dplyr)
library(odbc)
library(tidyr)
library(lubridate)
library(DBI)
library(data.table)
library(purrr)
library(tibble)
library(glue)
library(rlang)
library(yaml)

# assumes all necessary files are in same working directory
source("make_connection.R")
source("run_query.R")
source("funs.R")


configuration <- yaml::read_yaml("example_config.yml") # change as required

# variables
start_date <- configuration$dates$start_date
end_date <- Sys.Date()

# create a list of values to loop over

datalist <- list(server_name = configuration$con$Server, 
                 db = configuration$con$Database, 
                 schema_name = configuration$schema,  
                 tablename  = configuration$tablename, 
                 date_var = configuration$date_vars, 
                 col_vars = configuration$col_vars)

# iterate over the database with purrr, populating the SQL query with the parameters
# each time

data <- purrr::pmap(datalist, run_query)
data <- purrr::map2(data,configuration$date_vars, convert_dates_string)

# combine to one data.table
DT <- rbindlist(data, use.names = TRUE, fill = TRUE, idcol = FALSE)

# pivot to table, column, value
DT <- melt(DT, 
           id.vars = c('week_starting', 'isoweek', 'year', 'tablename'), 
           variable.name = 'column', 
           value.name = 'value')
           
           # you may get a warning about conversion
           # depending on your data / columns / values, this should be OK, but please check


DT <- DT[!is.na(value)] # remove NAs as the app wiil not render

DT <- DT[,tablename := gsub('"', "",tablename, fixed =  TRUE)][] # remove extra " " from table name

DT[, key := .I] # unique ID for each row for shiny purposes, otherwise meaningless

saveRDS(DT,"sourcedata.RDS") # rename as appropriate
