
run_query <- function(server_name = "server", 
                      db = "db", 
                      start_date = Sys.Date() - 91, # rolling 13 weeks (ish)
                      end_date = Sys.Date(),
                      schema_name = "schema",
                      tablename  = "table",
                      date_var = "date_co1", 
                      col_vars = c(
                        "date_col1",
                        "col1",
                        "col2",
                        "col3")) {
  
 con <- make_connection(server_name,db) 
# internal wrapper function that make a regular SQL Server connection
# this function uses the 'server_name' and 'db' variables from the configuration file
  
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  query <- glue_sql(.con = con, 
           "SELECT {`col_vars`*}, 
           '{`schema_name`}.{`tablename`}' AS 'tablename'
            FROM {`schema_name`}.{`tablename`} with(nolock)
            WHERE CAST({`schema_name`}.{`tablename`}.{`date_var`} AS DATE) BETWEEN {start_date} AND {end_date}")
  
  res <- DBI::dbGetQuery(con, query)
  
  dbDisconnect(con)

  return(res)  
}
