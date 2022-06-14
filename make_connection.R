make_connection <- function (server_name = NULL, db = NULL) {

    checkmate::assert_character(server_name)
    checkmate::assert_character(db)
   
    con <- DBI::dbConnect(odbc::odbc(), 
    Driver = "SQL Server", 
        Server = eval(server_name), 
        Database = eval(db), 
        Trusted_Connection = "True", 
        Port = 1433)
        
    connection_info <- paste0("Connecting to ", toupper(db), 
         " on ", toupper(server_name))
    message(connection_info)
    return(con)
}
