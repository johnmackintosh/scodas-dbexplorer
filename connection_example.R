# assumes you use SQL SERVER with Windows authentication

con <- DBI::dbConnect(odbc::odbc(), 
                      Driver = "SQL Server", 
                      Server = "SERVER", 
                      Database = "DB_NAME", 
                      Trusted_Connection = "True", 
                      Port = 1433)

