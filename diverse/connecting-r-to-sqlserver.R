
library(RODBC)
#dbconnection <- odbcDriverConnect("Driver={SQL Server};Server=direccion-fdi\\SQLEXPRESS; Database=Northwind;Uid=; Pwd=; trusted_connection=yes")
dbconnection <- odbcDriverConnect("Driver={SQL Server};Server=direccion-fdi\\SQLEXPRESS; Database=Northwind;Uid=sa; Pwd=123; trusted_connection=no")
initdata <-data.frame(sqlQuery(dbconnection,paste("select * from Products;"))) 
odbcClose(dbconnection)

