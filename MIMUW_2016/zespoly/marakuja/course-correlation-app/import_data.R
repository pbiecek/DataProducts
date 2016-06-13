#' Skrypt służący do pobierania danych o ocenach z USOSa.
#'
#' Wymagania:
#'    install.packages("JDBC")
#'
#' Użycie:
#'    Ustalić wartości poniższych parametrów.
#'    Rscript import_data.R


driverClass <- "oracle.jdbc.OracleDriver"
classPath <- "ojdbc6.jar"
host <- "usos.edu.pl"
port <- 1521
serviceName <- "USOS"
user <- "user"
password <- "password"
outputFile <- "courses_marks.csv"

#####

library(RJDBC)

codePrefixMIM <- "1000-"

driver <- JDBC(driverClass=driverClass,
               classPath=classPath,
               identifier.quote=" ")

url <- paste("jdbc:oracle:thin:@//", host, ":", port, "/", serviceName,
             sep="")

connection <- dbConnect(driver, url, user, password)

pattern <- paste("'", codePrefixMIM, "%'",
                sep="")

query <- paste("SELECT kod, prz_nazwa, numer_terminu, ocena_liczbowa, osoba FROM rstat_oceny ",
               "WHERE ocena_liczbowa is not null ",
               "AND kod LIKE ", pattern,
               sep="")

data <- dbGetQuery(connection, query)

write.csv(data, outputFile)