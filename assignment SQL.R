library(sqldf)
library(RSQLite)
con <- dbConnect(SQLite(), dbname="~/chicagocrime.db")
getwd()
#1 How many crime incidents in the database?
dbGetQuery(con, "
           SELECT COUNT(*) AS TotalIncidents
           FROM crime
           ")

#2 IUCR codes for MOTOR VEHICLE THEFT
dbGetQuery(con, "
           SELECT IUCR
           FROM iucr
           WHERE PrimaryType='MOTOR VEHICLE THEFT'
           ")

#3 Incidents on Tuesdays between 9am to 5pm
dbGetQuery(con, "
           SELECT COUNT(*) AS TuesdayDaytimeIncidents
           FROM crime
           WHERE STRFTIME('%w', date) = '2'
           AND STRFTIME('%H', date) >= '09'
           AND STRFTIME('%H', date) <= '17'
            ")

#4 What was the PrimaryType for case HX536540
dbGetQuery(con, "
           SELECT iucr.PrimaryType
           FROM crime
           INNER JOIN iucr ON crime.IUCR = iucr.IUCR
           WHERE crime.CaseNumber = 'HX536540'
           ")

#5 Number of motor vehicle thefts in each ward 
dbGetQuery(con, "
           SELECT COUNT(*) AS TheftCount,
           CAST(crime.Ward AS INTEGER) AS WardNum
           FROM crime
           INNER JOIN iucr ON crime.IUCR = iucr.IUCR
           WHERE iucr.PrimaryType = 'MOTOR VEHICLE THEFT'
           GROUP BY WardNum
           ORDER BY WardNum
           ")
#6 Primary Type for crimes at '100XX W OHARE ST'
dbGetQuery(con, "
           SELECT DISTINCT iucr.PrimaryType
           FROM crime
           INNER JOIN iucr ON crime.IUCR = iucr.IUCR
           WHERE crime.Block = '100XX W OHARE ST'
           ")
dbDisconnect(con)






