setwd("C:/Users/VSharma/Desktop/R files")
require(dplyr)
require(RODBC)
require(sqldf)
require(lubridate)
RateNames<-read.csv("Ratenames.csv",stringsAsFactors = FALSE)
GLTypes<-read.csv("GLTypes.csv",stringsAsFactors = FALSE)
RateSched<-read.csv("Rateschedule.csv",stringsAsFactors = FALSE)
MN<-read.csv("Master_Names.csv",stringsAsFactors = FALSE)

##----------------------- Sourcing the date function from the helper function--------------------
source("helper_fns.R")

## --------------------Fetching the data from the Server Table F1915-----------------------------

ch <- odbcDriverConnect("Server=lynxRO.mnwd.local;Driver=SQL Server;Catalog=JDE_PRODUCTION_COPY")
res <- sqlQuery(ch, "SELECT NRBITM, NRGLCZ, NRAG,NRBITB,NRGLDTE from PRODDTA.F1915 WHERE NRGLDTE >0")


##----------------------------------Applying the data function to covert the date in YMD format--------------------------------------------

odbcCloseAll()

res$NRGLDTE <- mapply(fnDateFromServer,res$NRGLDTE)
res$NRGLDTE <- as.Date(res$NRGLDTE, format = "%Y-%m-%d")

relatabel<-select(res,NRBITM,NRGLCZ,NRAG,NRBITB,NRGLDTE)%>%
  filter(NRGLDTE>="2015-04-01" & NRGLDTE<="2016-03-31")

##-----------------------------------------------------------------------------------------------
newlist<-MN[MN$Object.Name=="F1915",]
colnames( relatabel ) <- newlist[ match( colnames(relatabel) , newlist[ , 'SQL.Column.Name' ] ) , 
                                  'Data.Item.Alpha' ]



#-----------------------------creating the year month column---------------------------------------
res1<-relatabel
res1$YrMonth<-paste(year(res1$GLDate),'-',
                   month(res1$GLDate),sep='')

# grouping  the data  monthly  for customemer types and revenue types for the specific period--------

Report<-res1%>%
             group_by(YrMonth,RateCode,GlClassAlternative,RateSchedule)


reportV1<-Report

Ratesched<-select(RateSched,Rate.Schedule,Rate.Code)

## joining rate Names and Rate Schedule table to join the customer type , rate codes and rate schedule-----

colnames(reportV1)[colnames(reportV1)=="Rate.Code"] <- "RateCode"
colnames(Ratesched)[colnames(Ratesched)=="Rate.Code"] <- "RateCode"
customertable<-left_join(RateNames,Ratesched)



# -------------------------Trimming the blank space in the names-------------------------------



reportV1temp <- sqldf("select trim(RateCode) as RateCode,YrMonth,GlClassAlternative,AmountGross from reportV1")

reportV3<-left_join(reportV1temp,Ratesched,by="RateCode")

colnames(GLTypes)[colnames(GLTypes)=="GL.Code"] <- "GlClassAlternative"
reportV2temp <- sqldf("select trim(GlClassAlternative) as GlClassAlternative,RateCode,YrMonth,AmountGross from reportV3")

#---------------Joining the tables to get the Rate codes corresponding to each Enterprise type------- 

Final<-left_join(reportV2temp,GLTypes,by="GlClassAlternative")
##---------------- Joining the table to get the customer name corresponding to the rate codes-------

FinalV1<-inner_join(customertable,Final,by="RateCode")
FinalV1<-select(FinalV1,RateCode,GlClassAlternative,AmountGross,Customer.Type,YrMonth,Enterprise)

##-------------------------------------------------------------------------------------------------

#Taking out the unique values

FinalV1temp <- unique( FinalV1[ , 1:6 ] )

##-------------------------------------------------------------------------------------------------

#---------- grouping and summarising by customer type and month  ----------------------------------
FinalV2<-FinalV1temp%>%
           group_by(Customer.Type,Enterprise,YrMonth)%>%
           summarise(monthlyrev=sum(AmountGross))

##--------------------------------------------------------------------------------------------------

write.csv(FinalV2, "FinalV3.csv")
