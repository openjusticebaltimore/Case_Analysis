library(tidyverse)
library(lubridate)
library(DBI)
library(RPostgreSQL)
library(igraph)
# library(reshape2)

pwd <- ""
# create an PostgreSQL instance and create one connection.
drv <- dbDriver("PostgreSQL")

# open the connection using user, passsword, etc., as
con <- dbConnect(drv, dbname = "mjcs",
                 user="mjcs_ro",
                 password=,
                 port=5432,
                 host="mjcs.c7q0zmxhx4uo.us-east-1.rds.amazonaws.com")

cops.cops <- dbGetQuery(con, statement = 'SELECT 
                        name, cases.case_number
                        FROM cases inner join dscr_related_persons on cases.case_number=dscr_related_persons.case_number
                        WHERE connection in (\'WITNESS/POLICE OFFICER\',\'COMPLAINANT/POLICE OFFICER\')
and cases.filing_date>\'2016-12-31\'
                        ')
cops.cops <- arrange(cops.cops,case_number)%>% filter(!is.na(name))

# cops.cops$Relation <- ifelse(!duplicated(cops.cops$case_number),1,NA)
# while (sum(is.na(cops.cops$Relation))>0) {
#   cops.cops$Relation <- ifelse(duplicated(cops.cops$case_number),lag(cops.cops$Relation)+1,cops.cops$Relation)
# }
# cops.cops$Relation <- paste("Relation", cops.cops$Relation)
# cops.wide <- dcast(cops.cops,case_number~Relation, value.var = "name")

cop.relations <- left_join(dplyr::rename(cops.cops,name1=name) ,dplyr::rename(cops.cops,name2=name)) %>%
  filter(name1!=name2) %>% filter(!is.na(name))

cop.network <- graph_from_data_frame(d=unique(cop.relations[,c("name1","name2")]), vertices=unique(cops.cops$name), directed=F)
tiff("relations.tiff",width=10000,height=10000)
plot(cop.network,vertex.size=.1,label.cex=.1)
dev.off()