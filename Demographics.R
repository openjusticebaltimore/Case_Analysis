# Here's some programmatic graphing of demographics of defendants

# Program Parameters

# Set the working directory. This is where the
setwd("Filepath")

# Name the subdirectory filepath or subdirectory where you want to save outputs. Leave it as "" if you want to save outputs to the working directory
outfolder<-"filepath"
# Credentials for the database connection
user <- ""
password <- ""

# Install the required packages
# install.packages("DBI")
# install.packages("RPostgreSQL")
# install.packages("tidyverse")
# install.packages("lubridate")

# Load Them
require("DBI")
require("RPostgreSQL")
require("tidyverse")
require("lubridate")

# The types of cases (for reference)
# DSCIVIL - district civil system (DONE)
# DSTRAF - district traffic court
# DSCR - district criminal system (DONE)
# DSCP - district civil citations
# DSK8 - circuit criminal system (DONE)
# DV - district domestic violence (civil system)
# CC - circuit civil system (DONE)
# K - circuit criminal (seems to all be child support cases?)

# create an PostgreSQL instance and create a connection
drv <- dbDriver("PostgreSQL")

# open the connection using user, passsword, etc., as
con <- dbConnect(drv, dbname = "mjcs",
                 port=5432,
                 user=user,
                 password=password,
                 host="mjcs.c7q0zmxhx4uo.us-east-1.rds.amazonaws.com")

#######################################################
# Sex by Age for district criminal cases for each year#
#######################################################
source<-dbGetQuery(con, statement = '
                   select 
                   sex, race, filing_date, "DOB" 
                   from
                   cases c 
                   inner join dscr_defendants def on c.case_number=def.case_number 
                   where 1=1 
                   ')

sexage<-source %>%
  group_by(sex,
           race,
           age=floor(time_length(filing_date-DOB, "years")),
           filing.year=year(filing_date)) %>%
  summarise(Count=n())



# Here's a SQL query that does the same thing, but for some reason the PostgreSQL age calculations are a little bit different from the lubridate ones...  
# summary<-dbGetQuery(con, statement = '
#                     select 
#                     count(def.*) Count,
#                     def.race,
#                     def.sex,
#                     extract(year from c."filing_date") filing_yr
#                     
#                     , case when def."DOB" is null then \'\' else  (case when extract(year from age(def."DOB"))<18 then \'17 and under\'
#                     when extract(year from age(def."DOB"))>17 and  extract(year from age(def."DOB"))<26 then \'18-25\'
#                     when extract(year from age(def."DOB"))>25 and  extract(year from age(def."DOB"))<31 then \'26-30\'
#                     when extract(year from age(def."DOB"))>30 and  extract(year from age(def."DOB"))<36 then \'31-35\'
#                     when extract(year from age(def."DOB"))>35 and  extract(year from age(def."DOB"))<41 then \'36-40\'
#                     when extract(year from age(def."DOB"))>40 and  extract(year from age(def."DOB"))<46 then \'41-45\'
#                     when extract(year from age(def."DOB"))>45 and  extract(year from age(def."DOB"))<51 then \'46-50\'
#                     when extract(year from age(def."DOB"))>50 and  extract(year from age(def."DOB"))<56 then \'51-55\'
#                     when extract(year from age(def."DOB"))>55 and  extract(year from age(def."DOB"))<61 then \'56-60\'
#                     else \'over 60\' END) END as agegroup
#                     
#                     from 
#                     cases c 
#                     inner join dscr_defendants def on c.case_number=def.case_number 
#                     where 1=1 
#                     group by race, sex, agegroup, filing_yr
#                     ')
# # date_trunc(\'month\', c."filing_date") + interval \'1 day\' as filing_mo,




plot.age<-function(d, y, subtitle){ 
  title<-paste("District Criminal Cases filed", y )
  limit<-max(d$Count, na.rm=T)+100
  age.plot<-ggplot(data=filter(d,sex=="M"), aes(x=age,y=Count,fill="#006666")) + #ylim(-limit,limit)+
    xlim(0,100)+
    geom_col(position  = "identity") +
    geom_col(data=filter(d,sex=="F"), aes(x=age,y=Count*-1,fill="#Cc6666")) +
    coord_flip()+ 
    theme(plot.margin=unit(c(1,1.2,1,1.2),"cm"))+scale_fill_manual(name=NULL,guide="legend",
                                                                   values =c("#006666","#Cc6666"), labels =  c("Male","Female")) +
    labs(title=title, subtitle=subtitle,y="Count", x="Age")+scale_y_continuous(limits=c(-limit,limit),labels = function(x){as.integer(abs(x))})
  ggsave(filename=paste0(outfolder, title," ",subtitle,".png"), plot=age.plot)
}
age.plots<-function(y){
  
  base=filter(sexage, filing.year==y & age!="" & !is.na(age) & Count!=0 & !is.na(Count))
  black=filter(base, race=="BLACK, AFRICAN AMERICAN")
  white=filter(base, race=="WHITE, CAUCASIAN, ASIATIC INDIAN, ARAB")
  
  
  plot.age(black,y,
           "Black Defendant")
  plot.age(white,y,
           "White Defendant")
}

lapply(unique(sexage$filing.year), age.plots)