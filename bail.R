
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

# query colnames
db.colnames<-dbGetQuery(con, 
                        statement ="
           SELECT
           TABLE_NAME,
           COLUMN_NAME
           FROM
           INFORMATION_SCHEMA.COLUMNS
           ")
events<-dbGetQuery(con, 
                        statement ="
           SELECT *
           FROM
           dsk8_events
           where comment like \'%BAIL%\'
          
           and date>\'2015-06-30\'")
#################
# Bail and Bond #
#################
# bailhead <- dbGetQuery(con, statement = '
# 
#                    select * from dsk8_bail_and_bond limit 10')
dsk8.source<-dbGetQuery(con, statement = '
   
                   select
         c.filing_date,
         c.court,
         ch.description description,
                        ch.disposition,
                        ch.cjis_traffic_code cjis_code,
                        ch.plea,
                        ch.sentence_years jail_term_years,
                        ch.sentence_months jail_term_months,
                        ch.sentence_days jail_term_days,
                        nch.charges,

         bb.*
                   from
                   cases c
                   inner join dsk8  on dsk8.case_number=c.case_number
                   inner join dsk8_bail_and_bond bb on bb.case_number=c.case_number
                   left join dsk8_charges ch on ch.case_number=c.case_number and ch.charge_number=1
                   left join (select case_number, count(*) charges from dsk8_charges group by case_number) nch on nch.case_number=c.case_number

where 1=1 and c.filing_date>\'2015-06-30\'
                   ')

dscr.source<-dbGetQuery(con, statement = '
                   select
         c.filing_date,
         c.court,
         ch.charge_description description,
                        ch.disposition,
                        ch.cjis_code,
                        ch.plea,
                        ch.jail_term_years,
                        ch.jail_term_months,
                        ch.jail_term_days,
                        nch.charges,

bb.date set_date,

         bb.*
         
                   from
                   cases c
                   inner join dscr  on dscr.case_number=c.case_number
                   inner join dscr_bail_events bb on bb.case_number=c.case_number
                   left join dscr_charges ch on ch.case_number=c.case_number and ch.charge_number=1
                   left join (select case_number, count(*) charges from dscr_charges group by case_number) nch on nch.case_number=c.case_number
where 1=1 
and c.filing_date>\'2015-06-30\'
                   ')

# charges <- dbGetQuery(con, statement = '
#                    select * from dscr_charges order by case_number')
#                    
# case.charges <- charges %>%
#   group_by(case_number) %>%
#   summarise(n.charges=n())

demo.source<-dbGetQuery(con, statement = '
                   select 
                   c.case_number,
                   sex, race, "DOB" 
                   from
                   cases c 
                   inner join dscr_defendants def on c.case_number=def.case_number 
                   where 1=1 
                   and c.filing_date>\'2015-06-30\'
                        ') %>% unique()

cases <- rbind(select(dscr.source,
                    case_number,
                    filing_date,
                    court,
                    set_date,
                    bail_amount,
                    description,
                    cjis_code,
                    charges,
                    release_date,
                    set_date),
               select(dsk8.source,
                      case_number,
                      filing_date,
                      court,
                      set_date,
                      bail_amount,
                      description,
                      cjis_code,
                      charges,
                      release_date,
                      set_date)) %>% unique() %>%
  arrange(case_number,
          set_date) %>%
  filter(!duplicated(case_number)) %>%
left_join(., demo.source) %>%
  mutate(race=factor(race, levels = c("WHITE, CAUCASIAN, ASIATIC INDIAN, ARAB" ,       
                     "BLACK, AFRICAN AMERICAN"   ,                    
                     "AMERICAN INDIAN, ALASKA NATIVE"          ,      
                     "UNKNOWN, OTHER"                           ,     
                     "ASIAN, NATIVE HAWAIIAN, OTHER PACIFIC ISLANDER", NA))
) 

sent <-
  read.csv("C:/OJB/sentencing_guidelines_2018.csv",
         stringsAsFactors = F,
         strip.white = T) %>%
  mutate(cjis_code=gsub("-"," ",cjis_code))

cases.sent <- left_join(cases,sent)

unique(filter(cases.sent, is.na(seriousness))$cjis_code)

source.bond<- left_join(cases.sent, source_data[,c("case_number", 
                                                   "ASSISTANT STATES ATTORNEY", 
                                                   "SPECIAL PROSECUTING ATTORNEY", 
                                                   "WITNESS/POLICE OFFICER", 
                                                   "COMPLAINANT/POLICE OFFICER", 
                                                   "BAILBONDSMAN", 
                                                   "charge_description", 
                                                   "disposition", 
                                                   "plea", 
                                                   "jail_term_years", 
                                                   "jail_term_months", 
                                                   "jail_term_days")]) %>%
  mutate(cjis_code=as.factor(cjis_code),
         month=floor_date(filing_date,"month"),
         year=year(filing_date),
         implementation=case_when(set_date<as.Date("2017-07-01")~"Before Rule Change",
                                  set_date>as.Date("2017-06-30")~"After Rule Change",
                                  TRUE~NA_character_))

summarize.bond <- function(...){
   source.bond %>%
  group_by(...) %>%
  dplyr::summarise(bond=sum(bail_amount,na.rm=T),
            cases=n(),
            bonded.cases=sum(bail_amount!=0),
            sentenced_jail=sum(!is.na(jail_term_days) & jail_term_days!=0),
            jail_term_days=sum(jail_term_days, na.rm=T)) %>%
  mutate(pct.bonded=bonded.cases/cases,
         av.bond=bond/bonded.cases,
         pct.sentenced=sentenced_jail/cases,
         av.term=jail_term_days/sentenced_jail)
}

s <- summarize.bond(month,
                      race,
                      seriousness)

bond.summary.year <- summarize.bond(year,
                                    race)

bond.summary.month <- summarize.bond(month,
                                    race)

lapply(unique(bond.summary.year$year),function(m){
  df<-filter(bond.summary.year,year==m)
  bonds<-df$bond
  names(bonds) <- df$race
  names(bonds)[bonds/sum(bonds,na.rm=T)<.01] =""
  pie(bonds[order(bonds)],main = m)})

ggplot(data=filter(source.bond,bail_amount!=0 & !is.na(bail_amount) & !is.na(fel_mis)),aes(x=fel_mis,y=bail_amount,fill=factor(implementation,levels=c("Before Rule Change", "After Rule Change")))) +
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge") + scale_y_continuous(label = scales::dollar) + labs(y="Bail Amount", x="", title = "Average Bail Amount at Initial Bail Hearing", fill="")


# n.cases <- bond.summary$cases
# 
# names(n.cases) <- bond.summary$race
# bonds<-bond.summary$bond
# names(bonds) <- bond.summary$race
# bonded<-bond.summary$bonded.cases
# names(bonded) <- bond.summary$race

# pie(bonds[order(bonds)])
# pie(bonded[order(bonded)])
# 
# pie(n.cases[order(n.cases)])

bail.set.reg <- lm(data = cases[cases$bail_amount!=0,],bail_amount~race*sex)





bond.reg<-
lm(data = source.bond[source.bond$bail_amount!=0,],
   bail_amount~
     race*sex 
   +
     `ASSISTANT STATES ATTORNEY`
   +
     seriousness)

term.reg<-
  lm(data = source.bond,
     jail_term_days~
       race*sex +
       `ASSISTANT STATES ATTORNEY` +
       seriousness
  )
term.reg.co <- (summary(term.reg)$coefficients)
rows <- row.names(term.reg.co)

term.reg.co <- as.data.frame(term.reg.co) %>%
  arrange(desc(abs(`t value`)))

