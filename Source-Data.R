# Query to get a wide structured dataset of district criminal cases with defendant demographics and names of related cops, prosecutors, and bail bondsman
# If a case has multiple related persons of the same type, this query agggregates them in a text string separated by semi colons

# List the types of related persons 

# cons <- dbGetQuery(con, statement = '
#                    select 
#                    distinct
# rp.connection
#                    from
#                    cases c 
#                    inner join dscr_defendants def on c.case_number=def.case_number 
#                    left join dscr_related_persons rp on c.case_number=rp.case_number                   
#                    where 1=1 
#                    limit 10')


# dscr_charges <- dbGetQuery(con, statement = '
#                    select *
#                    from
# dscr_charges')



# Here is a SQL query that pulls the wide dataset that shouldn't need much munging to run regressions and other analyses on

source_data<-dbGetQuery(con, statement = "
                   select 
c.case_number,
                   sex, race, filing_date, \"DOB\",
sa.name \"ASSISTANT STATES ATTORNEY\",
pa.name \"SPECIAL PROSECUTING ATTORNEY\",
wpo.name \"WITNESS/POLICE OFFICER\",
cpo.name \"COMPLAINANT/POLICE OFFICER\",\
bb.name \"BAILBONDSMAN\",
ncharges charges,
c.filing_date,
ch.charge_description,
                        ch.disposition,
                        ch.plea,
                        ch.jail_term_years,
                        ch.jail_term_months,
                        ch.jail_term_days

                   from
                   cases c 
                   inner join dscr_defendants def on c.case_number=def.case_number 
                   left join (select case_number, connection, string_agg(distinct name, '; ') as name from dscr_related_persons
where connection='ASSISTANT STATES ATTORNEY' group by case_number, connection) sa on c.case_number=sa.case_number                   
                   left join (select case_number, connection, string_agg(distinct name, '; ') as name from dscr_related_persons
where connection='SPECIAL PROSECUTING ATTORNEY' group by case_number, connection) pa on c.case_number=pa.case_number                   
   left join (select case_number, connection, string_agg(distinct name, '; ') as name from dscr_related_persons
where connection='WITNESS/POLICE OFFICER' group by case_number, connection) wpo on c.case_number=wpo.case_number                   
left join (select case_number, connection, string_agg(distinct name, '; ') as name from dscr_related_persons
where connection='COMPLAINANT/POLICE OFFICER' group by case_number, connection) cpo on c.case_number=cpo.case_number                   
left join (select case_number, connection, string_agg(distinct name, '; ') as name from dscr_related_persons
where connection='BAILBONDSMAN' group by case_number, connection) bb on c.case_number=bb.case_number                   

left join (select case_number, count(*) ncharges from dscr_charges group by case_number) ncharges on ncharges.case_number=c.case_number
left join dscr_charges ch on ch.case_number=c.case_number and ch.charge_number=1 

                where 1=1 
and c.filing_date>'2018-01-01'
and c.filing_date<'2019-01-01'

                   ")


# It may be interesting to write a few simpler queries of the various tables and do the subsetting & joining with dplyr (or base R)
