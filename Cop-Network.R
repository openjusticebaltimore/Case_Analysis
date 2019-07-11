library(tidyverse)
library(lubridate)
library(DBI)
library(RPostgreSQL)
library(igraph)
library(ggraph)
# install.packages("tidygraph")
 library(tidygraph)
# library(reshape2)
``
pwd <- ""
# create an PostgreSQL instance and create one connection.
drv <- dbDriver("PostgreSQL")

# open the connection using user, passsword, etc., as
con <- dbConnect(drv, dbname = "mjcs",
                 user="mjcs_ro",
                 password=pwd,
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

cops.base <- cops.cops %>%
  dplyr::group_by(name) %>%
  dplyr::summarise(count=n()) %>%
  dplyr::filter(count>100)
cops.base <- unique(cops.base$name)

cop.relations <- left_join(dplyr::rename(cops.cops,name1=name) ,dplyr::rename(cops.cops,name2=name)) %>%
  filter(name1!=name2)  %>%
  filter(name1 %in% cops.base | name2 %in% cops.base )%>% filter(!is.na(name1) & !is.na(name2))
cops.base2 <- c(cop.relations$name1,cop.relations$name2)
  

# + 
#   geom_edge_link() + 
#   geom_node_point(size = 8, colour = 'steelblue') +
#   geom_node_text(aes(label = name), colour = 'white', vjust = 0.4) + 
#   theme_graph()

cop.network <- graph_from_data_frame(d=cop.relations[,c("name1","name2")], vertices=unique(cops.base2), directed=F)
tiff("relations.tiff",width=10000,height=10000)
plot(cop.network,vertex.size=.1,label.cex=.1)
dev.off()

cop.graph <- cop.relations %>% 
  tbl_graph() %>%
  ggraph(layout = 'kk') 

ggraph(cop.relations) + 
  geom_edge_density(aes(fill = weight)) #+
  geom_edge_link(aes(width = weight), alpha = 0.2) + 
  geom_node_point(aes(color = factor(group)), size = 10) +
  geom_node_text(aes(label = name), size = 8, repel = TRUE) +
  scale_color_brewer(palette = "Set1") +
  theme_graph() +
  labs(title = "Cop Network",
       subtitle = "Nodes are colored by group")
