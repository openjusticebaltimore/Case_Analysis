#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
result <- function(pw,ln,
                   fn,
                   sqn){
 
  drv <- dbDriver("PostgreSQL")
  
  # open the connection using user, passsword, etc., as
  con <- dbConnect(drv, dbname = "mjcs",
                   user="mjcs_ro",
                   password=pw,
                   port=5432,
                   host="mjcs.c7q0zmxhx4uo.us-east-1.rds.amazonaws.com")
  name<-paste(trimws(toupper(ln)),
              trimws(toupper(fn)),
              sep=", ")
  
  qry.cases<-paste0('SELECT
              c.case_number,
              c."filing_date"
              FROM
              cases c
              inner join dscr_related_persons r on c.case_number=r.case_number
              WHERE 1=1 
              and
              connection LIKE \'%POLICE OFFICER%\' 
              and (name LIKE \'%',name,'%\' or officer_id = \'',trimws(toupper(sqn)),'\')
              order by c."filing_date" desc
                    LIMIT 100')
  tabl<-dbGetQuery(con,qry.cases) 
  cases    <- paste("<b>Case Numbers</b><br/>",paste(unique(tabl$case_number),collapse='<br/>')  )
  names(tabl)<-c("Case Number", "Filing Date")
  tabley <- kable(tabl) %>% kable_styling() %>% scroll_box(width = "100%", height = "200px")
  return(tabley)
}
result2 <- function(pw,ln,
                   fn,
                   sqn){
  
  drv <- dbDriver("PostgreSQL")
  
  # open the connection using user, passsword, etc., as
  con <- dbConnect(drv, dbname = "mjcs",
                   user="mjcs_ro",
                   password=pw,
                   port=5432,
                   host="mjcs.c7q0zmxhx4uo.us-east-1.rds.amazonaws.com")
  name<-paste(trimws(toupper(ln)),
              trimws(toupper(fn)),
              sep=", ")

qry.name<-paste0('SELECT distinct name,
                    officer_id
                    FROM dscr_related_persons 
                    WHERE 1=1 
                    and
                    connection LIKE \'%POLICE OFFICER%\' 
                    and (name LIKE \'%',name,'%\' or officer_id = \'',trimws(toupper(sqn)),'\')
                    
                    LIMIT 100')

officer<-dbGetQuery(con,qry.name)
SEQ.Ns<-paste(unique(officer$officer_id),collapse = ", ")
NAMEs<-paste(unique(officer$name),collapse = ", ")
return(paste(NAMEs,SEQ.Ns,sep="\n"))
}
library(shiny)
library(tidyverse)
library(kableExtra)
require("lubridate")

# install.packages("DBI")
# install.packages("RPostgreSQL")
require("DBI")
require("RPostgreSQL")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Officer Search"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      # sidebarPanel(
      #    radioButtons("searchtype",
      #                "Search Type:"
      #                ), 
     # choices=c("Name","Sequence Number")
      # ),
     sidebarPanel(
       textInput("pw",
                 "Password:"),
        textInput("fname",
                     "First Name:"
        ),
        textInput("lname",
                  "Last Name:"
        ),
        textInput("seqn",
                  "Sequence Number:"
        ),
        actionButton("act",label="Submit")
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(textOutput("results2"),
        htmlOutput("results")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  res <- eventReactive(input$act,{  
  result(input$pw,input$lname,input$fname,input$seqn)})
  res2 <- eventReactive(input$act,{  
    result2(input$pw,input$lname,input$fname,input$seqn)})
  
     output$results <-renderUI(HTML(res())) 
   output$results2 <-renderText(res2()) 

}

# Run the application 
shinyApp(ui = ui, server = server)

