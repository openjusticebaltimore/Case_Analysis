# This reads and cleans the offense tables from this pdf

offense.url <- "http://www.msccsp.org/Files/Guidelines/offensetable.pdf"
# You might want to set the library path
# .libPaths("C:/R/R-3.5.0/library")

library("tabulizer")
library("dplyr")

# get the tables with tabulizer
out <- extract_tables(offense.url)

# These will be the column names
tabnames <- 
  c("COMAR",
  "offense_literal",
  "cjis_code",
  "source",
  "fel_mis",
  "max_term",
  "min_term",
  "type",
  "seriousness",
  "fine")

# function to reorder the columns, including the ones that haven't been names yet. This will come in handy in the following function
taborder <- function(df){
  df[,c(tabnames[tabnames %in% names(df)], names(df)[!names(df)%in% tabnames])]
}

# Tabulizer does an ok job, but doesn't always get the number of columns right. So we need a function to
# get all the information where it's supposed to be. Some character string data might need to be taken out of one column and put in another.
# There's one table per page. Some of the pages don't really have tables, so we'll leave those out.
fixtab <- 
function(x){
  # make the table a dataframe
df<-tab.pages[x]
df <- as.data.frame(df[[1]])
df <- df %>% mutate_if(is.factor,as.character)

# name some columns where there's not likely to be anything amiss
names(df)[1]<-"COMAR"
names(df)[2]<-"offense_literal"

# If there are cjis codes in the offense column, find them and get them out. Otherwise name the existing cjis column.
if(grepl("[0-9]-[0-9]{4}",df$offense_literal)>0){
  df$cjis_code <- NA
  df$cjis_code[grepl("[0-9]-[0-9]{4}",df$offense_literal)] <- regmatches(df$offense_literal, regexpr("[0-9]-[0-9]{4}.*$",df$offense_literal))
  df$offense_literal <- gsub("[0-9]-[0-9]{4}.*$","",df$offense_literal)
  }else{
names(df)[3]<-"cjis_code"
  }

# If there are source codes in the cjis column, get them out of there. Otherwise name the existing source column
if(sum(grepl("§",df$cjis_code))>0){
  some.source <- trimws(gsub("[0-9]-[0-9]{4}"," ",df$cjis_code))
  df$cjis_code <- gsub(" .*$","",df$cjis_code)
  sourceind <- colSums(apply(df,2,function(x)grepl("§",x)))
  if(sum(sourceind)>0 & 
     which(names(df)==names(sourceind[rev(order(sourceind))][1]))>3){
  names(df)[which(names(df)==names(sourceind[rev(order(sourceind))][1]))] <- "source"
  df$source <- trimws(df$source)
  df$source <- ifelse(df$source=="",NA, df$source)
  df$source <- coalesce(df$source, some.source)}else{
    df$source <- some.source}
  }else{
  names(df)[4]<-"source"  
}

df$cjis_code <- trimws(df$cjis_code)
df$cjis_code[!grepl("^[0-9]-[0-9]{4}$",df$cjis_code)] <- ""

df <- taborder(df)

typeind <- colSums(apply(df,2,function(x)grepl("Felony|Misd\\.",x)))
names(df)[which(names(df)==names(typeind[rev(order(typeind))][1]))] <- "fel_mis"
type.plus <- trimws(gsub("Felony|Misd\\.","",df$fel_mis))

if(sum(nchar(type.plus))>0){
  df$max_term <- type.plus
}else{
  max.ind <- colSums(apply(df,2,function(x)grepl("^[0-9]{1,2}[MY]$",trimws(x))))
  names(df)[which(names(df)==names(max.ind[rev(order(max.ind))][1]))] <- "max_term"
}

df$fel_mis <- trimws(df$fel_mis)
df$fel_mis <- gsub(" .*$","",df$fel_mis)

df <- taborder(df)
names(df)[which(names(df)=="max_term")+1] <- "min_term"

typeind <- colSums(apply(df,2,function(x)grepl("Person|Drug|Property",x)))
names(df)[which(names(df)==names(typeind[rev(order(typeind))][1]))] <- "type"
type.plus <- trimws(gsub("Person|Drug|Property","",df$type))
if(!"min_term" %in% names(df)){
  df$min_term <- ""
}
df <- taborder(df)

serious.ind <- colSums(apply(df,2,function(x)grepl("^[XIVA-C]{1,4}$",trimws(x))))
# names(serious.ind[rev(order(serious.ind))][1])
names(df)[which(names(df)==names(serious.ind[rev(order(serious.ind))][1]))] <- "seriousness"
df <- taborder(df)
fine.ind <- colSums(apply(df,2,function(x)grepl("\\$",trimws(x))))
if(sum(fine.ind>0)){
names(df)[which(names(df)==names(fine.ind[fine.ind>0])[!names(fine.ind[fine.ind>0]) %in% tabnames])] <- "fine"
}else{
  df$fine <- ""
}
df[,tabnames]

}
tab.pages <- out[c(2:13,15:30,32:56)]
fixed <- lapply(1:length(tab.pages), fixtab)
fixed.all <- plyr::rbind.fill(fixed)
fixed.all$COMAR <- ifelse(trimws(fixed.all$COMAR)=="",NA,fixed.all$COMAR)
fixed.all$COMAR <- zoo::na.locf(fixed.all$COMAR)

fixed.all <- fixed.all %>%
  group_by(COMAR) %>%
  summarise_all(function(x)trimws(paste(x,collapse="\n"))) %>% 
  ungroup() %>% as.data.frame() 

split.cjis <- 
  function(n){
    row <- fixed.all[n,]
    new <- as.data.frame(strsplit(row$cjis_code,"\n"))
    names(new) <- c("cjis_code")
    new[,names(row)[names(row)!="cjis_code"]] <- row[,names(row)[names(row)!="cjis_code"]]
    new
    }

new <- plyr::rbind.fill(lapply(1:nrow(fixed.all),split.cjis))
no.cjis <- filter(fixed.all, cjis_code=="")
all <- rbind(new, no.cjis) %>%
  arrange(COMAR,cjis_code)
# long dash
all <- apply(all,2,function(x)gsub("â???"","--",x))
# diamond
all <- apply(all,2,function(x)gsub("âT¦","\\*",x))


write.csv(all,"C:/OJB/sentencing_guidelines_2018.csv",
          row.names = F)

# library(devtools)
# install_github("pdfHarvester", "hansthompson")
# library(pdfHarvester)
# install.packages("tesseract")
# library(tesseract)
# install.packages("magick")
# library(magick)
# data <- ocr(path, engine = tesseract('eng', options = list(tessedit_char_whitelist = '0123456789.-',
#                                                            tessedit_pageseg_mode = 'auto',
#                                                            textord_tabfind_find_tables = '1',
#                                                            textord_tablefind_recognize_tables = '1')))
# cat(data)
