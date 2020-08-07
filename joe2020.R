#Job market 2020 (Jan,1-July,31) info
## Reads XLS files downloaded from JOE website. 
## Based on code by Tyler Ransom  (https://github.com/tyleransom/econJobMarket)

## Clear out memory
rm(list = ls())
gc()

## Load required packages ----

rr<-c("qdap","tidyverse", "stringr", "zoo","tibble",
      "readxl", "countrycode", "gdata",  "rvest",
      "stringr", "data.table", "sqldf", "plyr",
      "ggplot2", "mgsub", "svDialogs")
for (package in rr){
  if(package %in% rownames(installed.packages()) == FALSE) {
    install.packages(package)}
}

lapply(rr, require, character.only=T)
options(stringsAsFactors=FALSE)


# Ask user for an input ----
# Working Directory
my.path <- dlg_input(message="Enter working directory folder: ", default=getwd())$res
setwd(my.path)
#setwd("C:/Users/Mara/Downloads/jobmarket")
my.file <- dlg_input(message="Enter filename with JOE data: ", default=list.files(pattern="(joe)(.*)(xls)"))$res

# want.scrape
#wantScrape <- as.logical(readline(prompt="Do you want to scrape JOE? T/F: "))
wantScrape <- as.logical(dlg_input(message="Scrape JOE? T/F: ", default=F)$res)

# Decide on interactive mode ----
want.interactiveselection<-dlg_input(message="Continue with interactive mode? Y/N: ", default="Y")$res
if (want.interactiveselection=="N"){
  warning("Supply your lists of countries, fields, subset options")
  my.countries<-"USA"
  my.states<-"CA"
  my.fields<-NA
  want.agricultural<-F
  want.medschool<-F
  want.permanent<-T
  
  #exclusion lists
  my.institutions<-NA
  my.depts<-NA
  my.divisions<-NA
  
  }else {

## what countries----
my.countries<- str_split(dlg_input(message="Enter 3-letter country codes (default is USA): ", default="USA")$res, pattern=" ")[[1]]

## what states ----
want.specific_states<-dlg_input(message="Subset by state? Y/N: ", default="N")$res
if (want.specific_states=="Y"){
my.states<-str_split(dlg_input(message="Enter 2-letter state codes (default is CA): ", default="CA")$res, pattern=" ")[[1]]
}else {
  my.states<-NA
}
## what fields ----
want.specific_fields<-dlg_input(message="Subset by field? Y/N: ", default="N")$res
if (want.specific_fields=="Y"){
my.fields <- str_split(dlg_input(message="Enter fields: ", default="00 -")$res, pattern=" ")[[1]]
}else {
  my.fields<-NA
}
## include only permanent jobs ----
want.permanent<-as.logical(dlg_input(message="Include only permanent positions? T/F: ", default=T)$res)

# include agricultural econ ----
want.agricultural <- as.logical(dlg_input(message="Include positions in Agricultural Economics? T/F: ", default=F)$res)

## include medical schools ----
want.medschool <- as.logical(dlg_input(message="Include positions in medical and health schools? T/F: ", default=F)$res)

## exclude institutions ----
want.drop_institution<-dlg_input(message="Exclude institution? Y/N: ", default="N")$res
if (want.drop_institution=="Y"){
my.institutions <- str_split(dlg_input(message="Enter institution name: ", default="")$res, pattern=" ")[[1]]
}else {
  my.institutions<-NA
}

##  exclude departments----
want.drop_depts<-dlg_input(message="Exclude department? Y/N: ", default="N")$res
if (want.drop_depts=="Y"){
my.depts <- str_split(dlg_input(message="Enter department name: ", default="")$res, pattern=" ")[[1]]
}else {
  my.depts<-NA
}

## exclude divisions ----
want.drop_divisions<-dlg_input(message="Exclude division? Y/N: ", default="N")$res
if (want.drop_divisions=="Y"){
my.divisions <- str_split(dlg_input(message="Enter divison name: ", default="")$res, pattern=" ")[[1]]
}else {
  my.divisions<-NA
}
}
# Function to scrape JOE application requirements/instructions -----
scrapeJOE <- function(url){
  temp <- read_html(url)
  out  <- temp %>% html_nodes("br+ .dialog_text > div") %>% html_text()
  return(out)
} 

# Function to clean XLS file ----
CleanJOE<-function(rawfile, want.scrape=wantScrape) { 

## Clean XLS file from JOE website
## rawfile: provide filename of XLS file from JOE
  # if file is not in working directory, include path to the file
## want.scrapeDoes the user want to invoke scraping?


## Convert downloaded JOE file from XLS to CSV (easier to turn into R data table)
#works fine with excel
#warning("Make sure you change the date on line 42 of this script!")
#warning("Note: You may have to manually delete certain listings because of unreadable characters")
#rawfile <- "joe_resultset_2019_2020.xls"
#library(readxl)
JOBS <- read_xls(rawfile,  sheet=1) #,quote = ""
print(dim(JOBS))

## Manually delete problematic listings (that for some reason have characters that cause the parsing to fail)
#flag <- (JOBS$jp_institution=="Federal Reserve Bank of San Francisco") | (JOBS$jp_institution=="St. Norbert College") | (is.na(JOBS$jp_id))
#JOBS <- JOBS[!flag,]
#print(dim(JOBS))

## If deadline is missing --> put 11/01/2016
JOBS$Application_deadline[JOBS$Application_deadline==""] <- "2020-11-01"
JOBS$Application_deadline <- as.POSIXct(JOBS$Application_deadline)

## Split out the country
JOBS$domestic <- grepl("UNITED STATES",JOBS$locations)
JOBS$country  <- gsub("([A-Z]*)( [A-Z]{2,})?(.*)","\\1\\2", JOBS$locations)
#country with 3 words
JOBS$country[grepl("UNITED ARAB EMIRATES",JOBS$locations)]<-"UNITED ARAB EMIRATES"
## Get harmonized country codes
JOBS$iso3 <- countrycode(JOBS$country, "country.name", "iso3c", warn = FALSE)

## GEt state and city
JOBS$locations[is.na(JOBS$locations)]<-""

JOBS$usstate<-mgsub(JOBS$locations, pattern=JOBS$country,
                  replacement="", recycle=T,
                  ignore.case=TRUE, fixed=FALSE)

JOBS$usstate<-trimws(mgsub(JOBS$usstate, pattern=state.name,
                  replacement=state.abb, recycle=T,
                  ignore.case=TRUE, fixed=FALSE))
JOBS$uscity<-trimws(word(JOBS$usstate,2))
JOBS$uscity[JOBS$iso3!="USA"]<-""
JOBS$uscity[grepl("District of Columbia", JOBS$locations)]<-"Washington DC"


JOBS$usstate<-trimws(word(JOBS$usstate,1))
JOBS$usstate[JOBS$iso3!="USA"]<-""
JOBS$usstate[grepl("District of Columbia", JOBS$locations)]<-"DC"


#Convert full text field to character
JOBS$jp_full_text <- as.character(JOBS$jp_full_text)

## Create "other date" field which is any other Oct, Nov, or Dec date mentioned in the full text
JOBS$otherdate <- ""
whichare <- regexpr("(October|November|December) ([0-9]{1,2})(,)? (2020)?",JOBS$jp_full_text, perl=TRUE, useBytes=TRUE)
JOBS$otherdate[whichare[1:nrow(JOBS)]!=-1] <- regmatches(JOBS$jp_full_text,whichare)
whichare <- regexpr("([0-9]{1,2}) (October|November|December)(,)? (2020)?",JOBS$jp_full_text, perl=TRUE, useBytes=TRUE)
JOBS$otherdate[whichare[1:nrow(JOBS)]!=-1] <- regmatches(JOBS$jp_full_text,whichare)
whichare <- regexpr("([0-9\\.\\/]{1,+})([1-9]\\.\\/]{1,+})(2020)?",JOBS$jp_full_text, perl=TRUE, useBytes=TRUE)
JOBS$otherdate[whichare[1:nrow(JOBS)]!=-1] <- regmatches(JOBS$jp_full_text,whichare)

## Add the JOB LISTING URL
JOBS$url <- paste("https://www.aeaweb.org/joe/listing.php?JOE_ID=",JOBS$joe_issue_ID,"_",JOBS$jp_id,sep="")


if (want.scrape==TRUE) {
  ## Scrape application instructions from job listing URL
  JOBS <- cbind(JOBS,matrix("",dim(JOBS)[1],1))
  colnames(JOBS)[length(JOBS)] <- "instructions"
  for (i in 1:dim(JOBS)[1]) {
    JOBS$instructions[i] <- scrapeJOE(JOBS$url[i])
  }

  JOBS$instructions <- gsub("\\n\\n\\t\\tApplication Requirements","Application Requirements: " ,JOBS$instructions)
  JOBS$instructions <- gsub("\\t\\n\\t"                           ,""                           ,JOBS$instructions)
  JOBS$instructions <- gsub("\\r\\n"                              ," "                          ,JOBS$instructions)
  JOBS$instructions <- gsub("Instructions Below"                  ,"Instructions Below. : "     ,JOBS$instructions)
  JOBS$instructions <- gsub("Application Instructions"            ," Application Instructions: ",JOBS$instructions)
  
  ## Generate the application system (i.e. JOE, EJM, AJO, native institution website, email, etc.)
  url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  
  JOBS$ApplicationURL <- ""
  whichare <- regexpr(url_pattern,JOBS$jp_full_text, perl=TRUE, useBytes=TRUE)
  JOBS$ApplicationURL[whichare[1:nrow(JOBS)]!=-1] <- regmatches(JOBS$jp_full_text,whichare)
  
  JOBS$EJM<-as.integer(grepl("econjobmarket\\.org"     ,JOBS$ApplicationURL,ignore.case=TRUE) | grepl("econjobmarket\\.org"     ,JOBS$instructions,ignore.case=TRUE))
  JOBS$JOE<-as.integer(grepl("aeaweb\\.org"            ,JOBS$ApplicationURL,ignore.case=TRUE) | grepl("aeaweb\\.org"            ,JOBS$instructions,ignore.case=TRUE))
  JOBS$AJO<-as.integer(grepl("academicjobsonline\\.org",JOBS$ApplicationURL,ignore.case=TRUE) | grepl("academicjobsonline\\.org",JOBS$instructions,ignore.case=TRUE))
  JOBS$Electronic<-as.integer(grepl("email",JOBS$instructions, ignore.case=TRUE))
  JOBS$Interfolio<-1 - EJM - JOE - AJO - Electronic 
  JOBS[ , Other := (AJO | Electronic) ]
}

## Extract e-mail address for inquiries
email_pattern <- "([_a-z0-9-]+(\\.[_a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,4}))"
JOBS$InquiryEmail <- ""
whichare <- regexpr(email_pattern,JOBS$jp_full_text, perl=TRUE, useBytes=TRUE)
JOBS$InquiryEmail[whichare[1:nrow(JOBS)]!=-1] <- regmatches(JOBS$jp_full_text,whichare)

## Remove year and comma from "other date" 
## What for?
JOBS$otherdate_noyear[grepl(", 2020",JOBS$otherdate)]<-gsub(", 2020","",JOBS$otherdate)
JOBS$otherdate_noyear[grepl(" 2020",JOBS$otherdate)] <- gsub(" 2020","",JOBS$otherdate_noyear)
JOBS$otherdate_noyear[grepl(", ",JOBS$otherdate) ]<-gsub(", ","",JOBS$otherdate_noyear)

## Convert "other date" to POSIX format
JOBS$otherdate_day<-as.numeric(gsub("[^\\d]+", "", JOBS$otherdate_noyear, perl=TRUE))
JOBS$otherdate_month<-""
JOBS$otherdate_month[ grepl("November",JOBS$otherdate_noyear)]<-11
JOBS$otherdate_month[ grepl("October",JOBS$otherdate_noyear)]<- 10
JOBS$otherdate_month[ grepl("December",JOBS$otherdate_noyear)]<-12

JOBS$otherdate_asDate_ready[is.na(JOBS$otherdate_month)==F & is.na(JOBS$otherdate_day)==F] <- paste0("2020-",JOBS$otherdate_month,"-",JOBS$otherdate_day)
JOBS$deadline_other <-as.Date(JOBS$otherdate_asDate_ready)

## Generate "effective" deadline
JOBS$true_deadline<-as.Date(JOBS$Application_deadline)
JOBS$true_deadline[ is.na(JOBS$deadline_other)==F]<- as.Date(JOBS$deadline_other)
#JOBS <- JOBS[,order(JOBS$true_deadline)]


return(JOBS)
}

# Function to filter JOE vacancies -----

JobMatches<-function (file,CountryCode=my.countries, State=my.states,
                    permanent=want.permanent, field=my.fields,
                    drop_senior_role=T,
                    drop_medicalschool=!want.medschool,
                    drop_agecon=!want.agricultural,
                    drop_institution=my.institutions,
                    drop_department=my.depts,
                    drop_division=my.divisions) {
  
JOBS<-file

## User-defined subset parameters 
## CountryCode: accept vector of country iso3 codes, e.g. c("KOR", "GBR", "CAN")  
  #             default is "USA"
## State: accept vector of US states abbreviations, e.g. c("CA", "AL")  
  #        default is NA
## permanent: default is TRUE, subsets only to permanent positions; 
  #          if FALSE, subset includes all position types
## field: accept a vector of JEL codes or names of fields
  # e.g. c("Labor", "microeconomics") or c("B10", "J5", "J -")
  #      default is NA, all fields are included
## drop_senior_role: default is TRUE, 
  #drops positions not suitable for the fresh PhDs 
  #(e.g. "Director", "Staff Fellow")  
## drop_medicalschool: default is TRUE
  # drops positions in medical or health institutions
##drop_agecon: default is TRUE
  #drops departments/ divisions related to Agricultural Economics
##drop_institution, drop_department, drop_division: defaulr is NA
  # drops user-specified companines (e.g. "Amazon")
  #divisions or departments (e.g. "Mathematics")

## Geography (countries) ----
#("USA","CAN","MEX","ESP")
JOBS <- JOBS[is.element(JOBS$iso3,CountryCode)==T, ]
if (!is.na(State)){
  JOBS <- JOBS[is.element(JOBS$usstate,State)==T, ]
}
## Keep only permanent jobs ----
if (permanent==T){
JOBS$ft <- grepl("Full-Time", JOBS$jp_section)
}
else{
  #include permanent and temporary
  JOBS$ft<-T
}

## Keep job listings only for a specific set of JEL codes ----
if (is.na(field)!=T){
toMatch <- field
JOBS$myfields <- grepl("Any field",JOBS$jp_full_text, ignore.case=TRUE, useBytes=TRUE) | 
  grepl(paste(toMatch, collapse="|"),JOBS$JEL_Classifications, ignore.case=TRUE, perl=TRUE, useBytes=TRUE)
}
else{
  JOBS$myfields<-T
}

## Drop listings in medical schools or public health schools ----
JOBS$positionsToKeep[grepl(paste(c("Public Health",
                                   "School of Medicine"), collapse="|"),JOBS$jp_division, perl=TRUE, useBytes=TRUE)] <- !drop_medicalschool


## Drop Ag Econ jobs ----
JOBS$positionsToKeep[grepl(("Agricultural"),
                           JOBS$jp_title, 
                           perl=TRUE, useBytes=TRUE)] <- !drop_agecon


## Keep only eligible tenure-track jobs for junior candidates ---- (i.e. no Deans, Dept Chairs, etc.)
JOBS$positionsToKeep <- !grepl(paste(c("Dean","Chair","Visiting","Professorship",
                                       "Professor of the Practice","Research Assistant",
                                       "Lecturer","Staff Fellow","Department Head of Economics",
                                       "Director of School of Public Policy", "Director"), collapse="|"), JOBS$jp_title, perl=TRUE, useBytes=TRUE)*drop_senior_role

## Drop jobs for unqualified positions for first-time job marketeers ----
#(note the leading/trailing spaces for some of these)
JOBS$positionsToKeep[JOBS$jp_title=="Associate or Professor of Economics "                                                       ] <- !drop_senior_role
JOBS$positionsToKeep[JOBS$jp_title=="Associate or Professor of Economics"                                                        ] <- !drop_senior_role
JOBS$positionsToKeep[JOBS$jp_title=="Associate or Full Professor of Economics "                                                  ] <- !drop_senior_role
JOBS$positionsToKeep[JOBS$jp_title=="Associate or Full Professor of Economics"                                                   ] <- !drop_senior_role
JOBS$positionsToKeep[JOBS$jp_title=="Associate or Full Professor "                                                               ] <- !drop_senior_role
JOBS$positionsToKeep[JOBS$jp_title=="Associate or Full Professor"                                                                ] <- !drop_senior_role
JOBS$positionsToKeep[JOBS$jp_title=="Associate or Professor "                                                                    ] <- !drop_senior_role
JOBS$positionsToKeep[JOBS$jp_title=="Associate Professor"                                                                        ] <- !drop_senior_role
JOBS$positionsToKeep[JOBS$jp_title=="Associate or Professor"                                                                     ] <- !drop_senior_role
JOBS$positionsToKeep[JOBS$jp_title=="Associate Professor, Economics"                                                             ] <- !drop_senior_role
JOBS$positionsToKeep[JOBS$jp_title=="Associate Professor / Professor"                                                            ] <- !drop_senior_role
JOBS$positionsToKeep[JOBS$jp_title=="Associate Professor/ Professor"                                                             ] <- !drop_senior_role
JOBS$positionsToKeep[JOBS$jp_title=="Associate Professor or Professor"                                                           ] <- !drop_senior_role
JOBS$positionsToKeep[JOBS$jp_title=="Associate Professor or Professor "                                                          ] <- !drop_senior_role
JOBS$positionsToKeep[JOBS$jp_title=="Professor and Department Head"                                                              ] <- !drop_senior_role
JOBS$positionsToKeep[JOBS$jp_title=="Director of Undergraduate Studies"                                                          ] <- !drop_senior_role
JOBS$positionsToKeep[JOBS$jp_title=="Professor "                                                                                 ] <- !drop_senior_role
JOBS$positionsToKeep[JOBS$jp_title=="Professor"                                                                                  ] <- !drop_senior_role
JOBS$positionsToKeep[JOBS$jp_title=="Clinical Assistant Professor"                                                               ] <- !drop_senior_role
JOBS$positionsToKeep[JOBS$jp_title=="Director, School of Public Policy"                                                          ] <- !drop_senior_role
JOBS$positionsToKeep[JOBS$jp_title=="Professor of Public Policy and Economics"                                                   ] <- !drop_senior_role


## Drop other jobs that aren't a good fit  ----
#(these could be certain institutions, certain departments, certain divisions, etc.)

#for (var in c('drop_institution', 'drop_department', 'drop_division')){
if (is.na(drop_institution)!=T){
JOBS$positionsToKeep[grepl(paste(drop_institution, collapse="|"),JOBS$jp_institution, perl=TRUE, useBytes=TRUE)                  ] <- FALSE
}

if (is.na(drop_department)!=T){
JOBS$positionsToKeep[grepl(paste(drop_department, collapse="|"),JOBS$jp_department, perl=TRUE, useBytes=TRUE)                    ] <- FALSE                                                 
}

if (is.na(drop_division)!=T){
JOBS$positionsToKeep[grepl(paste(drop_division, collapse="|"),JOBS$jp_division, perl=TRUE, useBytes=TRUE)                        ] <- FALSE
}

## Subset  ----
JOBS_fit <- JOBS[JOBS$ft==TRUE & JOBS$myfields==TRUE & JOBS$positionsToKeep==TRUE, ]

return(JOBS_fit)
}

# Use functions on your file with your preferences -----
#ff<-CleanJOE(rawfile="joe_resultset_2019_2020.xls")
ff<-CleanJOE(rawfile=my.file)

fit<-JobMatches(file=ff)

print(dim(ff))
print(dim(fit))


## Keep only most important columns (for the "clean" CSV)
JOBSabbrev <- fit[,c("jp_id","jp_institution","jp_keywords","url","true_deadline"), with=FALSE]

# Output the files ----
write.csv(ff   , paste0('JOE_jobs_', Sys.Date(), '_all.csv'))
write.csv(fit    , paste0('JOE_jobs_', Sys.Date(), '_fit.csv'))
write.csv(JOBSabbrev, paste0('JOE_jobs_', Sys.Date(), '_fitclean.csv'))









###                                                      Marina Lovchikova UC Davis 2020