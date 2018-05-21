
### Packages
library(rvest)
library(dplyr)
library(stringr)
library(qdap)
library(stringdist)
library(parallel)
library(htmltab)
library(edgar)
library(stringi)

### IMPORT BASE FILE TO MATCH (NO BRANCH RECORDS INCLUDED)
### File is used to match SEC names against
### File contains 458,151 records
### generated using ULT PBIN field from bcp001.watch_lst

BASE_FILE = read.csv("BASE_FILe.csv", stringsAsFactors = FALSE)  # read csv file 
##hadlys freed package
#============================================================
#
#     SEC DATA EXTRACTION
#
#============================================================

#--- sec_extract function will pull previous business days annual filings

sec_extract <- function() {

#--- Monday Test, if the day is monday the filings from the previous Friday are pulled
#--- SEC does not take in filings on weekends
  
daily_sec_pull <<- if( weekdays(Sys.Date()) == "Monday" ) {
                      getDailyMaster(Sys.Date()-3)
                  } else { getDailyMaster(Sys.Date()-1) 
                 }

#--- creates daily 10k dataframe
#--- scans all filings and creates a subset of just annual 10k filers
daily_10k <<- subset(daily_sec_pull, FORM_TYPE=='10-K'|FORM_TYPE=='10-K/A')

#--- accension number column is added to daily 10k dataframe
daily_10k$ACCEN_NUM <<- stri_reverse(stri_sub(stri_reverse(daily_10k$EDGAR_LINK),5,24))

#--- comp_url column is the SEC location of the filers 10K which includes EX 21 if reported
daily_10k$comp_url <<- paste("https://www.sec.gov/Archives/edgar/data/",daily_10k$CIK,"/",gsub("-","",daily_10k$ACCEN_NUM),"/",daily_10k$ACCEN_NUM,"-index.htm",sep = "")

#--- CREATES EX21 LINK COLUMN TO STORE THE EX21 LINK IN THE DAILY 10K DATAFRAME
daily_10k$ex21 <<- NA

#--- CREATES EX21 HTML DATA 

#--- adds the pull date to the dataframe, this is when it was pulled not filed with SEC
daily_10k$ex21 <<- NA
daily_10k$pull_date <<- Sys.Date()

#--- check for exhibit 21 using daily_10k dataframe
#--- Not all 10Ks will have an EX 21 filing

for (i in 1:nrow(daily_10k)) {
     ex21 <<- subset.data.frame(htmltab(daily_10k[i,6],which = 1))
  
        if(count(ex21 %>% 
                 select(Document) %>% 
                 filter(stri_sub(ex21$Type,1,5) == "EX-21"))==0) {
                 daily_10k[i,7]<<- NA
        }else{
              daily_10k[i,7]<<-ex21 %>% 
              select(Document) %>%
              filter(stri_sub(ex21$Type,1,5) == "EX-21")
              daily_10k[i,7]<<-paste("https://www.sec.gov/Archives/edgar/data/",daily_10k[i,1],"/",gsub("-","",daily_10k[i,5]),"/",daily_10k[i,7],sep = "")
  }
}

#--- daily 10K data is added to the master 10k data frame
#--- master 10k data frame is a collection of all previously pulled 10ks
master_10k <<- bind_rows(master_10k, daily_10k)
master_10k <<- distinct(master_10k)

}

#============================================================
#     PROCESS TO PULL EX21 DATA TO MASTER EX 21 LIST 
#============================================================

for (i in 1:nrow(daily_10k)) {
  if(is.na(daily_10k[i,7])) {
    #--- no ex21 data is available, end process and move on to next i value
    next
  }else{
    temp_ex21_subs <- table_or_text(read_html(daily_10k[i,7]))
    temp_ex21_subs$cik <- daily_10k[i,1]
    temp_ex21_subs$pull_date <- Sys.Date()
    master_ex21_subs <- bind_rows(master_ex21_subs, temp_ex21_subs)
    master_ex21_subs <- distinct(master_ex21_subs)
  }
}
View(daily_10k)

#============================================================
#     FUNCTIONS TO SCRAPE EX 21 DATA
#============================================================
  
### function join tables
#head## this function takes all
### the table found in sec21
### and join them together
join_tables <- function(html_obj){
  count <- length(html_obj %>% html_nodes("table"))
  comp_table <- data.frame()
  for (i in 1:count){
    temp_table <- (html_table(html_nodes(html_obj,"table")[[i]])) %>% 
      mutate_all(as.character)
    comp_table <- bind_rows(comp_table, temp_table)
  }
  return(comp_table)
}


### Build out UTF fuction here
###BERKSHIRE ISSUE


### BUILD OUT FUNCTION TO HANDLE STRUCTURES WITHIN THE 21
### kROGER ISSUE


### function clean tables
### this function finds the headers,
### removes the blank row and columns
### removes extra header rows
### leaves fotenotes - for now

clean_tables <- function(df){
  tf <- df %>% 
    mutate_all(funs(str_replace_all(.,"[\r\n]",""))) %>%
    mutate_all(funs(str_replace_all(.," ","")))
  head_col <- which(grepl("orincorporated", tolower(tf))   |            ### replace with model
                      grepl("f incorporat", tolower(tf))   |
                      grepl("fincorporat", tolower(tf))    |
                      grepl("whereincorporat", tolower(tf))    |
                      grepl("fincorporat", tolower(tf))    |
                      grepl("oforgan", tolower(tf))        |
                      grepl("chorgan", tolower(tf))        |
                      grepl("organizedunder", tolower(tf)) |
                      grepl("eorgani", tolower(tf))        |
                      grepl("jurisdiction", tolower(tf)))
  print(head_col)
  head_row <- which(grepl("incorporat", tolower(tf[,head_col])) |
                      grepl("organiz", tolower(tf[,head_col]))   |
                      grepl("jurisdiction", tolower(tf[,head_col])))
  print(head_row)
  names(df) <- df[min(head_row),]
  df <- df[-c(1:min(head_row),head_row),]
  df <- df[(df[, head_col] != ""),]
  df <- df[!is.na(df[, head_col]),]
  df <- df[,colSums(!is.na(df))>0]
  return(df)
}


### function table or text
### this function determines if
### the section 21 filing contains
### tables or text. If test launches table
### functions. If text returns error msg.
table_or_text <- function(html_obj){
  if (length(html_nodes(html_obj, "table")) > 0){
    temp_table <- join_tables(html_obj)
    final_table <- clean_tables(temp_table)
    return(final_table)
  } else{
    final_table <- data.frame(Warning = "Need to Write this Code")
    return(final_table)
  }
    
}



##############################################################################
##0
##                 IDENT DATA COLLECTION AND STORAGE
##  
##############################################################################
## CREATES IDENT_DATA FRAME
## THIS DATAFRAME WILL STORE NAME, ADDRESS AND OTHER COMPANY DATA FROM SEC
## INTEGRATE CURL LIBRARY TO WORK AROUND 401 ISSUE FROM VPN FIREWALL

#--- Z DATAFRAME STORES DATA FROM COMPANY 10 FILING
ident_data_collection <- function(zurl) {
  z <- read_html(zurl)  
  z <- z %>%
     html_nodes("body") %>% 
     html_text()
z <- gsub("\\t","", z)
z <- strsplit(z,"\n")
z<- str_trim(z[[1]],c("both"))
z <- as.data.frame(z)
colnames(z) <- c("a")
z <- subset(z,z$a != "", drop = FALSE)
z <- z[(grep("COMPANY DATA:", z$a)+1):(grep("</SEC-HEADER>", z$a)-1),,drop = FALSE]


#--- Z2/Z3 creates subsets to pull business and mailing address from
ba <- grep("BUSINESS ADDRESS:",z$a)
ma <- grep("MAIL ADDRESS", z$a)
z2 <- z[ba:ma,, drop = FALSE]
z3 <- z[ma:nrow(z),,drop = FALSE]

#---- ident_validation function will check for zero length and replace with NA
ident_validation <- function(iv) {
    if (length(iv) == 0) {
      return(NA)
   }else{
    return(iv)
   }
 }

#-------- CIK CLEAN UP
ident_cik <- z$a[grep("CENTRAL INDEX KEY:",z$a)]
ic1 <- str_locate(ident_cik,":")
ic2 <- str_length(ident_cik)
ident_cik <- ident_validation(stri_sub(ident_cik,ic1[1]+1,ic2))
          
#---------- COMPANY NAME CLEAN UP
ident_name <- z$a[grep("COMPANY CONFORMED NAME:",z$a)]
ic1 <- str_locate(ident_name,":")
ic2 <- str_length(ident_name)
ident_name <- ident_validation(stri_sub(ident_name,ic1[1]+1,ic2))
           
#---------- SIC CLEAN UP
ident_sic <- z$a[grep("STANDARD INDUSTRIAL CLASSIFICATION:",z$a)]
ic1 <- str_locate(ident_sic,"\\[")
ic2 <- str_length(ident_sic)
ident_sic <- ident_validation(stri_sub(ident_sic,ic1[1]+1,ic2-1))

#---------- TAX ID CLEAN UP
ident_irs <- z$a[grep("IRS NUMBER:",z$a)]
ic1 <- str_locate(ident_irs,":")
ic2 <- str_length(ident_irs)
ident_irs <- ident_validation(stri_sub(ident_irs,ic1[1]+1,ic2))

#---------- YEAR END CLEAN UP
ident_fye <- z$a[grep("FISCAL YEAR END:",z$a)]
ic1 <- str_locate(ident_fye,":")
ic2 <- str_length(ident_fye)
ident_fye <- ident_validation(stri_sub(ident_fye,ic1[1]+1,ic2))

#---------- INC STATE CLEAN UP
ident_inc_state <- z$a[grep("STATE OF INCORPORATION:",z$a)]
ic1 <- str_locate(ident_inc_state,":")
ic2 <- str_length(ident_inc_state)
ident_inc_state <- ident_validation(stri_sub(ident_inc_state,ic1[1]+1,ic2))
                
#---------- BUSINESS STREET 1 CLEAN UP
ident_bus_street1 <-z2$a[grep("STREET 1:",z2$a)]
ic1 <- str_locate(ident_bus_street1,":")
ic2 <- str_length(ident_bus_street1)
ident_bus_street1 <- ident_validation(stri_sub(ident_bus_street1,ic1[1]+1,ic2))

#---------- BUSINESS STREET2 CLEAN UP
ident_bus_street2 <-z2$a[grep("STREET 2:",z2$a)]
ic1 <- str_locate(ident_bus_street2,":")
ic2 <- str_length(ident_bus_street2)
ident_bus_street2 <- ident_validation(stri_sub(ident_bus_street2,ic1[1]+1,ic2))

#---------- BUSINESS CITY CLEAN UP
ident_bus_city <- z2$a[grep("CITY:",z2$a)]
ic1 <- str_locate(ident_bus_city,":")
ic2 <- str_length(ident_bus_city)
ident_bus_city <- ident_validation(stri_sub(ident_bus_city,ic1[1]+1,ic2))

#---------- BUSINESS STATE CLEAN UP
ident_bus_state <- z2$a[grep("STATE:",z2$a)]
ic1 <- str_locate(ident_bus_state,":")
ic2 <- str_length(ident_bus_state)
ident_bus_state <- ident_validation(stri_sub(ident_bus_state,ic1[1]+1,ic2))

#---------- BUSINESS POSTAL CLEAN UP
ident_bus_postal <- z2$a[grep("ZIP:", z2$a)]
ic1 <- str_locate(ident_bus_postal,":")
ic2 <- str_length(ident_bus_postal)
ident_bus_postal <- ident_validation(stri_sub(ident_bus_postal,ic1[1]+1,ic2))

#---------- MAILING STREET1 CLEAN UP
ident_mail_street1 <- z3$a[grep("STREET 1:",z3$a)]
ic1 <- str_locate(ident_mail_street1,":")
ic2 <- str_length(ident_mail_street1)
ident_mail_street1 <- ident_validation(stri_sub(ident_mail_street1,ic1[1]+1,ic2))

#---------- MAILING STREET2 CLEAN UP
ident_mail_street2 <- z3$a[grep("STREET 2:",z3$a)]
ic1 <- str_locate(ident_mail_street2,":")
ic2 <- str_length(ident_mail_street2)
ident_mail_street2 <- ident_validation(stri_sub(ident_mail_street2,ic1[1]+1,ic2))
 
#---------- MAILING CITY CLEAN UP
ident_mail_city <- z3$a[grep("CITY:",z3$a)]
ic1 <- str_locate(ident_mail_city,":")
ic2 <- str_length(ident_mail_city)
ident_mail_city <- ident_validation(stri_sub(ident_mail_city,ic1[1]+1,ic2))
 
#---------- MAILING STATE CLEAN UP
ident_mail_state <- z3$a[grep("STATE:",z3$a)]
ic1 <- str_locate(ident_mail_state,":")
ic2 <- str_length(ident_mail_state)
ident_mail_state <- ident_validation(stri_sub(ident_mail_state,ic1[1]+1,ic2))

#---------- MAILING POSTAL CLEAN UP
ident_mail_postal <- z3$a[grep("ZIP:", z3$a)]
ic1 <- str_locate(ident_mail_postal,":")
ic2 <- str_length(ident_mail_postal)
ident_mail_postal <- ident_validation(stri_sub(ident_mail_postal,ic1[1]+1,ic2))

#---------- BUSINESS PHONE
ident_phone <- z$a[grep("BUSINESS PHONE:",z$a)]
ic1 <- str_locate(ident_phone,":")
ic2 <- str_length(ident_phone)
ident_phone <- ident_validation(stri_sub(ident_phone,ic1[1]+1,ic2))

#---------- THESE NEED TO BE ADDED AT SOME POINT IN THE FUTURE, NO CURRENT BUSINESS NEED
##ident_former_name1 <- ## add later on
##ident_former_name1_change <- ## add later on
##ident_former_name2 <- ## add later on
##ident_former_name2_change <- ## add later on

#--- TEMP TABLE CREATED TO STORE DATA PULLED FROM 10K FOR A SINGLE COMPANY
temp_ident_data <- data.frame("cik" = ident_cik,
                              "comp_name" = ident_name,
                              "sic" = ident_sic,
                              "irs_id" = ident_irs,
                              "fye" = ident_fye,
                              "inc_state" = ident_inc_state,
                              "bus_street1" = ident_bus_street1,
                              "bus_street2" = ident_bus_street2,
                              "bus_city" = ident_bus_city,
                              "bus_state" = ident_bus_state,
                              "bus_postal" = ident_bus_postal,
                              "mail_street1" = ident_mail_street1,
                              "mail_street2" = ident_mail_street2,
                              "mail_city" = ident_mail_city,
                              "mail_state" = ident_mail_state,
                              "mail_postal" = ident_mail_postal,
                              "phone" = ident_phone,
                              stringsAsFactors = FALSE)

#--- MASTER_IDENT_DATA STORES IDENT DATA ON ALL PREVIOUISLY PULLED COMPANIES
master_ident_data <<- bind_rows(temp_ident_data, master_ident_data)
master_ident_data <<- distinct(master_ident_data)
}

#test cases for ident_data_collection function
ident_data_collection("https://www.sec.gov/Archives/edgar/data/789019/000156459017014900/0001564590-17-014900-index-headers.html")  ## microsoft us address
ident_data_collection("https://www.sec.gov/Archives/edgar/data/1593812/000121465918003326/0001214659-18-003326-index-headers.html") ## canadaian address
ident_data_collection("https://www.sec.gov/Archives/edgar/data/1365357/000109181818000086/0001091818-18-000086-index-headers.html") ## hong kong
ident_data_collection("https://www.sec.gov/Archives/edgar/data/1012477/000114420418023840/0001144204-18-023840-index-headers.html") ## ireland
ident_data_collection("https://www.sec.gov/Archives/edgar/data/1623613/000162361318000010/0001623613-18-000010-index-headers.html") ## UK this is an issue
ident_data_collection("https://www.sec.gov/Archives/edgar/data/1652044/000165204418000007/0001652044-18-000007-index-headers.html") ## google us address

#-----------------------------------------------------------------------------
#============================================================
#     MATCH SEC DATA AGAINST EXPERIAN DATA  !!!!!! UNDER CONSTRUCTION !!!!!!
#============================================================
### MATCHES SEC DATA AGAINST BASE FILE WHICH IS EXPERIAN DATA
### NEED TO ADD A GEOGRAPHIC MATCH REQUIREMENT, CURRENTLY MATCH IS NAME ONLY
### NEED TO CREATE A WAY TO AUTOMATICALLY SELECT THE NAME FIELD FROM EACH SEC SCRAPE
### EACH COMPANY HAS A SEARCH RESTRICTION USING THE ULT PARENT BIN.  
###      A PROCESS OR CROSSWALK NEEDS TO BE ESTABLISHED TO ASSIGN THE ULT BIN TO THE ULT CIK


### YUM BRANDS TEST MATCH
YUM <- read_html("https://www.sec.gov/Archives/edgar/data/1041061/000104106118000013/yum-12312017xex211.htm")
SEC_YUM <-table_or_text(YUM)
SEARCH_BASE_FILE <- subset(BASE_FILE, ULT_PARENT_PBIN==749037323)
SEC_YUM$SEC_MATCH <- SEARCH_BASE_FILE[(amatch(tolower(SEC_YUM[,1]), tolower(SEARCH_BASE_FILE[,2]), method = 'jw' )),1:24]
View(SEC_YUM)

### UNITED TECHNOLOGIES TEST MATCH
UTC <- read_html("https://www.sec.gov/Archives/edgar/data/101829/000010182918000005/a2017-12x3110xkexhibit21.htm")
a <- UTC
ult<-700051349
View(SEC_UTC <- table_or_text(a))
SEARCH_BASE_FILE <- subset(BASE_FILE, ULT_PARENT_PBIN==ult)
SEC_UTC$SEC_MATCH <- SEARCH_BASE_FILE[(amatch(tolower(SEC_UTC[,1]), tolower(SEARCH_BASE_FILE[,2]), method = 'jw' )),1:24]
View(SEC_UTC)

###CFCB
### NOTE IN THE NAME COLOUMN CAUSED ONE NAME NOT TO PARSE OUT CORRECTLY

CFCB <- read_html("https://www.sec.gov/Archives/edgar/data/19612/000001961218000037/chfc201710-kexhibit21subsi.htm")
a <- CFCB
ult<-748392624
View(SEC_FILE <- table_or_text(a))
SEARCH_BASE_FILE <- subset(BASE_FILE, ULT_PARENT_PBIN==ult)
View(amatch(tolower(SEC_FILE[,1]), tolower(SEARCH_BASE_FILE[,2]), method = 'jw' ))
SEC_FILE$SEC_MATCH <- SEARCH_BASE_FILE[(amatch(tolower(SEC_FILE[,1]), tolower(SEARCH_BASE_FILE[,2]), method = 'jw' )),1:24]
View(SEC_CFCB <- SEC_FILE)


#########################################3


  
  
  
  
  
  













