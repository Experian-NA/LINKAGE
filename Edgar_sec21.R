
### Packages
library(rvest)
library(dplyr)
library(stringr)
library(qdap)
library(stringdist)
library(parallel)
library(htmltab)


### IMPORT BASE FILE TO MATCH (NO BRANCH RECORDS INCLUDED)
### File is used to match SEC names against
### File contains 458,151 records
### generated using ULT PBIN field from bcp001.watch_lst

BASE_FILE = read.csv("BASE_FILe.csv", stringsAsFactors = FALSE)  # read csv file 

#============================================================
#     SEC DATA EXTRACTION
#============================================================
### pull previous days filings with SEC
### no filings have been found over a weekend.
### data only returned Tuesday through Saturday morning.
### for testing, if Monday use Sys.Date()-3 to pull friday filings else Sys.Date()-1
daily_sec_pull <- getDailyMaster(Sys.Date()-1)
daily_10k <- subset(daily_sec_pull, FORM_TYPE=='10-K'|FORM_TYPE=='10-K/A')

### ADD THE ACCENSION NUMBER TO daily_10k dataframe
### The Accension number is a unique ID assigned to each filing with the SEC
daily_10k$ACCEN_NUM <- stri_reverse(stri_sub(stri_reverse(daily_10k$EDGAR_LINK),5,24))
### Build out the company URL to SEC filings
### comp_url is the SEC location of the filers 10K which includes EX 21 if reported
daily_10k$comp_url <- paste("https://www.sec.gov/Archives/edgar/data/",daily_10k$CIK,"/",gsub("-","",daily_10k$ACCEN_NUM),"/",daily_10k$ACCEN_NUM,"-index.htm",sep = "")

### CREATES EX21 COLUMN TO STORE THE EX21 LINK IN THE DAILY 10K DATAFRAME
daily_10k$ex21 <- NA


### check for exhibit 21 using daily_10k dataframe
### Not all 10Ks will have an EX 21 filing

for (i in 1:nrow(daily_10k)) {
     ex21 <- subset.data.frame(htmltab(daily_10k[i,6],which = 1))
  
        if(count(ex21 %>% 
                 select(Document) %>% 
                 filter(stri_sub(ex21$Type,1,5) == "EX-21"))==0) {
                 daily_10k[i,7]<- NA
        }else{
              daily_10k[i,7]<-ex21 %>% 
              select(Document) %>%
              filter(stri_sub(ex21$Type,1,5) == "EX-21")
              daily_10k[i,7]<-paste("https://www.sec.gov/Archives/edgar/data/",daily_10k[i,1],"/",gsub("-","",daily_10k[i,5]),"/",daily_10k[i,7],sep = "")
  }
}

#=======!!!!!!!!!!!!!UNDER CONSTRUCTION!!!!!!!!!!!!============
#PULL FILER DATA
#THIS IS SCRAPING THE COMPANY DATA IN THE BLUE BOX AT
#THE BOTTOM OF THE COMP_URL WEB PAGE
#IT WILL BE NEEDED TO IDENTIFY THE ULTIMATE PARENT FOR SEARCH MATCH
#===========================================================
## z <- "https://www.sec.gov/Archives/edgar/data/1652044/000165204418000007/0001652044-18-000007-index.htm"
## z <- "https://www.sec.gov/Archives/edgar/data/789019/000156459017014900/0001564590-17-014900-index.htm"
z <- "https://www.sec.gov/Archives/edgar/data/1593812/0001214659-18-003326-index.html" ## canadaian address
z <- "https://www.sec.gov/Archives/edgar/data/1365357/0001091818-18-000086-index.html" ## hong kong
z <- "https://www.sec.gov/Archives/edgar/data/1012477/0001144204-18-023840-index.html" ## ireland
z <- "https://www.sec.gov/Archives/edgar/data/1623613/000162361318000010/0001623613-18-000010-index.htm" ## UK this is an issue
comp <- read_html(z)
comp <- comp %>%
        html_nodes("#filerDiv") %>% 
        html_text()
comp <- gsub("\\|","\\\n", comp)
comp <- strsplit(comp,"\n")
comp<- str_trim(comp[[1]],c("both"))
comp <- as.data.frame(comp)
colnames(comp) <- c("a")
comp <- filter(comp,comp$a != "")
View(comp)
## COMPANY NAME
cn <- grep("(Filer)",comp$a) ## ROW WITH COMPANY NAME
comp_name <- comp$a[cn]
comp_name <- stri_sub(comp_name, 1, (stri_length(comp_name) - 7))
comp_name <- str_trim(comp_name, c("both"))
comp_name


## COMPANY MAILING ADDRESS (use if postal is US with no suffix)
mas <- grep("Mailing Address", comp$a) +1  ## MAILING ADDRESS STREET
mar <- grep("Business Address", comp$a)-1 ## MAILING ADDRESS REGION (CITY/ST/POSTAL)
mal <- str_length(comp$a[mar])  ## length of MAR (MAILING ADDRESS REGION)

## sets value of m, m is used to determine what sytle of address is used
## address can have different types of zips US(#####) US(#####-####), Canadian, other
## SEC recycles the State field to be used as a country code field 
## see the URL for look up  https://www.sec.gov/edgar/searchedgar/edgarstatecodes.htm
if ( regexpr(" ", stri_reverse(comp$a[mar]))== 6 ) {
    m <- 5
} else if (regexpr("-", stri_reverse(comp$a[mar])) == 5) {
    m <- 10
} else if (regexpr(" ", stri_reverse(comp$a[mar]))== 4 ) {
    m <- 7
} else {
   m <- 1
}


mail_street <- comp$a[mas]
mail_city <- stri_reverse(stri_sub(stri_reverse(comp$a[mar]),m+5,mal))
mail_state <- stri_reverse(stri_sub(stri_reverse(comp$a[mar]),m+2,m+3))
mail_postal <- stri_reverse(stri_sub(stri_reverse(comp$a[mar]),1,m))
mail_street
mail_city
mail_state
mail_postal

## COMPANY BUSINESS ADDRESS (!!!!!NEEDS TO BE ADDED!!!!!)

## STATE OF INCORPORATION
inc <- comp$a[grep("State of Incorp.:",comp$a)]
inc <- gsub(" ","",inc)
inc <- stri_sub(inc,16,18)
inc
## COMPANY CIK
cik <- comp$a[grep("(see all company filings)", comp$a)]
cik <- gsub(" ","",cik)
cik <- stri_sub(cik,5,14)
cik
## IRS ID (i believe this is the federal tax id)
irs <- comp$a[grep("IRS No.:", comp$a)]
irs <- gsub(" ","",irs)
irs <- stri_sub(irs,8,stri_length(irs))
irs
## YEAR END FOR COMPANY
fye <- comp$a[grep("Fiscal Year End:", comp$a)]
fye <- gsub(" ","",fye)
fye <- stri_sub(fye,15,18)
fye
## SIC (!!!!!! this should be the last line in the dataframe!!!!!)
sic <- tail(comp$a,1)
sic <- gsub(" ","",sic)
y = regexpr("SIC:", sic)
sic <- stri_sub(sic,y+4,y+7)
sic

## use the link info below to access earnings statement from EDGAR.
## May not be available for all records, accessed through the interactive data option
View(htmltab("https://www.sec.gov/Archives/edgar/data/51143/000104746918001117/R2.htm"))
View(htmltab("https://www.sec.gov/Archives/edgar/data/1138724/000101489718000034/R2.htm"))

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


#============================================================
#     TEST CASES INCLUDES TOP 20 F1000
#============================================================


### test for General Dynamics
GD <- read_html("https://www.sec.gov/Archives/edgar/data/40533/000004053318000008/ex21-20171231.htm")
table_or_text(UTC)

### test for IBM
IBM <-read_html("https://www.sec.gov/Archives/edgar/data/51143/000104746918001117/a2233835zex-21.htm")
table_or_text(IBM)

### test for Facebook
FB <- read_html("https://www.sec.gov/Archives/edgar/data/1326801/000132680118000009/fb-12312017x10kexhibit211.htm")
### failed text file no table

### test for Coke
COKE <- read_html("https://www.sec.gov/Archives/edgar/data/317540/000156459018003845/coke-ex21_14.htm")

### test for XOM
XOM <- read_html("https://www.sec.gov/Archives/edgar/data/34088/000003408818000015/xomexhibit21.htm")

### test for MRK
MRK <- read_html("https://www.sec.gov/Archives/edgar/data/310158/000031015818000005/ex21subsidiarylistasof1231.htm")

### test for MCD
MCD <- read_html("https://www.sec.gov/Archives/edgar/data/63908/000006390818000010/mcd-12312017xex21.htm")
### failed only on column

### test for DIS
DIS <- read_html("https://www.sec.gov/Archives/edgar/data/1001039/000100103917000198/fy2017_q4x10kxex21.htm")

### test for AAPL
AAPL <- read_html("https://www.sec.gov/Archives/edgar/data/320193/000032019317000070/a10-kexhibit2112017.htm")
### failed unsure why

### test for AXP
AXP <- read_html("https://www.sec.gov/Archives/edgar/data/4962/000000496218000032/axp2017ex21.htm")

### test for MSFT
MSFT <- read_html("https://www.sec.gov/Archives/edgar/data/789019/000156459017014900/msft-ex21_10.htm")

### test for BA
BA <- read_html("https://www.sec.gov/Archives/edgar/data/12927/000001292718000007/a201712dec3110kexhibit21.htm")

### test for CAT
CAT <- read_html("https://www.sec.gov/Archives/edgar/data/18230/000001823018000042/cat_exx21x12312017.htm")

### test for CVX
CVX <- read_html("https://www.sec.gov/Archives/edgar/data/93410/000009341018000010/cvx12312017ex211.htm")

### test CSCO
CSCO <- read_html("https://www.sec.gov/Archives/edgar/data/858877/000085887717000016/exh211significantsubsidiar.htm")

### test DWDP
DWDP <- read_html("https://www.sec.gov/Archives/edgar/data/1666700/000166670018000009/dwdp201710kex21.htm")

### test PFE
PFE <- read_html("https://www.sec.gov/Archives/edgar/data/78003/000007800318000027/pfe-exhibit21x12312017.htm")

### BRKA
BRK <- read_html("https://www.sec.gov/Archives/edgar/data/1067983/000119312518057033/d437858dex21.htm")

### svu [jarrod]
SVU <- read_html("https://www.sec.gov/Archives/edgar/data/95521/000009552118000026/f18form10-kex211.htm")

### carMAX [jarrod]
KMX <- read_html("https://www.sec.gov/Archives/edgar/data/1170010/000117001018000054/kmx02282018exhibit211.htm")
##  Error in names(frame) <- `*vtmp*` : names() applied to a non-vector

## renova health [jarrod]
RNVA <- read_html("https://www.sec.gov/Archives/edgar/data/931059/000149315218005716/ex21.htm")

## SIGMA DESIGNS [jarrod]
SIGM <- read_html("https://www.sec.gov/Archives/edgar/data/790715/000143774918007502/ex_110946.htm")

## Xspand Products Lab [jarrod]
XSPL <- read_html("https://www.sec.gov/Archives/edgar/data/1717556/000114420418021918/tv491658-ex21_1.htm")

## MCKESSON [jarrod]
MCK <- read_html("https://www.sec.gov/Archives/edgar/data/927653/000092765317000007/mck_exhibit21x3312017.htm")

### UNITED HEALTH CARE [jarrod]
UNH <- read_html("https://www.sec.gov/Archives/edgar/data/731766/000073176618000005/unhex21112312017.htm")

### GM [jarrod]
GM <- read_html ("https://www.sec.gov/Archives/edgar/data/1467858/000146785818000022/ex-21x12312017.htm")

###AmerisourceBergen [jarrod]
ABC <- read_html ("https://www.sec.gov/Archives/edgar/data/1140859/000114085917000047/exhibit21subsidiariesofthe.htm")

### AMAZON [jarrod]
AMZN <- read_html ("https://www.sec.gov/Archives/edgar/data/1018724/000101872418000005/amzn-20171231xex211.htm")

### GE [jarrod]
GE <- read_html ("https://www.sec.gov/Archives/edgar/data/40545/000004054518000014/geform10-k2017xex21.htm")

### Verizon [jarrod]
vz <- read_html ("https://www.sec.gov/Archives/edgar/data/732712/000073271218000009/a201710-kxexhibit21.htm")

### Cardinal Health [jarrod]
CAH <- read_html ("https://www.sec.gov/Archives/edgar/data/721371/000072137117000083/a17q4_10kx063017xexhibit211.htm")

### COSTCO [jarrod]
COST <- read_html("https://www.sec.gov/Archives/edgar/data/909832/000090983217000014/costex21110k90317.htm")

### Walgreens [jarrod]
WBA <- read_html("https://www.sec.gov/Archives/edgar/data/1618921/000161892117000069/exhibit21.htm")


### Chevron [jarrod]
CVX <- read_html ("https://www.sec.gov/Archives/edgar/data/93410/000009341018000010/cvx12312017ex211.htm")

 ## Parkway Properties [jarrod]
PKY <- read_html ("https://www.sec.gov/Archives/edgar/data/1677761/000167776117000018/pkyinc-20161231x10kxex211.htm")

## US AUTO PARTS ASSOCIATION [jarrod]
APA <- read_html("https://www.sec.gov/Archives/edgar/data/1378950/000162828018003196/ex-21110xk2017.htm")

#####################################################
#
#      MAY 1, 2018 results for filings on 4/30/2018
#      3 Successes  1 Failure (helen of troy)
#####################################################

## JANEL CORP (MAY 1, 2018)
JANEL <- read_html("https://www.sec.gov/Archives/edgar/data/1133062/000095015918000180/ex21.htm")
SEC_JANEL <- table_or_text(JANEL)
## results = success

## APOGEE ENTERPRISES, INC. (MAY 1, 2018)
APOGEE <- read_html("https://www.sec.gov/Archives/edgar/data/6845/000000684518000009/apog-ex21_20183310k.htm")
SEC_APOGEE <- table_or_text(APOGEE)
## results = success

## Marina Biotech, Inc. (MAY 1, 2018)
MARINA_BIO <- read_html("https://www.sec.gov/Archives/edgar/data/737207/000149315218005964/ex21-1.htm")
SEC_MARINA_BIO <- table_or_text(MARINA_BIO)
## results = success

## HELEN OF TROY (MAY 1, 2018)
HELEN_TROY <- read_html("https://www.sec.gov/Archives/edgar/data/916789/000155837018003549/hele-20180228ex2187ca156.htm")
SEC_HELEN_TROY <- table_or_text(HELEN_TROY)
join_tables(HELEN_TROY)
## results = !!!!!failure!!!!!

#####################################################
#
#      MAY 2, 2018 results for filings on 5/1/2018
#      1 Successes  2 Failure (---)
#####################################################
## NAKED BRAND GROUP - (MAY 2, 2018)
TEMP1 <- read_html("https://www.sec.gov/Archives/edgar/data/1383097/000114420418024496/tv492321_ex21-1.htm")
SEC_NKD_BRND_GRP <- table_or_text(TEMP1)
View(SEC_NKD_BRND_GRP)
## results = SUCCESS

##  Neutra Corp - (MAY 2, 2018)
TEMP1 <- read_html("https://www.sec.gov/Archives/edgar/data/1512886/000116169718000230/ex_21.htm")
NEUTRA_CORP <- table_or_text(TEMP1)
View(NEUTRA_CORP)
## results = FAILURE, GENERATED "NEED TO WRITE THIS CODE"

## IPIC_ENT - (MAY 2, 2018)
TEMP1 <- read_html("https://www.sec.gov/Archives/edgar/data/1720201/000121390018005338/f10k2017ex21-1_ipicentertain.htm")
IPIC_ENT <- table_or_text(TEMP1)
View(IPIC_ENT)
## results = FAILURE, GENERATED "NEED TO WRITE THIS CODE"


#######################################################################################################
#####################################################
#
#      MAY 4, 2018 results for filings on 5/3/2018
#      6 Successes  0 Failure (---)
#####################################################
View(daily_10k)
TEMP1 <- read_html(daily_10k[10,7])

IHEART_COMMUNICATIONS <- table_or_text(TEMP1)
View(IHEART_COMMUNICATIONS)
## results = SUCCESS

IHEARTMEDIA_CAPITAL <- table_or_text(TEMP1)
View(IHEARTMEDIA_CAPITAL)
## results = SUCCESS

IHEARTMEDIA <- table_or_text(TEMP1)
View(IHEARTMEDIA)
## results = SUCCESS

CCOH <- table_or_text(TEMP1)
View(CCOH)
## results = SUCCESS

COMVALUT_SYSTEMS <- table_or_text(TEMP1)
View(COMVALUT_SYSTEMS)
## results = SUCCESS

GILLA <- table_or_text(TEMP1)
View(GILLA)
## results = SUCCESS

#####################################################
#
#      MAY 5, 2018 results for filings on 5/4/2018
#      2 Successes  1 Failure (---)
#####################################################
View(daily_10k)
TEMP1 <- read_html(daily_10k[11,7])

FREDS <- table_or_text(TEMP1)
View(FREDS)
## results = success

TRANS_WORLD <- table_or_text(TEMP1)
View(TRANS_WORLD)
## results = success
STEIN_MART <- table_or_text(TEMP1)
View(STEIN_MART)
## failure = "need to write this code"



##############################################################################
##
##                 IDENT DATA COLLECTION AND STORAGE
##
##############################################################################
## CREATES IDENT_DATA FRAME
## THIS DATAFRAME WILL STORE NAME, ADDRESS AND OTHER COMPANY DATA FROM SEC
ident_data <- data.frame(cik = integer(),
                         accen_num_10K = integer(),
                         comp_name = character(),
                         sic = character(),
                         irs_id = integer(),
                         fye = character(),
                         bus_street = character(),
                         bus_city = character(),
                         bus_state = character(),
                         bus_postal = character(),
                         mail_street = character(),
                         mail_city = character(),
                         mail_state = character(),
                         mail_postal = character(),
                         former_name1 = character(),
                         former_name_change = character(),
                         former_name2 = character(),
                         former_name_change = character(),
                         stringsAsFactors = FALSE)

###########################################################
##z <- read_html("https://www.sec.gov/Archives/edgar/data/789019/000156459017014900/0001564590-17-014900-index-headers.html")  ## us address
##z <- read_html("https://www.sec.gov/Archives/edgar/data/1593812/000121465918003326/0001214659-18-003326-index-headers.html") ## canadaian address
##z <- read_html("https://www.sec.gov/Archives/edgar/data/1365357/000109181818000086/0001091818-18-000086-index-headers.html") ## hong kong
##z <- read_html("https://www.sec.gov/Archives/edgar/data/1012477/000114420418023840/0001144204-18-023840-index-headers.html") ## ireland
##z <- read_html("https://www.sec.gov/Archives/edgar/data/1623613/000162361318000010/0001623613-18-000010-index-headers.html") ## UK this is an issue

z <- read_html("https://www.sec.gov/Archives/edgar/data/1652044/000165204418000007/0001652044-18-000007-index-headers.html") ## unknown

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

## insert ident data into the ident data frame

ident_cik <- grep("CENTRAL INDEX KEY:",z$a)
ident_cik
ident_name <- grep("COMPANY CONFORMED NAME:",z$a)
ident_name
ident_sic <- grep("STANDARD INDUSTRIAL CLASSIFICATION:")
ident_irs <- grep("IRS NUMBER:")
ident_fye <- grep("FISCAL YEAR END:")
ident_inc_state <- grep("STATE OF INCORPORATION:")
ident_bus_street <-
ident_bus_city <-
ident_bus_state <-
ident_bus_postal <-
ident_mail_street <-
ident_mail_city <-
ident_mail_postal <-
ident_former_name1 <-
ident_former_name1_change <-
ident_former_name2 <-
ident_former_name2_change <-
