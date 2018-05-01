
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
