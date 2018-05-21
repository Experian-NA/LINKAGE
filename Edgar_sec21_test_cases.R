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

#####################################################
#
#      MAY 10, 2018 results for filings on 5/4/2018
#      2 Successes  0 Failure (---)
#####################################################
View(daily_10k)
TEMP1 <- read_html(daily_10k[10,7])

CA <- table_or_text(TEMP1)
View(CA)
## results = success
PLANTRONICS <- table_or_text(TEMP1)
View(PLANTRONICS)
## results = success
