setwd("~/GitHub/Incubator-Proposal")

#load openpv data
solar <- read.csv("~/GitHub/Incubator-Proposal/openpv_all.csv", stringsAsFactors = FALSE)
#some basic data cleaning to collapse levels of factor solar$install_type
unique(solar$install_type)
solar$install_type2 <- `levels<-`(factor(solar$install_type), list(agriculture=c("Agricultural","agricultural",
                                                          "agriculture","Commercial - Agriculture"), 
                                            commercial=c("Commercial","commercial","Commercial - Other",
                                                         "Commerical","Commercial - Agriculture",
                                                         "Small Business","Commercial - Small Business"),
                                            educational=c("Educational","educational","education"),
                                            institutional=c("Nonprofit","government","Government","nonprofit",
                                                            "Municipal","public","Institutional","Public",
                                                            "Gov't/NP"),
                                            residential=c("residential","Residential","Residential/SF"),
                                            utility=c("Utility","utility"),
                                            unknown=c("Unknown","","unknown","Customer","Nonresidential","Not Stated")))
unique(solar$install_type2)

# load utility rates by zip
setwd("~/GitHub/Incubator-Proposal/Rates")
files <- list.files(path = "~/GitHub/Incubator-Proposal/Rates", pattern="*.csv")
# First apply read.csv, then rbind
rates <- data.frame()
rates <- do.call(rbind, lapply(files, function(x) cbind(read.csv(x), name=strsplit(x,'\\.')[[1]][1])))
save(rates, file = "~/GitHub/Incubator-Proposal/rates.csv")
rm(files)
setwd("~/GitHub/Incubator-Proposal")
load("~/GitHub/Incubator-Proposal/rates.csv")

library(tidyr)
rates <- rates %>% separate(name, c("iou | noniou","201*"), "zipcodes")
names(rates)[names(rates) == '201*'] <- 'year'
# how to combine

solar$install <- as.Date(solar$date_installed, format = "%m/%d/%Y")
solar$installyear <- as.numeric(format(solar$install, "%Y"))

solarpv <- solar[c(-(31:81))]


# trying to add other data

library(plyr)
library(xlsx)
library(xlsxjars)
library(rJava)
#download all available data: tax returns by zip code
years <- c(1999, 2001, 2002,(seq(2004,2010)))
temp <- tempfile()
download.file("https://www.irs.gov/pub/irs-soi/1998zipcode.zip",temp, mode="wb")
unzipped <- unzip(temp, exdir=getwd())
filenames <- list.files(path = "C:/Users/Sarah/Documents/GitHub/Incubator-Proposal/1998ZIPCode", pattern = "xls")
soi <- read.xlsx("~/GitHub/Incubator-Proposal/1998ZIPCode/98zp01al.xlsx", 1, colIndex = c(1,2,5), startRow = 8, endRow = 4825)


#import DSIRE data on incentives
library(XML)
library(rvest)
library(dplyr)
urldsire <- "https://ncsolarcen-prod.s3.amazonaws.com/fullexports/dsire-2017-08.xml"
download.file(urldsire, "~/GitHub/Incubator-Proposal/dsire-2017-08.xml")
xmlfile <- xmlParse("~/GitHub/Incubator-Proposal/dsire-2017-08.xml")
class(xmlfile) 
xmltop = xmlRoot(xmlfile)
class(xmltop)
xmlName(xmltop)
xmlSize(xmltop)
