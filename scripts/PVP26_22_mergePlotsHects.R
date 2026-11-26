########
rm(list=ls())

# The following packages need to be installed for the following functions to work: "RCurl", "data.table","picante"
library("RCurl")
library("data.table")
library("picante")

## LOAD PPW FUNCTIONS
# this code is from some delightful human on the internet who figured out how to source from GitHub; run this function in order to source in the script with all the Pepperwood Functions
source_https <- function(url, ...) { 
  # parse and evaluate each .R script
  sapply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
  })
}

# sources in all functions (described below) that allow access to the PPW Vegetation Plot Data

# once functions are uploaded to github, swap these two lines
source_https('https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/Analyses/PW_functions_GitHub_2026update.R')
########

# read in plot data; file prepped by K26_10 and K26_20
tAll <- read.csv('bigdata/tmp/tAllPlot.csv')
dim(tAll)
names(tAll)

# Read in Hectare data. This data file is cleaned and prepared in KR26_10.5.process.hectare.data.R
htAll <- readRDS('bigdata/tmp/hectares.rds')
names(htAll)
dim(htAll)

allNames <- union(names(tAll),names(htAll))
(sharedNames <- intersect(names(tAll),names(htAll)))

length(allNames)
length(sharedNames)
sharedNames


# check no overlap in numbers
length(intersect(tAll$Num,htAll$Num))

pw <- merge(tAll,htAll,by=intersect(names(htAll),names(tAll)),all=T)
dim(pw)

names(pw)
table(pw$Survey)
table(pw$Survey,pw$ExcStem,useNA='a')
table(pw$coordOutlier,pw$ExcStem,pw$Survey)

# A fairly large number of hectare plants are tagged for exclusion, including main stem trees in Hect that were < 20 cm at first survey. But they are not being excluded. One reason is that we want the final data set to include all tags that people might encounter in the field.

# larger stem numbers evident for superplots which include hectare samples (e.g. 1301, 1352)
table(pw$Plot,useNA='always') 
table(pw$Plot.Orig,pw$Survey)

# checking that any plants not Alive in 2017 were in plots (these would be later additions). Hectares only surveyed in 2018 (and 2021 for 2152), so only stems Alive or assumed to be Alive in 2017 are included
table(pw$Live.17,pw$Survey,useNA='always')
table(pw$Dead.17,pw$Live.17,pw$Survey,useNA='always')
pw$Dead.17 <- pw$Live.17-1 # fixed missing Dead value for hectare plants

### Read in species codes - in this script, called 'Species'
spAtt <- read.csv("https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/SpeciesData/all-spp-attributes.csv")

dim(spAtt)
head(spAtt)
tail(spAtt)
table(spAtt$FuncGroup)

### convert Species# convert QUEBEGA to QUEBER, QUEDOGA to QUEDOU, QUEdec, QUEDEC to QUEDOU, and remove unknowns and BACPIL
# # Baccharis was removed from analysis and dropped from ongoing surveys, due to rapid resprouting which made it difficult to evaluate fire impacts.

table(pw$Species)
pw$Species[which(pw$Species=='QUEBEGA')] <- 'QUEBER' # potential hybrids assigned to BER
pw$Species[which(pw$Species=='QUEDOGA')] <- 'QUEDOU' # potential hybrids assigned to DOU
pw$Species[which(pw$Species %in% c('QUEdec','QUEDEC'))] <- 'QUEDOU' # indet deciduous assigned to DOUG
pw <- pw[-which(pw$Species %in% c('BACPIL')),]
table(pw$Species)
dim(pw)

# update prows and hrows after removing rows
pRows <- which(pw$Survey=='Plot')
hRows <- which(pw$Survey=='Hect')

# a few errors in Types - fix these in csvs?
table(pw$Type.13)
table(pw$Type.17)

### Add functional groups to tAll
### Are all species in spAtt? YES
(tAllspec <- sort(unique(pw$Species)))
tAllspec[which(!tAllspec %in% spAtt$OrigSpecies)]

pw$FuncGroup <- spAtt$FuncGroup[match(pw$Species,spAtt$OrigSpecies)]
table(pw$FuncGroup)

### TS are large trunks that have broken off and have a resprout. Removed for KR publication. Left here to ensure that all tagged plants are in database
table(pw$Type.13,pw$Type.17,useNA='always')

# Calculate prefire basal area at breast height (BABH) and at 10 cm stem height (BAd10.17)
pw$BABH.17 <- pi*(pw$DBH_cm.17/2^2)
pw$BAd10.17 <- pi*(pw$d10.17/2^2)

# assign cumulative tag pulled variable - run again after fixing 2020 - Melina
pw$Tag.Pulled.Cum <- 0
pw$Tag.Pulled.Cum[which(pw$Tag.Pulled.18==1)] <- 1
pw$Tag.Pulled.Cum[which(pw$Tag.Pulled.19==1)] <- 1
pw$Tag.Pulled.Cum[which(pw$Tag.Pulled.20=="1")] <- 1
pw$Tag.Pulled.Cum[which(pw$Num>=99000)] <- 1
table(pw$Tag.Pulled.Cum)

## EDIT AFTER MELINA FIX
tp20comments <- which(pw$Tag.Pulled.20 %in% c('LOST','NEW SA','NOT FOUND','NOT FOUND IN 19 OR 20','NOT FOUND, DOSER KILL','TAG MOVED TO RSPRT','TYPE IN 19, NOT TR'))
write.csv(pw[tp20comments,c('Plot','Num','Tag.Pulled.20')],'tmp/tp20problems.csv')

## REMOVE UNNEEDED COLUMNS
names(pw)

countData <- function(i)
{
  pd <- length(which(!is.na(pw[pRows,i])))
  pn <- length(which(is.na(pw[pRows,i])))
  hd <- length(which(!is.na(pw[hRows,i])))
  hn <- length(which(is.na(pw[hRows,i])))
  return(c(pd,pn,hd,hn))
}

dTab <- data.frame(colN=names(pw),pd=NA,pn=NA,hd=NA,hn=NA)
dim(dTab)
for (i in 1:length(names(pw))) dTab[i,2:5] <- countData(i)
head(dTab)
tail(dTab)
write.csv(dTab,'tmp/pwAllColNames.csv')

# plot large trees
LT <- which(pw$DBH_cm.17>=20)
hp <- which(pw$hectPlot==1)
rsel <- which(pw$DBH_cm.17>=20 & pw$hectPlot==1 & pw$coordOutlier==0)
length(LT)
plot(pw$Rel.UTM.E[rsel],pw$Rel.UTM.N[rsel],asp=1)

#colOrder <- c('Plot','Num','')

## Write final data set
dim(pw)
write.csv(pw,'bigdata/masterdata/pvp26.csv')
