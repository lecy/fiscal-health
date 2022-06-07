library( dplyr )

source( "https://www.dropbox.com/s/obub14z1st4z85o/crosswalk-functions.R?dl=1" )

# loaded crosswalk functions 
# ls() 


v2019.soi <- "https://www.dropbox.com/s/88y9sup4d3mwoo5/irs-990-pc-soi-extract-2019.csv?dl=1"
d.2019.soi <- read.csv( v2019.soi )
d.2019.soi <- dplyr::arrange( d.2019.soi, desc(tax_pd) )
d.2019.soi <- d.2019.soi[ ! duplicated(d.2019.soi$EIN) , ]
d.2019 <- X2019.SOI.RENAME( d.2019.soi )
d.2019$SOURCE <- "SOI"
d.2019$YEAR <- "2019"

v2016.soi <- "https://www.dropbox.com/s/iuav0itdx8glqpt/irs-990-pc-soi-extract-2016.csv?dl=1"
d.2016.soi <- read.csv( v2016.soi )
d.2016.soi <- dplyr::arrange( d.2016.soi, desc(tax_pd) )
d.2016.soi <- d.2016.soi[ ! duplicated(d.2016.soi$EIN) , ]
d.2016 <- X2016.SOI.RENAME( d.2016.soi, ein="EIN" )
d.2016$SOURCE <- "SOI"
d.2016$YEAR <- "2016"

v2013.soi <- "https://www.dropbox.com/s/glkyn5mlsx0d2c7/irs-990-pc-soi-extract-2013.csv?dl=1"
d.2013.soi <- read.csv( v2013.soi )
d.2013.soi <- dplyr::arrange( d.2013.soi, desc(tax_pd) )
d.2013.soi <- d.2013.soi[ ! duplicated(d.2013.soi$EIN) , ]
d.2013 <- X2013.SOI.RENAME( d.2013.soi, ein="EIN" )
d.2013$SOURCE <- "SOI"
d.2013$YEAR <- "2013"

v2010.nccs <- "https://nccs-data.urban.org/dl.php?f=core/2010/nccs.core2010pc.csv"
d.2010.nccs <- read.csv( v2010.nccs )
d.2010 <- X2010.NCCS.RENAME( d.2010.nccs, ein="EIN"  )
d.2010$SOURCE <- "CORE"
d.2010$YEAR <- "2010"

v2007.nccs <- "https://nccs-data.urban.org/dl.php?f=core/2007/nccs.core2007pc.csv"
d.2007.nccs <- read.csv( v2007.nccs )
d.2007 <- X2007.NCCS.RENAME( d.2007.nccs, ein="EIN" )
d.2007$SOURCE <- "CORE"
d.2007$YEAR <- "2007"

v2004.nccs <- "https://nccs-data.urban.org/dl.php?f=core/2004/nccs.core2004pc.csv"
d.2004.nccs <- read.csv( v2004.nccs )
d.2004 <- X2004.NCCS.RENAME( d.2004.nccs, ein="EIN" )
d.2004$SOURCE <- "CORE"
d.2004$YEAR <- "2004"

v2001.nccs <- "https://nccs-data.urban.org/dl.php?f=core/2001/nccs.core2001pc.csv"
d.2001.nccs <- read.csv( v2001.nccs )
d.2001 <- X2001.NCCS.RENAME( d.2001.nccs, ein="EIN" )
d.2001$SOURCE <- "CORE"
d.2001$YEAR <- "2001"


d <- dplyr::bind_rows( d.2019, d.2016 )
d <- dplyr::bind_rows( d, d.2013 )
d <- dplyr::bind_rows( d, d.2010 )
d <- dplyr::bind_rows( d, d.2007 )
d <- dplyr::bind_rows( d, d.2004 )
d <- dplyr::bind_rows( d, d.2001 )





#####
##### Organization Sample
#####

url.orgs <- "https://www.dropbox.com/s/o11gud4t83tvn14/org-sample-full.csv?dl=1"
orgs <- read.csv( url.orgs )
orgs <- dplyr::select( orgs, orgname, ein )
orgs$ein <- gsub( "O", "0", orgs$ein )
orgs$ein <- gsub( "\n", "", orgs$ein )
orgs$ein <- gsub( "\t", "", orgs$ein )
orgs$ein <- gsub( "\\|", "", orgs$ein )
orgs$ein <- trimws( orgs$ein )
# grep( "[^0-9]", orgs$ein, value=TRUE )  # should be empty 
orgs$ein <- as.numeric( orgs$ein )
sample.ein <- unique( orgs$ein )


dd <- dplyr::filter( d, F9_00_ORG_EIN %in% sample.ein )


#####
##### Business Master File 
#####

bmf <- readRDS(gzcon(url( "https://www.dropbox.com/s/de004nt0h7n3v8c/bmf-master.rds?dl=1" )))
bmf$EIN <- as.numeric( bmf$EIN )
bmf <- dplyr::filter( bmf, EIN %in% sample.ein )

bmf <- dplyr::select( bmf, EIN, NAME, ADDRESS, CITY, STATE, FIPS,  
               RULEDATE, NTEEFINAL, NTEECC,
               NTEE1, NTMAJ5, NTMAJ10, NTMAJ12,	
               LEVEL1,	LEVEL2, LEVEL3, LEVEL4 )	

bmf$RULEDATE <- substr( bmf$RULEDATE, 1, 4 ) %>% as.numeric()
bmf$RULEDATE[ bmf$RULEDATE < 1900 ] <- NA
bmf$AGE <- 2022 - bmf$RULEDATE 

url.census <- "https://raw.githubusercontent.com/lecy/fiscal-health/main/01-data-raw/us-census-bureau-regions-and-divisions.csv"
census <- read.csv( url.census )
bmf <- merge( bmf, census, by.x="STATE", by.y="State.Code", all.x=TRUE )

bmf <- unique( bmf )

dd <- merge( dd, bmf, by.x="F9_00_ORG_EIN", by.y="EIN", all.x=TRUE )


#####
##### Save File 
#####

write.csv( dd, "PANEL-2001-2019.csv", row.names=F )








###############################
###############################
###############################


https://nccs-data.urban.org/dl.php?f=digitizeddata/digdata.revexp2005b.csv



