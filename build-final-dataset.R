library( dplyr )


#####
##### Organization Sample
#####

url.orgs <- "https://www.dropbox.com/s/fqk05x2ox3girq7/org-sample.csv?dl=1"
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




#####
##### EFile Data  
#####

d.header         <- read.csv( "https://www.dropbox.com/s/ktegtmeytz4l3s1/F9-P00-T00-HEADER-2018.csv?dl=1" )
d.header$ORG_EIN <- as.numeric( d.header$ORG_EIN )
d.header         <- dplyr::filter( d.header, ORG_EIN %in% sample.ein )
 
d.summary  <- read.csv( "https://www.dropbox.com/s/7vslaxy1w7vumlw/F9-P01-T00-SUMMARY-2018.csv?dl=1" )
d.summary$ORG_EIN <- as.numeric( d.summary$ORG_EIN )
d.summary         <- dplyr::filter( d.summary, ORG_EIN %in% sample.ein )

d.revenue  <- read.csv( "https://www.dropbox.com/s/jnsotxyyj6ai8vl/F9-P08-T00-REVENUE-2018.csv?dl=1" )
d.revenue$ORG_EIN <- as.numeric( d.revenue$ORG_EIN )
d.revenue         <- dplyr::filter( d.revenue, ORG_EIN %in% sample.ein )

d.expenses <- read.csv( "https://www.dropbox.com/s/wem5panqhmt030x/F9-P09-T00-EXPENSES-2018.csv?dl=1" )
d.expenses$ORG_EIN <- as.numeric( d.expenses$ORG_EIN )
d.expenses         <- dplyr::filter( d.expenses, ORG_EIN %in% sample.ein )

d.balance  <- read.csv( "https://www.dropbox.com/s/1jlrdu1emeewc7o/F9-P10-T00-BALANCE-SHEET-2018.csv?dl=1" )
d.balance$ORG_EIN <- as.numeric( d.balance$ORG_EIN )
d.balance         <- dplyr::filter( d.balance, ORG_EIN %in% sample.ein )

d.assets   <- read.csv( "https://www.dropbox.com/s/utb9pngzscgzler/F9-P11-T00-ASSETS-2018.csv?dl=1" )
d.assets$ORG_EIN <- as.numeric( d.assets$ORG_EIN )
d.assets         <- dplyr::filter( d.assets, ORG_EIN %in% sample.ein )


d <- merge( d.header, d.summary, all.x=TRUE )
d <- merge( d, d.revenue, all.x=TRUE )
d <- merge( d, d.expenses, all.x=TRUE )
d <- merge( d, d.balance, all.x=TRUE )
d <- merge( d, d.assets, all.x=TRUE )

d <- merge( d, bmf, by.x="ORG_EIN", by.y="EIN", all.x=TRUE )
d <- unique( d )


#####
##### Add MSA 
#####


msa <- read.csv( "https://raw.githubusercontent.com/DS4PS/cpp-529-master/master/data/cbsatocountycrosswalk.csv",  stringsAsFactors=F, colClasses="character" )
keep <- c( "countyname", "state", "fipscounty", "msa", 
           "msaname", "cbsa", "cbsaname" )
msa <-  msa[keep] %>% unique()

msa$fipscounty <- as.numeric( msa$fipscounty )
d$FIPS <- as.numeric( d$FIPS )

d <- merge( d, msa, by.x="FIPS", by.y="fipscounty", all.x=TRUE )


d$SOURCE <- "EFILE"


#####
##### SOI Extract Cases 
##### 




core <- readRDS(gzcon(url( "https://www.dropbox.com/s/3ooiwpnems88ykz/core.rds?dl=1" )))

core <- dplyr::select( core, -NTMAJ12 )

core <-
  core %>% 
  dplyr::rename( 
          ORG_EIN = ein,
          RETURN_TYPE = tax.form, 
          TAX_YEAR = taxyr,
          F9_00_TAX_PERIOD_END_DATE = tax_pd,  
          F9_10_LIAB_ACC_PAYABLE_EOY = accntspayableend,
          F9_10_ASSET_ACC_NET_EOY = accntsrcvblend,
          F9_09_EXP_DEPREC_TOT = deprcatndepletn,
          F9_01_EXP_GRANT_SIMILAR_CY = grntspayableend,
          F9_10_ASSET_INV_SALE_EOY = invntriesalesend,
          F9_10_ASSET_LAND_BLDG_EOY = lndbldgsequipend,
          F9_10_ASSET_CASH_EOY = nonintcashend,
          F9_10_ASSET_PLEDGE_NET_EOY = pldgegrntrcvblend,
          F9_10_ASSET_EXP_PREPAID_EOY = prepaidexpnsend,
          F9_10_ASSET_CASH_SAVING_EOY = svngstempinvend,
          F9_10_ASSET_TOT_EOY = totassetsend,
          F9_01_EXP_TOT_CY = totfuncexpns,
          F9_10_LIAB_TOT_EOY = totliabend,
          F9_10_NAFB_TOT_EOY = totnetassetend,
          F9_08_REV_PROG_TOT_TOT = totprgmrevnue,
          F9_01_REV_TOT_CY = totrevenue,
          F9_10_NAFB_UNRESTRICT_EOY = unrstrctnetasstsend,
          F9_08_REV_CONTR_TOT = totcntrbgfts,
          F9_08_REV_OTH_FUNDR_NET_TOT = netincfndrsng,
          F9_08_REV_OTH_INVEST_INCOME_TOT = invstmntinc, 
          F9_08_REV_OTH_INVEST_BOND_TOT = txexmptbndsproceeds, 
          F9_08_REV_OTH_RENT_NET_TOT = netrntlinc, 
          F9_08_REV_OTH_SALE_GAIN_NET_TOT = netgnls, 
          F9_08_REV_OTH_ROY_TOT = royaltsinc, 
          F9_08_REV_OTH_INV_NET_TOT = netincsales, 
          F9_08_REV_MISC_TOT_TOT = miscrevtot11e,
          NTMAJ12 = NTMAJ12v1 )


add.these <- setdiff( unique( core$ORG_EIN ), unique( d$ORG_EIN )  )

core2 <- dplyr::filter( core, ORG_EIN %in% add.these )
core2$TAX_YEAR <- as.numeric( core2$TAX_YEAR )
core2$F9_00_TAX_PERIOD_END_DATE <- as.character( core2$F9_00_TAX_PERIOD_END_DATE )
core2$FIPS <- as.numeric(as.character(( core2$FIPS ))

core2$SOURCE <- "SOI"



d <- bind_rows( d, core2 )





d$NTEE1[ d$NTEE1 == "L" ] <- "Housing"
d$NTEE1[ d$NTEE1 == "P" ] <- "Human Services"
d$NTEE1[ d$NTEE1 == "S" ] <- "Community Development"
d$NTMAJ12 <- factor( d$NTMAJ12 )


table( d$SOURCE )
#  EFILE   SOI 
#   5508   791 

d <- filter( d, NTEE1 %in% c( "Housing", "Human Services", "Community Development") )

table( d$SOURCE )
#  EFILE   SOI 
#   4967   791

# library( fiscal )
d <- fiscal::get_dar( d, debt="F9_10_LIAB_TOT_EOY", assets="F9_10_ASSET_TOT_EOY" )


write.csv( d, "05-data-rodeo/final-dataset.csv" )
saveRDS( d, "05-data-rodeo/final-dataset.rds" )


# write.csv( d, "final-dataset.csv" )
# saveRDS( d, "final-dataset.rds" )

