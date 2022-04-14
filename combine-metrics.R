
setwd( "fiscal-health/03-data-ratios" )

x <- dir()
rds.files <- x[ grepl( ".rds", x ) ]


d <- readRDS( rds.files[1] )
d$id <- paste0( d$ein, "-", d$tax_pd )
d <- d[ ! duplicated(d$id) , ]

for( i in rds.files[-1] )
{
  d.temp <- readRDS(i)
  d.temp$id <- paste0( d.temp$ein, "-", d.temp$tax_pd )
  d.temp <- d.temp[ ! duplicated(d.temp$id) , ]

  d <- merge( d, d.temp, by=c("id","ein","tax_pd"), all.x=T )
}


write.csv( d, "ALL-METRICS.csv", row.names=F )
saveRDS( d, "ALL-METRICS.rds" )






