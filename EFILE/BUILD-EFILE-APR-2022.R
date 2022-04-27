library( dplyr )
library( irs990efile )

setwd( "C:/Users/jdlec/Dropbox (Personal)/00 - URBAN/github/fiscal-health/EFILE" )

index.cdev <- readRDS( "index-cdev.rds" )



dir.create( "TEMP" )
setwd( "./TEMP" )


years <- 2009:2020
failed.urls <- NULL

for( i in years )
{
  g <- dplyr::filter( index.cdev, TaxYear == i )
  groups <- split_index( g, 1000 )
  
  start_time <- Sys.time()
  f <- build_tables_parallel( groups=groups, year=i, cores=8 )
  end_time <- Sys.time()
  print( end_time - start_time )

  failed.urls <- c( failed.urls, f )

}


bind_data <- function( )
{
   file.names <- dir()
   # drop the dates from the end and combine years
   x <- substr( file.names, 6, nchar(file.names)-30 )
   table.names <- unique(x)
   table.names <- table.names[ ! table.names == "" ] 

   for( i in table.names )
   {
     these <- grepl( i, file.names ) & grepl( "*.rds", file.names )
     loop.list <- ( file.names )[ these ]

     d <- NULL

     for( j in loop.list )
     {
         d.j <- readRDS( j )
         d <- dplyr::bind_rows( d, d.j )
     }

     d <- unique(d)

     # drop the -time from table name 
     i <- substr( i, 1, nchar(i)-5 )
     
     write.csv( d, paste0( "../", i, ".csv" ), row.names=F )
     # saveRDS( d, paste0( year, "-", name.out, ".rds") )

   }
}



bind_data()


################
################



d <- read.csv( "01-data-raw/org-sample.csv" )

ein <- as.numeric( d$ein )




dir.create( "EFILE" )
setwd( "./EFILE" )


index <- readRDS( "index.rds" )


index$EIN <- as.numeric( index$EIN ) 

index.cdev <- filter( index, EIN %in% ein )

nrow( index.cdev )
# [1] 40999
length( unique( index.cdev$EIN ))
# [1] 5194






