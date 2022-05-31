d <- read.csv( "https://www.dropbox.com/s/ra5asy3rphwaw0w/panel-variables.csv?dl=1" )

head( d )

d[ is.na(d) ] <- ""



v1 <- unlist( d["E.File.Variables"] )
v2 <- unlist( d["X2019.SOI"] )
paste0( v1, " = ", v2, ", " )




filename <- "crosswalk-functions.R"
file.create( filename )
fileConn <- file( filename, open="a" )


nm <- names(d)

for( i in 5:11 )
{

  v1 <- unlist( d["E.File.Variables"] )
  v2 <- unlist( d[,i] )

  vars <- paste0( "    ", v1, " = ", v2, ", " )
  vars <- vars[ ! ( v1 == "" | v2 == "" ) ] 

  writeLines( paste0( nm[i], ".RENAME", " <- function(d){ " ), con = fileConn, sep = "\n\n" )
  writeLines( "  d <- dplyr::rename( ", con = fileConn, sep = "\n" )
  writeLines( vars, con = fileConn, sep = "\n" )
  writeLines( "}", con = fileConn, sep = "\n\n\n" )
 
}


close( fileConn ) 
shell( filename )



