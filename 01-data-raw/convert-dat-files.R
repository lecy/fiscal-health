dat1 <- read.csv( "irs-990-pc-soi-extract-2016-DAT.dat", sep=" ", skip=1 )
raw.names <- readLines( "irs-990-pc-soi-extract-2016-DAT.dat", n=1 )
dat.names <- strsplit( raw.names, " ")[[1]]
names( dat1 ) <- dat.names
head( dat1 )
write.csv( dat1, "irs-990-pc-soi-extract-2016.csv", row.names=F )

dat1 <- read.csv( "irs-990-pc-soi-extract-2013-DAT.dat", sep=" ", skip=1 )
raw.names <- readLines( "irs-990-pc-soi-extract-2013-DAT.dat", n=1 )
dat.names <- strsplit( raw.names, " ")[[1]]
names( dat1 ) <- dat.names
head( dat1 )
write.csv( dat1, "irs-990-pc-soi-extract-2013.csv", row.names=F )




dat1 <- read.csv( "irs-990-ez-soi-extract-2016-DAT.dat", sep=" ", skip=1 )
raw.names <- readLines( "irs-990-ez-soi-extract-2016-DAT.dat", n=1 )
dat.names <- strsplit( raw.names, " ")[[1]]
names( dat1 ) <- dat.names
head( dat1 )
write.csv( dat1, "irs-990-ez-soi-extract-2016.csv", row.names=F )
