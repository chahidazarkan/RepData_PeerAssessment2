fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
temp <- tempfile()
download.file(fileUrl,temp, method="curl")
weather <- fread(sprintf("bzcat %s | tr -d '\\000'", temp, verbose=TRUE))
unlink(temp)