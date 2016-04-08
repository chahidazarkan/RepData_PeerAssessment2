temp <- "repdata-data-StormData.csv.bz2"

weather <- fread(sprintf("bzcat %s | tr -d '\\000'", temp, verbose=TRUE))