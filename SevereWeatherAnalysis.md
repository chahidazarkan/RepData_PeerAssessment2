# Severe weather events,  population health and economic consequences
Chahid Azarkan  

#Synopsis


#Introduction
Severe weather events, like storms, can cause problems in different communites and municipalities to both public health and the economy. Preventing the effect of severe weather events is a key concern for the federal, state and municipal governments.

In this analysis data is used from the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. In this database characteristics are tracked of major weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.

This analysis is answers the following two questions:

1. Across the United States, which types of events are most harmful with respect to population health?
2. Across the United States, which types of events have the greatest economic consequences?


#Data processing
The following libraries are used for this analyis:

```r
library(dplyr)
library(data.table)
library(xtable)
library(ggplot2)
```


To be able to carry out the analysis the data needs to be donwloaded and imported.

```r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
temp <- tempfile()
download.file(fileUrl,temp, method="curl")
weather <- fread(sprintf("bzcat %s | tr -d '\\000'", temp, verbose=TRUE))
```


Read 9.3% of 967216 rows
Read 35.2% of 967216 rows
Read 54.8% of 967216 rows
Read 75.5% of 967216 rows
Read 83.7% of 967216 rows
Read 902297 rows and 37 (of 37) columns from 0.523 GB file in 00:00:08

```r
unlink(temp)
```

Not all imported data is needed for the analysis, therefore a few columns need to be selected and named.

```r
data <- data.table(weather$STATE__, weather$BGN_DATE, weather$COUNTY, weather$STATE, weather$EVTYPE, weather$FATALITIES, weather$INJURIES, weather$PROPDMG, weather$PROPDMGEXP, weather$CROPDMG, weather$CROPDMGEXP, weather$REFNUM)

colnames(data) <- c("state.id", "date", "county", "state", "evtype", "fatalities", "injuries", "propdmg", "propdmg.exp", "cropdmg", "cropdmg.exp", "id")
```

To be able to make the right calculations, the data needs to be grouped by event type.


```r
data <- group_by(data, evtype)
```


#Data analysis
This analysis needs to answer the following two questions:

1. Across the United States, which types of events are most harmful with respect to population health?
2. Across the United States, which types of events have the greatest economic consequences?

## Most harmful events in respect to population health
To be able to determine the most harmful events in respect to population health we need to calculate the sum of the effect of certain event types on fatalities and injuries.

Furthermore, the mean value for both fatalities and injuries needs to be determined. This allows for a correction of the amount of occurence of an event type.  

```r
sum.fatalities <- arrange(summarise(data, sum.of.fatalities=sum(fatalities, na.rm = TRUE)), desc(sum.of.fatalities))
sum.injuries <- arrange(summarise(data, sum.of.injuries=sum(injuries, na.rm = TRUE)), desc(sum.of.injuries))
mean.fatalities <- arrange(summarise(data, mean.of.fatalities=mean(fatalities, na.rm = TRUE)), desc(mean.of.fatalities))
mean.fatalities$mean.of.fatalities <- round(mean.fatalities$mean.of.fatalities, digits=0)
mean.injuries <- arrange(summarise(data, mean.of.injuries=mean(injuries, na.rm = TRUE)), desc(mean.of.injuries))
mean.injuries$mean.of.injuries <- round(mean.injuries$mean.of.injuries, digits=0)
```

Below the top 5 of event types, that resulted in the highest fatalities, is given.

```r
mostfatalities <- sum.fatalities[1:5,]
knitr::kable(mostfatalities, col.names=c("Event type", "Total number of fatalities"), format ="html")
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> Event type </th>
   <th style="text-align:right;"> Total number of fatalities </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> TORNADO </td>
   <td style="text-align:right;"> 5633 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EXCESSIVE HEAT </td>
   <td style="text-align:right;"> 1903 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FLASH FLOOD </td>
   <td style="text-align:right;"> 978 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HEAT </td>
   <td style="text-align:right;"> 937 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LIGHTNING </td>
   <td style="text-align:right;"> 816 </td>
  </tr>
</tbody>
</table>


Below the top 5 of event types, that resulted in the highest mean fatalities, is given.

```r
mean.mostfatalities <- mean.fatalities[1:5,]
knitr::kable(mean.mostfatalities, col.names= c("Event type", "Mean number of fatalities"), format ="html")
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> Event type </th>
   <th style="text-align:right;"> Mean number of fatalities </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> TORNADOES, TSTM WIND, HAIL </td>
   <td style="text-align:right;"> 25 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> COLD AND SNOW </td>
   <td style="text-align:right;"> 14 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TROPICAL STORM GORDON </td>
   <td style="text-align:right;"> 8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RECORD/EXCESSIVE HEAT </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EXTREME HEAT </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
</tbody>
</table>


Below the top 5 of event types, that resulted in the highest injuries, is given.

```r
mostinjuries <- sum.injuries[1:5,]
knitr::kable(mostinjuries, col.names=c("Event type", "Total number of injuries"), format ="html")
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> Event type </th>
   <th style="text-align:right;"> Total number of injuries </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> TORNADO </td>
   <td style="text-align:right;"> 91346 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TSTM WIND </td>
   <td style="text-align:right;"> 6957 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FLOOD </td>
   <td style="text-align:right;"> 6789 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EXCESSIVE HEAT </td>
   <td style="text-align:right;"> 6525 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LIGHTNING </td>
   <td style="text-align:right;"> 5230 </td>
  </tr>
</tbody>
</table>


Below the top 5 of event types, that resulted in the highest mean injuries, is given.

```r
mean.mostinjuries <- mean.injuries[1:5,]
knitr::kable(mean.mostinjuries, col.names=c("Event type", "Mean number of injuries"), format ="html")
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> Event type </th>
   <th style="text-align:right;"> Mean number of injuries </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Heat Wave </td>
   <td style="text-align:right;"> 70 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TROPICAL STORM GORDON </td>
   <td style="text-align:right;"> 43 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> WILD FIRES </td>
   <td style="text-align:right;"> 38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> THUNDERSTORMW </td>
   <td style="text-align:right;"> 27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HIGH WIND AND SEAS </td>
   <td style="text-align:right;"> 20 </td>
  </tr>
</tbody>
</table>

Below a figure is given which shows the above stated tables in a barplot.

```r
par(mfrow=c(2,2))
barplot(height=mostfatalities$sum.of.fatalities, names.arg=mostfatalities$evtype, cex.names=.8, xlab="Event types", ylab = "Total amount of fatalities")
barplot(height=mean.mostfatalities$mean.of.fatalities, names.arg = mean.mostfatalities$evtype, cex.names=.8, xlab="Event types", ylab = "Mean of fatalities")
barplot(height=mostinjuries$sum.of.injuries, names.arg=mostinjuries$evtype, cex.names=.8, xlab="Event types", ylab = "Total amount of injuries")
barplot(height = mean.mostinjuries$mean.of.injuries, names.arg = mean.mostinjuries$evtype, cex.names=.8, xlab="Event types", ylab = "Mean of injuries")
```

![](SevereWeatherAnalysis_files/figure-html/plot health-1.png)








Does the analysis address the question of which types of events are most harmful to population health?







Does the analysis address the question of which types of events have the greatest economic consequences?


#Results







