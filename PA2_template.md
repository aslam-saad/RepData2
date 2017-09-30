<font color = "#982446" face = Times New Roman>Introduction</font>
------------------------------------------------------------------

Storms and other severe weather events can cause both public health and economic problems for communities and municipalities.
Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

This project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.

<font color = "#982446" face = Times New Roman>Data</font>
----------------------------------------------------------

The data for this assignment come in the form of a comma-separated-value file compressed via the bzip2 algorithm to reduce its size.
You can download the file from the course web site:
\* [Storm Data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2) \[47Mb\]

There is also some documentation of the database available. Here you will find how some of the variables are constructed/defined.
\* National Weather Service [Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)
\* National Climatic Data Center Storm Events [FAQ](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf)

The events in the database start in the year 1950 and end in November 2011. In the earlier years of the database there are generally fewer events recorded, most likely due to a lack of good records. More recent years should be considered more complete.

<font color = "#982446" face = Times New Roman>Assignment</font>
----------------------------------------------------------------

The basic goal of this assignment is to explore the NOAA Storm Database and answer some basic questions about severe weather events.
You must use the database to answer the questions below and show the code for your entire analysis. Your analysis can consist of tables, figures, or other summaries. You may use any R package you want to support your analysis.

<font color = "#982446" face = Times New Roman>Questions</font>
---------------------------------------------------------------

<font color = "#354678" face = Times New Roman size = 4.5px>1- Across the United States, which types of events (as indicated in the <font color = "red">EVTYPE</font> variable) are most harmful with respect to population health?</font>

<font color = "#354678" face = Times New Roman size = 4.5px>2- Across the United States, which types of events have the greatest economic consequences ?</font>

<font color = "#982446" face = Times New Roman>loading required libraries</font>
--------------------------------------------------------------------------------

``` r
library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
```

<font color = "#982446" face = Times New Roman>Importing data</font>
--------------------------------------------------------------------

``` r
# download the file if it's not exist
if(!file.exists("storm.csv")) {
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
  destfile = "storm.csv.bz2")
    }

storm_data <- read_csv("storm.csv")
```

<font color = "#982446" face = Times New Roman>Data Processing</font>
---------------------------------------------------------------------

``` r
storm_data <- storm_data %>%
    mutate(PROPDMGNUM = ifelse(is.na(PROPDMGEXP) , 1, 
                              ifelse(PROPDMGEXP %in% c("H", "h"), 1e2,
                                    ifelse(PROPDMGEXP == "K", 1e3,
                                          ifelse(PROPDMGEXP %in% c("m", "M"), 1e6,
                                                ifelse(PROPDMGEXP == "B", 1e9,
                                                      ifelse(is.na(parse_integer(PROPDMGEXP)), 0, PROPDMGEXP)
                                                    ))))),
           CROPDMGNUM = ifelse(is.na(CROPDMGEXP), 1, 
                              ifelse(CROPDMGEXP %in% c("H", "h"), 1e2,
                                    ifelse(CROPDMGEXP == "K", 1e3,
                                          ifelse(CROPDMGEXP %in% c("m", "M"), 1e6,
                                                ifelse(CROPDMGEXP == "B", 1e9,
                                                      ifelse(is.na(parse_integer(CROPDMGEXP)), 0, CROPDMGEXP)
                                                    )))))) %>%
    mutate(PROPDMGNUM = parse_double(PROPDMGNUM), CROPDMGNUM = parse_double(CROPDMGNUM), 
           PROPVAL = PROPDMG * PROPDMGNUM, CROPVAL = CROPDMG * CROPDMGNUM, DMGVAL = PROPVAL+CROPVAL) 
```

<font color = "#982446" face = Times New Roman>Results</font>
-------------------------------------------------------------

<font color = "#354678" face = Times New Roman size = 4.5px>1- Most harmful types of events (with respect to population health)</font>

<font color = "#112467" face = Times New Roman size = 4px>*Top number of injuries caused by weather events*</font>

``` r
storm_data %>%
    group_by(EVTYPE) %>%
    summarise(FATALITIES = sum(FATALITIES)) %>%
    arrange(desc(FATALITIES)) %>%
    filter(row_number() < 11)
```

    ## # A tibble: 10 x 2
    ##            EVTYPE FATALITIES
    ##             <chr>      <dbl>
    ##  1        TORNADO       5633
    ##  2 EXCESSIVE HEAT       1903
    ##  3    FLASH FLOOD        978
    ##  4           HEAT        937
    ##  5      LIGHTNING        816
    ##  6      TSTM WIND        504
    ##  7          FLOOD        470
    ##  8    RIP CURRENT        368
    ##  9      HIGH WIND        248
    ## 10      AVALANCHE        224

<font color = "#112467" face = Times New Roman size = 4px>*Plotting of top injuries caused by weather events*</font>

``` r
storm_data %>% 
    group_by(EVTYPE) %>%
    summarise(FATALITIES = sum(FATALITIES)) %>%
    arrange(desc(FATALITIES)) %>%
    filter(row_number() < 11)%>%
    ggplot(aes(x = reorder(EVTYPE, desc(FATALITIES)), y = FATALITIES, fill = desc(FATALITIES))) + 
    geom_bar(stat = "identity") +
    geom_label(aes(label = FATALITIES), color = "#ffffff", size = 3.5)+
    scale_y_continuous(breaks = seq(0, 6000, by = 500)) +
    labs(x = "EVENT TYPE", y = "FATALITIES", 
         title = "Most Harmful Types of Weather Events" ) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = .5), 
         axis.title.x = element_text(color = "#113467", size = 15),
         axis.title.y = element_text(color = "#113467", size = 15))
```

![](figure/WEATHER%20FATALITIES-1.png)

<font color = "#112467" face = Times New Roman size = 4px>*Top number of injuries caused by weather events*</font>

``` r
storm_data %>%
    group_by(EVTYPE) %>%
    summarise(INJURIES = sum(INJURIES)) %>%
    arrange(desc(INJURIES)) %>%
    filter(row_number() < 11)
```

    ## # A tibble: 10 x 2
    ##               EVTYPE INJURIES
    ##                <chr>    <dbl>
    ##  1           TORNADO    91346
    ##  2         TSTM WIND     6957
    ##  3             FLOOD     6789
    ##  4    EXCESSIVE HEAT     6525
    ##  5         LIGHTNING     5230
    ##  6              HEAT     2100
    ##  7         ICE STORM     1975
    ##  8       FLASH FLOOD     1777
    ##  9 THUNDERSTORM WIND     1488
    ## 10              HAIL     1361

<font color = "#112467" face = Times New Roman size = 4px>*Plotting of top injuries caused by weather events*</font>

``` r
storm_data %>% 
    group_by(EVTYPE) %>%
    summarise(INJURIES = sum(INJURIES)) %>%
    arrange(desc(INJURIES)) %>%
    filter(row_number() < 11)%>%
    ggplot(aes(x = reorder(EVTYPE, desc(INJURIES)), y = INJURIES, fill = desc(INJURIES))) + 
    geom_bar(stat = "identity") +
    geom_label(aes(label = INJURIES), color = "#ffffff", size = 2.5)+
    scale_y_continuous(breaks = seq(0, 91500, by = 10000)) +
    labs(x = "EVENT TYPE", y = "INJURIES", 
         title = "Most Harmful Types of Weather Events" ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = .5), 
         axis.title.x = element_text(color = "#113467", size = 15),
         axis.title.y = element_text(color = "#113467", size = 15))
```

![](figure/WEATHER%20INJURIES-1.png)

<font color = "#354678" face = Times New Roman size = 4.5px>2- types of events have the greatest economic consequences)</font>

``` r
storm_data %>%
    group_by(EVTYPE) %>%
    summarise(DMGVAL = sum(DMGVAL)) %>%
    arrange(desc(DMGVAL)) %>%
    filter(row_number() < 11) %>%
    mutate(DMGVAL = str_c("$", str_extract(as.character(DMGVAL/1e9),"^[0-9]+\\.[0-9]"), "bn", sep =""))
```

    ## # A tibble: 10 x 2
    ##               EVTYPE   DMGVAL
    ##                <chr>    <chr>
    ##  1             FLOOD $150.3bn
    ##  2 HURRICANE/TYPHOON  $71.9bn
    ##  3           TORNADO  $57.3bn
    ##  4       STORM SURGE  $43.3bn
    ##  5              HAIL  $18.7bn
    ##  6       FLASH FLOOD  $17.5bn
    ##  7           DROUGHT  $15.0bn
    ##  8         HURRICANE  $14.6bn
    ##  9       RIVER FLOOD  $10.1bn
    ## 10         ICE STORM   $8.9bn

``` r
storm_data %>%
    group_by(EVTYPE) %>%
    summarise(DMGVAL = sum(DMGVAL)) %>%
    arrange(desc(DMGVAL)) %>%
    filter(row_number() < 11) %>%
    ggplot(aes(x = reorder(EVTYPE, desc(DMGVAL)), y = round(DMGVAL/1e9), fill = desc(DMGVAL))) + 
    geom_bar(stat = "identity") +
    geom_label(aes(label = round(DMGVAL/1e9)), color = "#ffffff", size = 3.5)+
    scale_y_continuous(breaks = seq(0, 150, by = 10)) +
    labs(x = "EVENT TYPE", y = "COST(In Billions)", 
         title = "Most Harmful Types of Weather Events" ) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = .5), 
         axis.title.x = element_text(color = "#113467", size = 15),
         axis.title.y = element_text(color = "#113467", size = 15),
         legend.position = "none")
```

![](figure/DMG%20COST-1.png)
