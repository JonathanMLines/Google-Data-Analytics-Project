# Cyclistic Case Study
### Jonathan M Lines
### 20/02/2022

# Intro
## Stakeholders. About the company
Cyclistic is a successful bike-sharing business with a fleet of 5824 bicycles spread over a network of 692 stations across Chicago. The bikes can be unlocked from one station and returned to any other station in the system. Cyclistic has flexible price plans: single-ride passes, full-day passes, and annual memberships. Customers who purchase single-ride or full-day passes are referred to as ‘casual’ riders. Customers who purchase annual memberships are Cyclistic ‘members’.
## Business Task
Cyclistic’s annual members are much more profitable than casual riders. Moreno believes that maximizing the number of annual members will be key to future growth. Rather than creating a marketing campaign that targets all-new customers, Moreno believes there is a very good chance to convert casual riders into members. She notes that casual riders are already aware of the Cyclistic program and have chosen Cyclistic for their mobility needs.
## Project task:
### How do annual members and casual riders use Cyclistic bikes differently?

# Data
## Prepare
The data set contains bike trip info for all four quarters of the year 2019. A supplementary data set about bike stations might also be used. This is first party data with personally identifiable information removed.
## Process
### Install required packages

```r
library(tidyverse)  #Data wrangling
library(lubridate)  #Data wrangling
library(janitor)    #Cleaning
library(naniar)     #Cleaning (NA's)
library(ggplot2)    #Data visualization
```
### Defaults and style guide for graphs
```R
# Defaults
colour1 <- "#FAAB18"   # yellow
colourCasual <- "#4285F4"   # blue
colourMember <- "#FF8400"   # orange"
colourGrey <- "grey30"      # grey
colourTitle <- "#222222"    # blash-ish
# colourSubtitle <- ""
colourAxis <- "#333333"     # black-ish
font <- "Helvetica"

upright_style <- function() {
  ggplot2::theme(
    #Text format: title and subtitle
    plot.title = ggplot2::element_text(family = font,
                            size = 28,
                            face = "bold",
                            color = colourTitle),
    plot.title.position = "plot", 
    plot.subtitle = ggplot2::element_text(family = font,
                               size = 22,
                               margin = ggplot2::margin(9,0,9,0)),
    # Legend
    legend.position = "top",
    legend.text.align = 0,
    legend.title = ggplot2::element_blank(),
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(), 
    legend.text = element_text(size = 12),
    #Grid lines
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    axis.title = ggplot2::element_text(family = font,
                                      size = 18,
                                      color = colourAxis),
    axis.text = ggplot2::element_text(family = font,
                                      size = 18,
                                      color = colourAxis),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = -5, b = 10)),
    axis.line = ggplot2::element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    #Strip (facet-wrapped plots)
    strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(size  = 22,  hjust = 0),
  )
}

side_style <- function() {
  ggplot2::theme(
    #Text format: title and subtitle
    plot.title = ggplot2::element_text(family = font,
                            size = 28,
                            face = "bold",
                            color = colourTitle),
    #plot.title.position = "plot", 
    plot.subtitle = ggplot2::element_text(family = font,
                               size = 22,
                               margin = ggplot2::margin(9,0,9,0)),
   # plot.subtitle.position = "plot", 
    # Legend
    legend.position = "top",
    legend.text.align = 0,
    legend.title = ggplot2::element_blank(),
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(), 
    legend.text = element_text(size = 12),
    #Grid lines
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_line(color="#cbcbcb"),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    # axis
    axis.title = ggplot2::element_text(family = font,
                                      size = 18,
                                      color = colourAxis),
    axis.text = ggplot2::element_text(family = font,
                                      size = 18,
                                      color = colourAxis),
    axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = -5)),# l r t b 
    axis.line = ggplot2::element_blank(),
    axis.ticks.x = element_blank(), 
    axis.ticks.y = element_blank(),
    #Strip (facet-wrapped plots)
    strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(size  = 22,  hjust = 0)
  )
}

baseLine_style <- function() {
  geom_hline(yintercept = 0, size = 0.66, colour = colourAxis)
}

finalise_plot <- function(plot) {
  setwd("/Users/jonathanlines/Desktop/Case_Studies/Cyclistic")
  #ggsave(plot, width = width, height = height, units = "px")
  ggsave(plot)
  ## Return (invisibly) a copy of the graph. Can be assigned to a
  ## variable or silently ignored.
  #invisible(plot_grid)
}
```
### Load data
```R
#https://divvy-tripdata.s3.amazonaws.com/index.html
q1_2019 <- read_csv("2019/Divvy_Trips_2019_Q1.csv")
q2_2019 <- read_csv("2019/Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("2019/Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("2019/Divvy_Trips_2019_Q4.csv")
stations <- read_csv("2019/Divvy_Stations_2014-Q3Q4.csv")
```
### Check column names and data types
```R
# compare column names
data.frame(colnames(q1_2019),colnames(q2_2019),colnames(q3_2019),colnames(q4_2019))


##    colnames.q1_2019.                                colnames.q2_2019.
## 1            trip_id                    01 - Rental Details Rental ID
## 2         start_time             01 - Rental Details Local Start Time
## 3           end_time               01 - Rental Details Local End Time
## 4             bikeid                      01 - Rental Details Bike ID
## 5       tripduration 01 - Rental Details Duration In Seconds Uncapped
## 6    from_station_id                     03 - Rental Start Station ID
## 7  from_station_name                   03 - Rental Start Station Name
## 8      to_station_id                       02 - Rental End Station ID
## 9    to_station_name                     02 - Rental End Station Name
## 10          usertype                                        User Type
## 11            gender                                    Member Gender
## 12         birthyear         05 - Member Details Member Birthday Year
##    colnames.q3_2019. colnames.q4_2019.
## 1            trip_id           trip_id
## 2         start_time        start_time
## 3           end_time          end_time
## 4             bikeid            bikeid
## 5       tripduration      tripduration
## 6    from_station_id   from_station_id
## 7  from_station_name from_station_name
## 8      to_station_id     to_station_id
## 9    to_station_name   to_station_name
## 10          usertype          usertype
## 11            gender            gender
## 12         birthyear         birthyear
```

```R
# compare column data types
compare_df_cols(q1_2019, q2_2019, q3_2019, q4_2019, return = "mismatch")


## [1] column_name q1_2019     q2_2019     q3_2019     q4_2019    
## <0 rows> (or 0-length row.names)
```














