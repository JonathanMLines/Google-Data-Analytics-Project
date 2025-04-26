# Cyclistic Case Study
### Jonathan M Lines

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

### Compare column data types

```R
compare_df_cols(q1_2019, q2_2019, q3_2019, q4_2019, return = "mismatch")
## [1] column_name q1_2019     q2_2019     q3_2019     q4_2019    
## <0 rows> (or 0-length row.names)
```

### Rename columns
```R
(q1_2019 <- rename(q1_2019
                   ,bike_id = bikeid
                   ,trip_duration = tripduration
                   ,birth_year = birthyear
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id))

(q2_2019 <- rename(q2_2019
                   ,trip_id = "01 - Rental Details Rental ID"
                   ,bike_id = "01 - Rental Details Bike ID" 
                   ,start_time = "01 - Rental Details Local Start Time"  
                   ,trip_duration = "01 - Rental Details Duration In Seconds Uncapped"  
                   ,end_time = "01 - Rental Details Local End Time"  
                   ,start_station_name = "03 - Rental Start Station Name" 
                   ,start_station_id = "03 - Rental Start Station ID"
                   ,end_station_name = "02 - Rental End Station Name" 
                   ,end_station_id = "02 - Rental End Station ID"
                   ,gender = "Member Gender" 
                   ,birth_year = "05 - Member Details Member Birthday Year"
                   ,usertype = "User Type"))

(q3_2019 <- rename(q3_2019
                   ,bike_id = bikeid
                   ,trip_duration = tripduration
                   ,birth_year = birthyear
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id))

(q4_2019 <- rename(q4_2019
                   ,bike_id = bikeid
                   ,trip_duration = tripduration
                   ,birth_year = birthyear
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id))
```

### Compare columns names

```R
data.frame(colnames(q1_2019),colnames(q2_2019),colnames(q3_2019),colnames(q4_2019))


##     colnames.q1_2019.  colnames.q2_2019.  colnames.q3_2019.  colnames.q4_2019.
## 1             trip_id            trip_id            trip_id            trip_id
## 2          start_time         start_time         start_time         start_time
## 3            end_time           end_time           end_time           end_time
## 4             bike_id            bike_id            bike_id            bike_id
## 5       trip_duration      trip_duration      trip_duration      trip_duration
## 6    start_station_id   start_station_id   start_station_id   start_station_id
## 7  start_station_name start_station_name start_station_name start_station_name
## 8      end_station_id     end_station_id     end_station_id     end_station_id
## 9    end_station_name   end_station_name   end_station_name   end_station_name
## 10           usertype           usertype           usertype           usertype
## 11             gender             gender             gender             gender
## 12         birth_year         birth_year         birth_year         birth_year
```

### Are column names all the same ?

```R
compare_df_cols_same(q1_2019, q2_2019, q3_2019, q4_2019)
## [1] TRUE
```

### Compare column data types

```R
compare_df_cols(q1_2019, q2_2019, q3_2019, q4_2019, return = "mismatch")
## [1] column_name q1_2019     q2_2019     q3_2019     q4_2019    
## <0 rows> (or 0-length row.names)
```

### Combine data frames

```R
total_trips <- rbind(q1_2019, q2_2019, q3_2019, q4_2019)
```

### Change data types

```R
total_trips <- mutate(total_trips, trip_id = as.integer(trip_id),
                      bike_id = as.integer(bike_id),
                      start_station_id = as.integer(start_station_id),
                      end_station_id = as.integer(end_station_id),
                      usertype = as.factor(usertype),
                      gender = as.factor(gender))
# summary(total_trips)
```

### Check station name spelling. What station is in end_station_name but not start_station_name coloumns ?

```R
start_stations <- select(total_trips, start_station_name) |>
  mutate(start_station_name = tolower(start_station_name)) |>
  group_by(start_station_name) |>
  summarise(no_rows = length(start_station_name)) |>
  arrange(no_rows) |>
  select(station_name = start_station_name)
# A tibble: 640 × 1

end_stations <- select(total_trips, end_station_name) |>
  mutate(end_station_name = tolower(end_station_name)) |>
  group_by(end_station_name) |>
  summarise(no_rows = length(end_station_name)) |>
  arrange(no_rows) |>
  select(station_name = end_station_name)
# A tibble: 641 × 1

start_id <- select(total_trips, start_station_name, start_station_id) |>
  group_by(start_station_name) |>
  summarise(no_rows = length(start_station_name))
# A tibble: 640 × 2

end_id <- select(total_trips, end_station_name, end_station_id) |>
  group_by(end_station_name) |>
  summarise(no_rows = length(end_station_name))
# A tibble: 641 × 2

anti_join(start_stations, end_stations, by = "station_name")

anti_join(end_stations, start_stations, by = "station_name")

## # A tibble: 1 × 1
##   station_name            
##   <chr>                   
## 1 ts ~ divvy parts testing
```

### Check age range. Someone was born on 1759

```R
ages <- select(total_trips, birth_year) |>
  mutate(age = 2019 - birth_year) 

ggplot(ages, aes(x = age)) +
  geom_histogram(binwidth = 10, na.rm = TRUE, colour = "white", fill = colourGrey) +
  upright_style() +
  baseLine_style() +
  labs(title = "Age of riders",
       subtitle = "5 to 260 years old")
```

>
> graph here
>

```R
arrange(ages, age)
## # A tibble: 3,818,004 × 2
##    birth_year   age
##         <dbl> <dbl>
##  1       2014     5
##  2       2014     5
##  3       2014     5
##  4       2014     5
##  5       2014     5
##  6       2003    16
##  7       2003    16
##  8       2003    16
##  9       2003    16
## 10       2003    16
## # … with 3,817,994 more rows

arrange(ages, -age)
## # A tibble: 3,818,004 × 2
##    birth_year   age
##         <dbl> <dbl>
##  1       1759   260
##  2       1790   229
##  3       1888   131
##  4       1888   131
##  5       1888   131
##  6       1888   131
##  7       1888   131
##  8       1888   131
##  9       1888   131
## 10       1888   131
## # … with 3,817,994 more rows

highAgeRange <- filter(ages, age > 65 & age < 85)

ggplot(highAgeRange, aes(x = age)) +
  geom_histogram(binwidth = 1, colour = "white", fill = colourGrey) +
  upright_style() +
  baseLine_style() +
  scale_x_continuous(limits = c(65, 85),
                     breaks = seq(65, 85, by = 5),
                     labels = c("65", "70", "75", "80", "90")) +
  labs(title = "Age of riders",
       subtitle = "65 to years old")
```

>
> graph here
>

```R
dim(filter(ages, age > 75))
## [1] 1954    2

dim(filter(ages, age < 16))
## [1] 5 2
```

### 1954 riders have ages between 76 and 260. 5 riders are 5 years old. This is incorrect data. Replace false birth_year data with NA’s and gender NA’s with Unknown.

```R
trips.cleaned <- total_trips |>
  mutate_at(vars(birth_year),
            function(.var){
              if_else(.var < 1944 | .var > 2003,
                      true = as.numeric(NA),
                      false = .var)
            }) |>
  mutate(gender = fct_explicit_na(gender, na_level = "Unknown"))
# summary(trips.cleaned)
```

## Process

### Tidy up columns. Covert trip_duration from seconds to minutes. Age is more useful than birth_year. Split start_time into month weekday and hour. Knowing if trip is over more than one day might be useful. Change usertype factors to Member and Casual.

```R
trips.cleaned <- trips.cleaned |>
  mutate(trip_duration = as.integer(trip_duration / 60)) |> # sec to min
  mutate(age = 2019 - birth_year) |>
  mutate(month = month(start_time)) |>
  mutate(weekday = wday(start_time, TRUE, TRUE)) |> # (dateTime, label, abbr)
  mutate(hour = hour(start_time)) |>
  mutate(returned_same_day = ifelse(wday(start_time) == wday(end_time), TRUE, FALSE)) |>
  mutate(usertype = as.factor(ifelse(usertype == "Subscriber", "Member", "Casual")))
        
# summary(trips.cleaned)
```

## Discriptive analysis

### Compare members and casual users

```R
aggregate(trips.cleaned$trip_duration ~ trips.cleaned$usertype, FUN = mean)
##   trips.cleaned$usertype trips.cleaned$trip_duration
## 1                 Casual                    56.52344
## 2                 Member                    13.83169

aggregate(trips.cleaned$trip_duration ~ trips.cleaned$usertype, FUN = median)
##   trips.cleaned$usertype trips.cleaned$trip_duration
## 1                 Casual                          25
## 2                 Member                           9

# plotting graph
pie <- trips.cleaned |>
  group_by(usertype) |>
  summarise(number_trips = n()) |>
  mutate(percentage = round(number_trips / sum(number_trips), 3))
pie
## # A tibble: 2 × 3
##   usertype number_trips percentage
##   <fct>           <int>      <dbl>
## 1 Casual         880637      0.231
## 2 Member        2937367      0.769
```

```R
pie |>
  ggplot(aes(x="", y=percentage, fill=usertype)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start = 0) +
  theme_minimal() +
  #upright_style() +
  labs(x ="", y = "") +#, title = "Distribution of trips") +
  geom_text(aes(label = scales::percent(percentage, accuracy = 1.0, trim = FALSE)),
            position = position_stack(vjust = 0.5)) +
  geom_label(aes(label = glue::glue("{usertype}\n{scales::percent(percentage)}")),
                  position = position_stack(vjust = 0.5),
             label.size = NA,
             size = 6) +
  scale_fill_manual(values = c(colourCasual, colourMember)) +
  theme(plot.title = element_text(family ="Helvetica",
                                  size = 28,
                                  face = "bold",
                                  colour = "#222222"),
        plot.title.position = "plot", 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        legend.position = "none")
```

![data graph](assets/Disribution_of_trips.png)

### analyze ridership data by type and weekday

```R
trips_by_day <- trips.cleaned |> 
  group_by(weekday, usertype) |>  # groups by usertype and weekday
  summarise(number_of_rides = n()   # number of rides 
            ,average_duration = mean(trip_duration)) |>
  arrange(weekday, usertype)
```

### Visualize the number of rides per day

```R
trips_by_day |>
  ggplot(aes(x = weekday, y = number_of_rides, fill = usertype)) +
  upright_style() +
  geom_bar(stat = "identity", position = "dodge") +
  baseLine_style() +
  scale_fill_manual(values = c(colourCasual, colourMember)) +
  labs(title = "  ",#Riders per day",
      x = "", y = "Number of riders (100,000)") +
  theme(legend.position = c(0.1, 1.05)) +
  guides(fill = guide_legend(ncol = 2)) +
  scale_y_continuous(limits = c(0, 500000),
                     breaks = seq(0, 500000, by = 100000),
                     labels = c("0", "1", "2", "3", "4", "5"))
```

![data graph](assets/Riders_per_day.png)

### Visualization for average duration per day

```R
trips_by_day |>
  ggplot(aes(x = weekday, y = average_duration, fill = usertype)) +
  upright_style() +
  geom_col(position = "dodge") +
  baseLine_style() +
  scale_fill_manual(values = c(colourCasual, colourMember)) +
  labs(title = "  ",#Average trip duration",
       x = "", y = "Time (minutes)") +
  theme(legend.position = c(0.1, 1.05)) +
  guides(fill = guide_legend(ncol = 2)) +
  scale_y_continuous(limits = c(0, 60),
                     breaks = seq(0, 60, by = 20),
                     labels = c("0", "20", "40", "60"))
```

![data graph](assets/Avg_trip_duration.png)

### Plot summary statistics of trip duration per day

```R
trips.cleaned |>
  ggplot(aes(x = weekday, y = trip_duration, fill = usertype)) +
  geom_boxplot(outlier.colour = "light grey", outlier.size = 0.25) +
  upright_style() +
  coord_cartesian(ylim = c(0, 50)) +
  baseLine_style() +
  labs(title="Trip duration statistics",
       x = "", y = "Time (minutes)") +
  theme(legend.position = c(0.8, 1.1)) +
  guides(fill = guide_legend(ncol = 2)) +
  scale_fill_manual(values = c(colourCasual, colourMember))
```

## Explorative analysis

### Analyze ridership data by type and weekday

```R
trips_by_hour <- trips.cleaned |> 
  group_by(hour, usertype) |>  # groups by hour of day and usertype
  summarise(number_of_rides = n()   # number of rides 
            ,average_duration = mean(trip_duration)) |>
  arrange(hour, usertype)

ggplot(trips_by_hour, aes(x = hour, y = number_of_rides, colour = usertype)) +
  geom_line(size = 1) +
  upright_style() +
  theme(legend.position = "none") +
  scale_colour_manual(values = c(colourCasual, colourMember)) +
  scale_x_continuous(limits = c(0, 25), 
                     breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24),
                     labels = c("Midnight", "3", "6", "9", "12", "15", "18", "21", "Midnight")) +
  scale_y_continuous(limits = c(0, 400000), breaks = seq(0, 400000, by = 100000),
                     labels = c("0", "1", "2", "3", "4")) +
  baseLine_style() +
  labs(title="Daily bike rides",
       subtitle = "During full week",
       x = "", y = "Number of bike rides (100,000s)") +
  geom_label(aes(x = 23.25, y = 15000, label = "Casual"), 
             hjust = 0, 
             vjust = 0.5, 
             colour = colourCasual, 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             label.padding = unit(0.1, "lines"), 
             size = 3.25) +
  geom_label(aes(x = 23.25, y = 35000, label = "Member"), 
             hjust = 0, 
             vjust = 0.5, 
             colour = colourMember, 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             label.padding = unit(0.1, "lines"), 
             size = 3.25)
```

![data graph](assets/Weekday_rides.png)

```R
trips_by_hour_on_weekend <- trips.cleaned |> 
  filter(weekday == "Sun" | weekday == "Sat") |>
  group_by(hour, usertype) |>  # groups by usertype and weekday
  summarise(number_of_rides = n()   # number of rides 
            ,average_duration = mean(trip_duration)) |>
  arrange(hour, usertype)

ggplot(trips_by_hour_on_weekend, aes(x = hour, y = number_of_rides, colour = usertype)) +
  geom_line(size = 1) +
  upright_style() +
  theme(legend.position = "none") +
  baseLine_style() +
  scale_x_continuous(limits = c(0, 25), breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24, 27),
                     labels = c("Midnight", "3", "6", "9", "12", "15", "18", "21", "Midnight", " ")) +
  scale_y_continuous(limits = c(0, 50000), breaks = seq(0, 50000, by = 10000),
                     labels = c("0", "10", "20", "30", "40", "50")) +
  scale_colour_manual(values = c(colourCasual, colourMember)) +
  labs(#title="Daily bike rides",
       #subtitle = "During the weekend",
        x = "", y = "Number of bike rides (1000s)") +
  geom_label(aes(x = 23.25, y = 5500, label = "Casual"), 
             hjust = 0, 
             vjust = 0.5, 
             colour = colourCasual, 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             label.padding = unit(0.1, "lines"), 
             size = 3.25) +
  geom_label(aes(x = 23.25, y = 8000, label = "Member"), 
             hjust = 0, 
             vjust = 0.5, 
             colour = colourMember, 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             label.padding = unit(0.1, "lines"), 
             size = 3.25)
```

![data graph](assets/Weekday_rides.png)


```R
trips_by_hour_on_week <- trips.cleaned |> 
  filter(weekday != "Sun" & weekday != "Sat") |>
  #filter(weekday == "Mon" | weekday == "Tue" | weekday == "Wed" | weekday == "Thu" | weekday == "Fri") |>
  group_by(hour, usertype) |>
  summarise(number_of_rides = n()   
            ,average_duration = mean(trip_duration)) |>
  arrange(hour, usertype)

ggplot(trips_by_hour_on_week, aes(x = hour, y = number_of_rides, colour = usertype)) +
  geom_line(size = 1) +
  upright_style() +
  theme(legend.position = "none") +
  baseLine_style() +
  scale_x_continuous(limits = c(0, 25), breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24),
                     labels = c("Midnight", "3", "6", "9", "12", "15", "18", "21", "Midnight")) +
  scale_y_continuous(limits = c(0, 400000), breaks = seq(0, 400000, by = 100000),
                     labels = c("0", "1", "2", "3", "4")) +
  scale_colour_manual(values = c(colourCasual, colourMember)) +
  labs(#title="Daily bike rides",
       #subtitle = "During week days",
       x = "", y = "Number of bike rides (100,000s)") +
    geom_label(aes(x = 23.25, y = 15000, label = "Casual"), 
             hjust = 0, 
             vjust = 0.5, 
             colour = colourCasual, 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             label.padding = unit(0.1, "lines"), 
             size = 3.25) +
  geom_label(aes(x = 23.25, y = 35000, label = "Member"), 
             hjust = 0, 
             vjust = 0.5, 
             colour = colourMember, 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             label.padding = unit(0.1, "lines"), 
             size = 3.25)

#finalise_plot(plot = "Weekday_rides.png")
```
![data graph](assets/Weekday_rides.png)

### Analyze ridership data by type and weekday

```R
trips_by_month <- trips.cleaned |> 
  group_by(month, usertype) |>  # groups by usertype and weekday
  summarise(number_of_rides = n()   # number of rides 
            ,average_duration = mean(trip_duration)) |>
  arrange(month, usertype)

ggplot(trips_by_month, aes(x = month, y = number_of_rides, colour = usertype)) +
  geom_line(size = 1) +
  upright_style() +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(1, 13), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug","Sep", "Oct", "Nov", "Dec", "")) +
  scale_y_continuous(limits = c(0, 450000), breaks = seq(0, 450000, by = 100000),
                     labels = c("0", "1", "2", "3", "4")) +
  baseLine_style() +
  scale_colour_manual(values = c(colourCasual, colourMember)) +
  labs(#title="Number of riders per month",
       y = "Number of rides (100,000s)", x = "") +
    geom_label(aes(x = 12.25, y = 20000, label = "Casual"), 
             hjust = 0, 
             vjust = 0.5, 
             colour = colourCasual, 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             label.padding = unit(0.1, "lines"), 
             size = 3.25) +
  geom_label(aes(x = 12.25, y = 140000, label = "Member"), 
             hjust = 0, 
             vjust = 0.5, 
             colour = colourMember, 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             label.padding = unit(0.1, "lines"), 
             size = 3.25)
```

![data graph](assets/Monthly_rides.png)

### analyze ridership data by type and weekday

```R
trips_by_month_weekend <- trips.cleaned |> 
  filter(weekday == "Sun" | weekday == "Sat") |>
  group_by(month, usertype) |>  # groups by usertype and weekday
  summarise(number_of_rides = n()   # number of rides 
            ,average_duration = mean(trip_duration)) |>
  arrange(month, usertype)

ggplot(trips_by_month_weekend, aes(x = month, y = number_of_rides, colour = usertype)) +
  geom_line(size = 1) +
  upright_style() +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(1, 13), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug","Sep", "Oct", "Nov", "Dec", "")) +
  scale_y_continuous(limits = c(0, 450000), breaks = seq(0, 450000, by = 100000),
                     labels = c("0", "1", "2", "3", "4")) +
  baseLine_style() +
  scale_colour_manual(values = c(colourCasual, colourMember)) +
  labs(title="Number of riders per month",
       subtitle = "During weekends",
       y = "Number of rides (100,000s)", x = "") +
  geom_label(aes(x = 12.25, y = 14000, label = "Casual"), 
             hjust = 0, 
             vjust = 0.5, 
             colour = colourCasual, 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             label.padding = unit(0.1, "lines"), 
             size = 3.25) +
  geom_label(aes(x = 12.25, y = 32000, label = "Member"), 
             hjust = 0, 
             vjust = 0.5, 
             colour = colourMember, 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             label.padding = unit(0.1, "lines"), 
             size = 3.25)
```





# Foo
