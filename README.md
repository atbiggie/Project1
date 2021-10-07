How to Access an API with RStudio
================
Autumn Biggie
10/3/2021

  - [Preliminary Steps](#preliminary-steps)
  - [Function to Access API](#function-to-access-api)
  - [Exploratory Data Analysis](#exploratory-data-analysis)
      - [Data Cleaning](#data-cleaning)
      - [Create New Variables](#create-new-variables)
      - [Contingency Tables](#contingency-tables)
      - [Numerical Summaries](#numerical-summaries)
      - [Data Visualization](#data-visualization)
  - [Final Thoughts](#final-thoughts)

In this document, we’ll walk through how to connect to an API, using an
example. The API we’ll be connecting to is the [OpenWeather
API](https://openweathermap.org/api). Specifically, we’ll be looking at
current and forecast weather data from the [One Call
API](https://openweathermap.org/api/one-call-api#history), one of the
many APIs that openweathermap.org offers.

## Preliminary Steps

1.  Many APIs require you to access the data using a unique API key. To
    acquire your free API key, register
    [here](https://home.openweathermap.org/users/sign_up).

2.  The following packages will be necessary in order to connect with
    the API:
    
      - `httr`  
      - `jsonlite`

3.  The following packages will be necessary in order to do some
    analyses after we load the data:
    
      - `tidyverse`  
      - `anytime`  
      - `ggplot2`
      - `chron`
      - `leaflet`

## Function to Access API

Here, I’ve written a function `weather.api` to be able to easily access
the target API. The arguments are as follows:

  - `latitude`(required): latitude of the geographic location desired  
  - `longitude`(required): longitude of the geographic location
    desired  
  - `api.id`(required): input your unique API key here  
  - `exclude`(optional): list any parts of the weather data you want to
    exclude from the results. This should be a comma-delimited list,
    with or without spaces. Case is unimportant. The options are:
      - `current`  
      - `minutely`  
      - `hourly`  
      - `daily`  
  - `units` (optional): The unit of measurement in which values are
    returned. Options are:
      - `standard`  
      - `imperial`  
      - `metric`

Note: All parameters should be in character format i.e. `latitude =
"-46.05"`.

``` r
weather.api <- function(latitude, longitude, api.id, exclude = NULL, units = "metric") {
  
  #assign the individual pieces of the required url to their respective objects  
  base <- "https://api.openweathermap.org/data/2.5/onecall"
  lat1 <- "lat="
  lat2 <- latitude
  lon1 <- "lon="
  lon2 <- longitude
  exc1 <- "exclude="
  exc2 <- tolower(sub(" ", "", exclude)) #remove spaces and convert to lowercase  
  apid1 <- "appid="
  apid2 <- api.id
  units1 <- "units="
  units2 <- units
  
  #paste pieces together  
  lat <- paste(lat1, lat2, sep = "")
  lon <- paste(lon1, lon2, sep = "")  
  ifelse(!is.null(exc2), exc <- paste(exc1, exc2, sep = ""), exc <- "nullexclude")
  apid <- paste(apid1, apid2, sep = "")
  units <- paste(units1, units2, sep = "")
  
  #paste base and latitude pieces together  
  first <- paste(base, lat, sep = "?")
  
  #create the entire url by pasting all pieces together using delimiter "&", conditioning on if any information is excluded  
  ifelse(!is.null(exc2), my.url <- paste(first, lon, exc, apid, units, sep = "&"), my.url <- paste(first, lon, apid, units, sep = "&"))
  
  #access the Weather API using the URL assembled above
  weather.info <- GET(my.url)
  
  #convert the content of the accessed data from raw to character, then present in readable data frame format
  final <- weather.info$content %>% rawToChar() %>% fromJSON()

return(final)
}
```

## Exploratory Data Analysis

Now that we can easily access the API, let’s do some exploratory data
analysis.

I want to explore the weather data of five locations that lie roughly
along the same line of longitude.

Locations:

  - 44.19’N, 69.47’W (Augusta, Maine, US)  
  - 21.28’N, 71.08’W (Cockburn Town, Turks & Caicos Islands, UK)  
  - 7.54’N, 72.3’W (Cucata, Colombia)  
  - 51.38’S, 69.13’W (Rio Gallegos, Argentina)
  - 55.05’S, 67.05’W (Puerto Williams, Chile)

![](locationmap.jpg)<!-- -->

``` r
Maine <- weather.api(latitude = "44.19", longitude = "-69.47", api.id = "input_your_key", exclude = "current, minutely, hourly")

Maine.day <- Maine$daily

Maine.day
```

    ##           dt    sunrise     sunset   moonrise    moonset
    ## 1 1633536000 1633516901 1633558193 1633516680 1633559760
    ## 2 1633622400 1633603373 1633644485 1633607760 1633647660
    ## 3 1633708800 1633689846 1633730778 1633698900 1633735860
    ## 4 1633795200 1633776319 1633817071 1633790160 1633824300
    ## 5 1633881600 1633862792 1633903365 1633881360 1633913280
    ## 6 1633968000 1633949266 1633989660 1633972140 1634002920
    ## 7 1634054400 1634035740 1634075955 1634062320 1634093100
    ## 8 1634140800 1634122215 1634162251 1634151780 1634183700
    ##   moon_phase temp.day temp.min temp.max temp.night temp.eve
    ## 1       0.00    20.28     8.15    21.41      12.36    15.09
    ## 2       0.05    20.59     9.98    21.70      11.92    15.50
    ## 3       0.08    20.40     8.52    20.77       8.52    13.19
    ## 4       0.12    13.68     4.17    14.24       5.48    11.76
    ## 5       0.16    13.58     4.98    15.54       8.19    13.03
    ## 6       0.20    16.56     8.40    18.45      11.58    15.28
    ## 7       0.25    18.25    11.38    21.04      12.61    17.82
    ## 8       0.27    20.06    12.15    20.94      12.98    17.12
    ##   temp.morn feels_like.day feels_like.night feels_like.eve
    ## 1      8.15          19.72            12.08          14.77
    ## 2     10.12          20.17            11.65          15.38
    ## 3     10.28          19.96             7.21          12.58
    ## 4      4.33          12.54             5.48          10.69
    ## 5      4.98          12.69             7.42          12.27
    ## 6      8.42          15.97            11.33          14.93
    ## 7     11.38          17.98            12.43          17.77
    ## 8     12.16          19.90            12.66          16.90
    ##   feels_like.morn pressure humidity dew_point wind_speed
    ## 1            7.17     1027       52      9.91       1.86
    ## 2            9.59     1024       56     11.43       2.17
    ## 3            9.71     1022       56     11.02       4.80
    ## 4            2.54     1032       55      4.49       2.98
    ## 5            4.98     1029       65      6.89       3.38
    ## 6            7.60     1021       65      9.76       3.20
    ## 7           11.11     1018       71     12.77       2.30
    ## 8           11.94     1015       68     13.86       3.58
    ##   wind_deg wind_gust                            weather
    ## 1      352      3.00         800, Clear, clear sky, 01d
    ## 2      249      3.50 802, Clouds, scattered clouds, 03d
    ## 3      104      9.04         800, Clear, clear sky, 01d
    ## 4      137      6.65       801, Clouds, few clouds, 02d
    ## 5      208      5.71  804, Clouds, overcast clouds, 04d
    ## 6      214      4.72    803, Clouds, broken clouds, 04d
    ## 7      208      5.35  804, Clouds, overcast clouds, 04d
    ## 8      187      5.04         500, Rain, light rain, 10d
    ##   clouds  pop  uvi rain
    ## 1      3 0.00 4.19   NA
    ## 2     32 0.00 3.85   NA
    ## 3      5 0.00 3.83   NA
    ## 4     15 0.00 3.60   NA
    ## 5    100 0.00 3.30   NA
    ## 6     70 0.00 0.21   NA
    ## 7    100 0.00 1.00   NA
    ## 8     28 0.22 1.00 0.21

### Data Cleaning

Let’s convert all dates/times that are in unix form to a date/time
stamp.

``` r
Maine.day$dt <- anydate(Maine.day$dt)
Maine.day$sunrise <- anytime(Maine.day$sunrise)
Maine.day$sunset <- anytime(Maine.day$sunset)
Maine.day$moonrise <- anytime(Maine.day$moonrise)
Maine.day$moonset <- anytime(Maine.day$moonset)
```

Convert the `dt` variable into three variables, `Year`, `Month`, `Day`,
and then change `sunrise`, `sunset`, `moonrise`, and `moonset` to only
include timestamps.

``` r
Maine.day <- Maine.day %>% separate(dt, c("Year", "Month", "Day"), sep = "-", convert = TRUE, remove = TRUE)

#change sunrise, sunset, moonrise, and moonset to only include timestamps

Maine.day <- Maine.day %>% separate(sunrise, c("Date", "Sunrise"), sep = " ", remove = TRUE) %>% subset(select = -Date)

Maine.day <- Maine.day %>% separate(sunset, c("Date", "Sunset"), sep = " ", remove = TRUE) %>% subset(select = -Date)

Maine.day <- Maine.day %>% separate(moonrise, c("Date", "Moonrise"), sep = " ", remove = TRUE) %>% subset(select = -Date)

Maine.day <- Maine.day %>% separate(moonset, c("Date", "Moonset"), sep = " ", remove = TRUE) %>% subset(select = -Date)
```

Lastly, combine the nested `temp` dataframe with `Maine.day` to create a
new dataframe called `Maine.day1`, removing unwanted variables.

``` r
Maine.day1 <- data.frame(Maine.day, Maine.day$temp) %>% select(Year:moon_phase, pressure:wind_gust, clouds, pop, min, max) %>% rename(mintemp = min, maxtemp = max)

Maine.day1
```

    ##   Year Month Day  Sunrise   Sunset Moonrise  Moonset
    ## 1 2021    10   6 06:41:41 18:09:53 06:38:00 18:36:00
    ## 2 2021    10   7 06:42:53 18:08:05 07:56:00 19:01:00
    ## 3 2021    10   8 06:44:06 18:06:18 09:15:00 19:31:00
    ## 4 2021    10   9 06:45:19 18:04:31 10:36:00 20:05:00
    ## 5 2021    10  10 06:46:32 18:02:45 11:56:00 20:48:00
    ## 6 2021    10  11 06:47:46 18:01:00 13:09:00 21:42:00
    ## 7 2021    10  12 06:49:00 17:59:15 14:12:00 22:45:00
    ## 8 2021    10  13 06:50:15 17:57:31 15:03:00 23:55:00
    ##   moon_phase pressure humidity dew_point wind_speed wind_deg
    ## 1       0.00     1027       52      9.91       1.86      352
    ## 2       0.05     1024       56     11.43       2.17      249
    ## 3       0.08     1022       56     11.02       4.80      104
    ## 4       0.12     1032       55      4.49       2.98      137
    ## 5       0.16     1029       65      6.89       3.38      208
    ## 6       0.20     1021       65      9.76       3.20      214
    ## 7       0.25     1018       71     12.77       2.30      208
    ## 8       0.27     1015       68     13.86       3.58      187
    ##   wind_gust clouds  pop mintemp maxtemp
    ## 1      3.00      3 0.00    8.15   21.41
    ## 2      3.50     32 0.00    9.98   21.70
    ## 3      9.04      5 0.00    8.52   20.77
    ## 4      6.65     15 0.00    4.17   14.24
    ## 5      5.71    100 0.00    4.98   15.54
    ## 6      4.72     70 0.00    8.40   18.45
    ## 7      5.35    100 0.00   11.38   21.04
    ## 8      5.04     28 0.22   12.15   20.94

Awesome\! Remember to clean the data returned from the other locations
too.

Now, let’s combine all datasets into one called `weather`, creating a
new variable called `location`.

``` r
Maine.day1 <- Maine.day1 %>% mutate(location = "Maine, US")
Turks.day1 <- Turks.day1 %>% mutate(location = "Turks & Caicos")
Colombia.day1 <- Colombia.day1 %>% mutate(location = "Colombia")
Chile.day1 <- Chile.day1 %>% mutate(location = "Chile")
Argentina.day1 <- Argentina.day1 %>% mutate(location = "Argentina")

weather <- rbind(Maine.day1, Turks.day1, Colombia.day1, Argentina.day1, Chile.day1) %>% relocate(location, .before = Year)
```

### Create New Variables

I’m interested in converting `humidity` and `clouds` into categorical
variables with different levels.

I’ll begin with humidity. Lets say that if humidity is less than or
equal to 60, there is low humidity, if 60 \< humidity is less than or
equal to 80, there is medium humidity, and if humidity \> 80, there is
high humidity.

``` r
weather <- weather %>% mutate(humidity.status = as.factor(ifelse(humidity > 80, "High", ifelse(humidity >60, "Medium", "Low"))))

weather$humidity.status <- ordered(weather$humidity.status, levels = c("Low", "Medium", "High"))
```

Great\! Now let’s look at `clouds`. If clouds is less than or equal to
25, then cloud coverage is low. If clouds \> 75 then cloud coverage is
high, and anything in between is medium.

``` r
weather <- weather %>% mutate(cloud.coverage = as.factor(ifelse(clouds > 75, "High", ifelse(clouds > 25, "Medium", "Low"))))

weather$cloud.coverage <- ordered(weather$cloud.coverage, levels = c("Low", "Medium", "High"))
```

Here’s our cleaned dataset:

``` r
weather
```

    ##          location Year Month Day  Sunrise   Sunset Moonrise
    ## 1       Maine, US 2021    10   6 06:41:41 18:09:53 06:38:00
    ## 2       Maine, US 2021    10   7 06:42:53 18:08:05 07:56:00
    ## 3       Maine, US 2021    10   8 06:44:06 18:06:18 09:15:00
    ## 4       Maine, US 2021    10   9 06:45:19 18:04:31 10:36:00
    ## 5       Maine, US 2021    10  10 06:46:32 18:02:45 11:56:00
    ## 6       Maine, US 2021    10  11 06:47:46 18:01:00 13:09:00
    ## 7       Maine, US 2021    10  12 06:49:00 17:59:15 14:12:00
    ## 8       Maine, US 2021    10  13 06:50:15 17:57:31 15:03:00
    ## 9  Turks & Caicos 2021    10   6 06:36:52 18:27:34 06:40:00
    ## 10 Turks & Caicos 2021    10   7 06:37:11 18:26:40 07:42:00
    ## 11 Turks & Caicos 2021    10   8 06:37:29 18:25:47 08:45:00
    ## 12 Turks & Caicos 2021    10   9 06:37:48 18:24:55 09:51:00
    ## 13 Turks & Caicos 2021    10  10 06:38:08 18:24:02 10:58:00
    ## 14 Turks & Caicos 2021    10  11 06:38:27 18:23:11 12:03:00
    ## 15 Turks & Caicos 2021    10  12 06:38:48 18:22:20 13:06:00
    ## 16 Turks & Caicos 2021    10  13 06:39:08 18:21:30 14:02:00
    ## 17       Colombia 2021    10   6 06:36:32 18:37:40 06:44:00
    ## 18       Colombia 2021    10   7 06:36:26 18:37:10 07:38:00
    ## 19       Colombia 2021    10   8 06:36:21 18:36:41 08:34:00
    ## 20       Colombia 2021    10   9 06:36:16 18:36:12 09:34:00
    ## 21       Colombia 2021    10  10 06:36:12 18:35:43 10:35:00
    ## 22       Colombia 2021    10  11 06:36:08 18:35:16 11:39:00
    ## 23       Colombia 2021    10  12 06:36:05 18:34:48 12:40:00
    ## 24       Colombia 2021    10  13 06:36:02 18:34:22 13:39:00
    ## 25      Argentina 2021    10   6 05:52:32 18:56:19 06:23:00
    ## 26      Argentina 2021    10   7 05:50:17 18:57:58 06:40:00
    ## 27      Argentina 2021    10   8 05:48:02 18:59:38 06:59:00
    ## 28      Argentina 2021    10   9 05:45:48 19:01:19 07:22:00
    ## 29      Argentina 2021    10  10 05:43:34 19:03:00 07:54:00
    ## 30      Argentina 2021    10  11 05:41:21 19:04:41 08:37:00
    ## 31      Argentina 2021    10  12 05:39:09 19:06:23 09:34:00
    ## 32      Argentina 2021    10  13 05:36:57 19:08:05 10:44:00
    ## 33          Chile 2021    10   6 05:39:55 18:52:17 06:13:00
    ## 34          Chile 2021    10   7 05:37:23 18:54:14 06:25:00
    ## 35          Chile 2021    10   8 05:34:51 18:56:11 06:40:00
    ## 36          Chile 2021    10   9 05:32:20 18:58:09 06:58:00
    ## 37          Chile 2021    10  10 05:29:49 19:00:07 07:24:00
    ## 38          Chile 2021    10  11 05:27:18 19:02:06 08:02:00
    ## 39          Chile 2021    10  12 05:24:49 19:04:05 08:57:00
    ## 40          Chile 2021    10  13 05:22:19 19:06:05 10:09:00
    ##     Moonset moon_phase pressure humidity dew_point
    ## 1  18:36:00       0.00     1027       52      9.91
    ## 2  19:01:00       0.05     1024       56     11.43
    ## 3  19:31:00       0.08     1022       56     11.02
    ## 4  20:05:00       0.12     1032       55      4.49
    ## 5  20:48:00       0.16     1029       65      6.89
    ## 6  21:42:00       0.20     1021       65      9.76
    ## 7  22:45:00       0.25     1018       71     12.77
    ## 8  23:55:00       0.27     1015       68     13.86
    ## 9  18:54:00       0.00     1017       75     23.73
    ## 10 19:35:00       0.05     1016       75     23.54
    ## 11 20:19:00       0.08     1014       77     23.81
    ## 12 21:09:00       0.12     1014       75     23.79
    ## 13 22:03:00       0.16     1014       75     23.76
    ## 14 23:02:00       0.20     1014       76     23.80
    ## 15 19:00:00       0.25     1014       79     23.83
    ## 16 00:04:00       0.27     1012       78     22.43
    ## 17 19:04:00       0.00     1016       88     15.17
    ## 18 19:52:00       0.05     1014       54     12.95
    ## 19 20:43:00       0.08     1014       70     15.18
    ## 20 21:38:00       0.12     1014       57     13.54
    ## 21 22:36:00       0.16     1015       68     14.75
    ## 22 23:38:00       0.20     1015       70     15.55
    ## 23 19:00:00       0.25     1016       66     15.03
    ## 24 00:38:00       0.27     1016       78     16.07
    ## 25 19:19:00       0.00     1010       42     -2.48
    ## 26 20:46:00       0.04     1014       57      0.60
    ## 27 22:15:00       0.08     1003       44      1.52
    ## 28 19:00:00       0.12     1000       45      3.41
    ## 29 23:43:00       0.16     1008       49     -1.77
    ## 30 01:07:00       0.19     1008       41     -3.15
    ## 31 02:18:00       0.23      997       38     -2.15
    ## 32 03:13:00       0.25      999       65     -0.01
    ## 33 19:14:00       0.00     1001       64      0.94
    ## 34 20:47:00       0.04     1007       58     -1.13
    ## 35 22:21:00       0.08      993       62     -0.90
    ## 36 19:00:00       0.12      993       76      3.51
    ## 37 23:56:00       0.16      999       47     -3.11
    ## 38 01:23:00       0.19     1002       67      1.89
    ## 39 02:37:00       0.23      993       43     -0.83
    ## 40 03:30:00       0.25      992       76      1.52
    ##    wind_speed wind_deg wind_gust clouds  pop mintemp maxtemp
    ## 1        1.86      352      3.00      3 0.00    8.15   21.41
    ## 2        2.17      249      3.50     32 0.00    9.98   21.70
    ## 3        4.80      104      9.04      5 0.00    8.52   20.77
    ## 4        2.98      137      6.65     15 0.00    4.17   14.24
    ## 5        3.38      208      5.71    100 0.00    4.98   15.54
    ## 6        3.20      214      4.72     70 0.00    8.40   18.45
    ## 7        2.30      208      5.35    100 0.00   11.38   21.04
    ## 8        3.58      187      5.04     28 0.22   12.15   20.94
    ## 9       11.24      100     12.72      9 0.34   28.46   28.63
    ## 10       9.78       81     10.57      2 0.75   27.89   28.63
    ## 11       7.90       87      8.27      6 0.77   27.72   28.52
    ## 12       8.12      107      8.57     24 0.87   27.97   28.64
    ## 13       9.37      118      9.79     31 0.41   28.33   28.87
    ## 14       8.78      125      9.11     16 0.18   28.15   28.65
    ## 15      10.15       82     10.27     92 1.00   26.45   28.27
    ## 16       8.76       83      8.82    100 0.52   26.18   27.57
    ## 17       2.55      123      4.56    100 0.77   13.41   19.65
    ## 18       3.08      101      4.92     68 0.96   12.43   22.26
    ## 19       2.25      104      3.30     90 1.00   12.36   20.35
    ## 20       2.38       82      3.17     96 1.00   12.91   21.62
    ## 21       1.62      111      2.41     91 1.00   13.42   20.05
    ## 22       2.49      100      3.24     52 1.00   12.75   21.20
    ## 23       2.69      102      3.36     89 1.00   13.05   20.82
    ## 24       1.64      245      2.13     92 1.00   13.64   19.05
    ## 25      15.52      246     21.63    100 0.34    4.26   12.12
    ## 26      12.17      238     18.03    100 0.00    2.72   11.35
    ## 27      15.28      237     18.10     93 0.00    5.32   13.32
    ## 28      13.28       14     21.15    100 0.00    4.75   16.73
    ## 29      10.89      227     15.73     52 0.34    5.17   10.18
    ## 30       6.09      359      7.90     77 0.00    5.00   11.89
    ## 31      10.28      310     15.91      9 0.00    6.04   14.77
    ## 32      13.99      241     17.26    100 0.63    4.92   10.14
    ## 33      10.06      243     16.73    100 0.75    2.04    9.36
    ## 34       8.71      228     15.89    100 0.57    1.66    8.48
    ## 35      11.50      251     19.99     94 0.56    3.74    8.98
    ## 36      10.69      343     24.06    100 0.47    1.78   10.67
    ## 37      10.92      316     20.55      2 0.58    3.06    9.52
    ## 38       8.90      321     16.90    100 0.31    2.01   11.61
    ## 39       7.91      338     16.30     77 0.00    7.09   13.35
    ## 40       5.25      179      6.49     96 0.71    3.17    6.22
    ##    humidity.status cloud.coverage
    ## 1              Low            Low
    ## 2              Low         Medium
    ## 3              Low            Low
    ## 4              Low            Low
    ## 5           Medium           High
    ## 6           Medium         Medium
    ## 7           Medium           High
    ## 8           Medium         Medium
    ## 9           Medium            Low
    ## 10          Medium            Low
    ## 11          Medium            Low
    ## 12          Medium            Low
    ## 13          Medium         Medium
    ## 14          Medium            Low
    ## 15          Medium           High
    ## 16          Medium           High
    ## 17            High           High
    ## 18             Low         Medium
    ## 19          Medium           High
    ## 20             Low           High
    ## 21          Medium           High
    ## 22          Medium         Medium
    ## 23          Medium           High
    ## 24          Medium           High
    ## 25             Low           High
    ## 26             Low           High
    ## 27             Low           High
    ## 28             Low           High
    ## 29             Low         Medium
    ## 30             Low           High
    ## 31             Low            Low
    ## 32          Medium           High
    ## 33          Medium           High
    ## 34             Low           High
    ## 35          Medium           High
    ## 36          Medium           High
    ## 37             Low            Low
    ## 38          Medium           High
    ## 39             Low           High
    ## 40          Medium           High

### Contingency Tables

With the new categorical variables I’ve created, let’s create some
contingency tables. `tabz1` will show the counts of observations within
each level combination of `cloud.coverage` and `humidity.status`.

``` r
tabz1 <- table(weather$humidity.status, weather$cloud.coverage, deparse.level = 2)

tabz1
```

    ##                        weather$cloud.coverage
    ## weather$humidity.status Low Medium High
    ##                  Low      5      3    8
    ##                  Medium   5      4   14
    ##                  High     0      0    1

It looks like there were 10 forecast observations that predicted high
cloud coverage and low humidity, and 12 observations that predicted high
cloud coverage and medium humidity. There weren’t many observations that
predicted high humidity, but it appears that when high humidity was
predicted, it was most often coupled with high cloud coverage.

`tabz2` will show the counts of days within each level combination of
`cloud.coverage` and `humidity.status`, separated by `location`.

``` r
tabz2 <- table(weather$humidity.status, weather$cloud.coverage, weather$location, deparse.level = 2)

tabz2
```

    ## , , weather$location = Argentina
    ## 
    ##                        weather$cloud.coverage
    ## weather$humidity.status Low Medium High
    ##                  Low      1      1    5
    ##                  Medium   0      0    1
    ##                  High     0      0    0
    ## 
    ## , , weather$location = Chile
    ## 
    ##                        weather$cloud.coverage
    ## weather$humidity.status Low Medium High
    ##                  Low      1      0    2
    ##                  Medium   0      0    5
    ##                  High     0      0    0
    ## 
    ## , , weather$location = Colombia
    ## 
    ##                        weather$cloud.coverage
    ## weather$humidity.status Low Medium High
    ##                  Low      0      1    1
    ##                  Medium   0      1    4
    ##                  High     0      0    1
    ## 
    ## , , weather$location = Maine, US
    ## 
    ##                        weather$cloud.coverage
    ## weather$humidity.status Low Medium High
    ##                  Low      3      1    0
    ##                  Medium   0      2    2
    ##                  High     0      0    0
    ## 
    ## , , weather$location = Turks & Caicos
    ## 
    ##                        weather$cloud.coverage
    ## weather$humidity.status Low Medium High
    ##                  Low      0      0    0
    ##                  Medium   5      1    2
    ##                  High     0      0    0

This table gives us an idea of how the relationship between humidity
status and cloud coverage can change based on location. An observation
to note is that Maine and Argentina, locations close to the poles, most
often have low humidity in this dataset. However, the low humidity is
more frequently coupled with high cloud coverage in Argentina and low
cloud coverage in Maine. I’d be interested to find out why this is the
case, or if the data only appears this way because we’re not working
with many observations. Also, it’s interesting that Chile, the location
closest to the south pole, has a range of humidity although it’s close
to the Argentina location.

### Numerical Summaries

Now that we’ve explored our categorical variables, let’s take a look at
some numeric summaries.

The table below lists the average maximum temperature, average minimum
temperature, their respective standard deviations, and the interquartile
range for each location.

``` r
weather$location <- ordered(weather$location, levels = c("Chile", "Argentina", "Colombia", "Turks & Caicos", "Maine, US")) #convert location into an ordered variable

weather %>% group_by(location) %>% summarise(avghigh = mean(maxtemp), avglow = mean(mintemp), sdhigh = sd(maxtemp), sdlow = sd(mintemp), IQR = IQR(maxtemp))
```

    ## # A tibble: 5 x 6
    ##   location       avghigh avglow sdhigh sdlow   IQR
    ##   <ord>            <dbl>  <dbl>  <dbl> <dbl> <dbl>
    ## 1 Chile             9.77   3.07  2.14  1.79  2.05 
    ## 2 Argentina        12.6    4.77  2.28  0.972 2.63 
    ## 3 Colombia         20.6   13.0   1.06  0.472 1.35 
    ## 4 Turks & Caicos   28.5   27.6   0.401 0.856 0.185
    ## 5 Maine, US        19.3    8.47  2.89  2.80  3.41

It’s clear that the average high and average low peak near the middle of
the globe, with greater variation near the poles. These numbers could
not only be affected by latitude, but also by elevation.

Now we’ll look at the average humidity forecasted for each location, as
well as their standard deviations.

``` r
weather %>% group_by(location) %>% summarise(avg_humidity = mean(humidity), sd.humidity = sd(humidity))
```

    ## # A tibble: 5 x 3
    ##   location       avg_humidity sd.humidity
    ##   <ord>                 <dbl>       <dbl>
    ## 1 Chile                  61.6       12.1 
    ## 2 Argentina              47.6        9.10
    ## 3 Colombia               68.9       10.8 
    ## 4 Turks & Caicos         76.2        1.58
    ## 5 Maine, US              61          7.05

Here we see a clear relationship between location (organized by
latitude) and humidity. The locations nearest to the poles generally
have the lowest humidity, but the Turks and Caicos Islands as well as
the Chile location have the highest humidity, most likely because they
are close to or in the ocean.

### Data Visualization

Now I’ll use some tools for visualizing the data we’ve collected from
the API.

Let’s visualize the second contingency table we made above using a bar
graph with `humidity.status` and `cloud.coverage`, separating the
results by `location`.

``` r
sum.tab <- weather %>% group_by(location, humidity.status, cloud.coverage) %>% summarise(count = n())

g6 <- ggplot(sum.tab, aes(x = humidity.status, y = count))

g6 + geom_bar(aes(fill = cloud.coverage), stat = "identity", position = "dodge") + facet_wrap(~ location) + labs(title = "8 Day Forecast: Humidity Status vs. Cloud Coverage by Location", x = "Humidity Status") + scale_fill_discrete(name = "Cloud Coverage") + theme(axis.text.x = element_text(angle = 45, vjust = .8, hjust = 1))
```

![](README_files/figure-gfm/bar%20graph-1.png)<!-- -->

Here we can see the observation we made earlier about Maine and
Argentina having low humidity, but most often low or high cloud
coverage, respectively. In addition, it’s also easier to see that the
forecast for Chile is mostly medium humidity and high cloud coverage. A
puzzling observation is that Turks and Caicos always has medium
humidity, but either low or high cloud coverage.

Next, I’ll look at how location affects minimum and maximum daily
temperature using boxplots.

``` r
max.means <- weather %>% group_by(location) %>% summarise(average = mean(maxtemp)) 

g <- ggplot(weather, aes(x = location, y = maxtemp))

g + geom_boxplot(fill = "grey") + geom_point(max.means, mapping = aes(x = location, y = average), color = "purple") + geom_line(max.means, mapping = aes(x = location, y = average, group = 1), color = "purple") + labs(title = "8 Day Forecast: Maximum Daily Temperature",x = "Location (highest to lowest latitude)", y = "Maximum Daily Temperature (C)") + coord_flip()
```

![](README_files/figure-gfm/maxtemp%20boxplot-1.png)<!-- -->

``` r
min.means <- weather %>% group_by(location) %>% summarise(average = mean(mintemp))

g1 <- ggplot(weather, aes(x = location, y = mintemp))

g1 + geom_boxplot(fill = "grey") + geom_point(min.means, mapping = aes(x = location, y = average), color = "green") + geom_line(min.means, mapping = aes(x = location, y = average, group = 1), color = "green") + labs(title = "8 Day Forecast: Minimum Daily Temperature", x = "Location (highest to lowest latitude)", y = "Minimum Daily Temperature (C)") + coord_flip()
```

![](README_files/figure-gfm/mintemp%20boxplot-1.png)<!-- -->

Both boxplots are fairly consistent with each other, showing Turks and
Caicos as having the highest minimum and maximum temperature, and
locations at the highest and lowest latitudes as having the lowest
minimum and maximum temperatures. Although Cucata, Colombia is closer to
the equator than Turks and Caicos Islands, a possible reason as to why
it may not be warmer is its elevation at 320 meters (1,050 ft).

Now we’ll turn to exploring `humidity` as a quantitative variable,
creating a histogram with density plots of `humidity` facetted by
`location`. We’ll also create a boxplot of the same information.

``` r
g2 <- ggplot(weather, aes(x = humidity))

g2 + geom_histogram(bins = 20, aes(y = ..density..)) + geom_density(color = "aquamarine", fill = "aquamarine") + facet_wrap(~location) + labs(title = "8 Day Forecast: Histogram of Humidity by Location", x = "Humidity (%)")
```

![](README_files/figure-gfm/humidity%20hist%20and%20density%20plot-1.png)<!-- -->

``` r
hum.means <- weather %>% group_by(location) %>% summarise(average = mean(humidity))

g3 <- ggplot(weather, aes(x = location, y = humidity))

g3 + geom_boxplot(fill = "grey") + geom_point(hum.means, mapping = aes(x = location, y = average), color = "red") + geom_line(hum.means, mapping = aes(x = location, y = average, group = 1), color = "red") + labs(title = "8 Day Forecast: Average Daily Humidity by Location", x = "Location (highest to lowest latitude)", y = "Average Daily Humidity (C)") + coord_flip()
```

![](README_files/figure-gfm/humidity%20boxplot-1.png)<!-- -->

This histogram and boxplot are very helpful in displaying how humidity
varies by location. While Argentina and Maine consistently show lower
humidity levels, Turks and Caicos is vary consistent in showing high
humidity, while Colombia and Chile’s humidities are less consistent.

To prepare for the next plot, I’ll convert all variables with timestamps
to numeric variables of time since 00:00:00, calculated in minutes.

``` r
weather$Sunrise <- round(60 * 24 * as.numeric(times(weather$Sunrise)),digits = 2)

weather$Sunset <- round(60 * 24 * as.numeric(times(weather$Sunset)),digits = 2)

weather$Moonrise <- round(60 * 24 * as.numeric(times(weather$Moonrise)), digits = 2)

weather$Moonset <- round(60 * 24 * as.numeric(times(weather$Moonset)), digits = 2)

weather
```

    ##          location Year Month Day Sunrise  Sunset Moonrise
    ## 1       Maine, US 2021    10   6  401.68 1089.88      398
    ## 2       Maine, US 2021    10   7  402.88 1088.08      476
    ## 3       Maine, US 2021    10   8  404.10 1086.30      555
    ## 4       Maine, US 2021    10   9  405.32 1084.52      636
    ## 5       Maine, US 2021    10  10  406.53 1082.75      716
    ## 6       Maine, US 2021    10  11  407.77 1081.00      789
    ## 7       Maine, US 2021    10  12  409.00 1079.25      852
    ## 8       Maine, US 2021    10  13  410.25 1077.52      903
    ## 9  Turks & Caicos 2021    10   6  396.87 1107.57      400
    ## 10 Turks & Caicos 2021    10   7  397.18 1106.67      462
    ## 11 Turks & Caicos 2021    10   8  397.48 1105.78      525
    ## 12 Turks & Caicos 2021    10   9  397.80 1104.92      591
    ## 13 Turks & Caicos 2021    10  10  398.13 1104.03      658
    ## 14 Turks & Caicos 2021    10  11  398.45 1103.18      723
    ## 15 Turks & Caicos 2021    10  12  398.80 1102.33      786
    ## 16 Turks & Caicos 2021    10  13  399.13 1101.50      842
    ## 17       Colombia 2021    10   6  396.53 1117.67      404
    ## 18       Colombia 2021    10   7  396.43 1117.17      458
    ## 19       Colombia 2021    10   8  396.35 1116.68      514
    ## 20       Colombia 2021    10   9  396.27 1116.20      574
    ## 21       Colombia 2021    10  10  396.20 1115.72      635
    ## 22       Colombia 2021    10  11  396.13 1115.27      699
    ## 23       Colombia 2021    10  12  396.08 1114.80      760
    ## 24       Colombia 2021    10  13  396.03 1114.37      819
    ## 25      Argentina 2021    10   6  352.53 1136.32      383
    ## 26      Argentina 2021    10   7  350.28 1137.97      400
    ## 27      Argentina 2021    10   8  348.03 1139.63      419
    ## 28      Argentina 2021    10   9  345.80 1141.32      442
    ## 29      Argentina 2021    10  10  343.57 1143.00      474
    ## 30      Argentina 2021    10  11  341.35 1144.68      517
    ## 31      Argentina 2021    10  12  339.15 1146.38      574
    ## 32      Argentina 2021    10  13  336.95 1148.08      644
    ## 33          Chile 2021    10   6  339.92 1132.28      373
    ## 34          Chile 2021    10   7  337.38 1134.23      385
    ## 35          Chile 2021    10   8  334.85 1136.18      400
    ## 36          Chile 2021    10   9  332.33 1138.15      418
    ## 37          Chile 2021    10  10  329.82 1140.12      444
    ## 38          Chile 2021    10  11  327.30 1142.10      482
    ## 39          Chile 2021    10  12  324.82 1144.08      537
    ## 40          Chile 2021    10  13  322.32 1146.08      609
    ##    Moonset moon_phase pressure humidity dew_point wind_speed
    ## 1     1116       0.00     1027       52      9.91       1.86
    ## 2     1141       0.05     1024       56     11.43       2.17
    ## 3     1171       0.08     1022       56     11.02       4.80
    ## 4     1205       0.12     1032       55      4.49       2.98
    ## 5     1248       0.16     1029       65      6.89       3.38
    ## 6     1302       0.20     1021       65      9.76       3.20
    ## 7     1365       0.25     1018       71     12.77       2.30
    ## 8     1435       0.27     1015       68     13.86       3.58
    ## 9     1134       0.00     1017       75     23.73      11.24
    ## 10    1175       0.05     1016       75     23.54       9.78
    ## 11    1219       0.08     1014       77     23.81       7.90
    ## 12    1269       0.12     1014       75     23.79       8.12
    ## 13    1323       0.16     1014       75     23.76       9.37
    ## 14    1382       0.20     1014       76     23.80       8.78
    ## 15    1140       0.25     1014       79     23.83      10.15
    ## 16       4       0.27     1012       78     22.43       8.76
    ## 17    1144       0.00     1016       88     15.17       2.55
    ## 18    1192       0.05     1014       54     12.95       3.08
    ## 19    1243       0.08     1014       70     15.18       2.25
    ## 20    1298       0.12     1014       57     13.54       2.38
    ## 21    1356       0.16     1015       68     14.75       1.62
    ## 22    1418       0.20     1015       70     15.55       2.49
    ## 23    1140       0.25     1016       66     15.03       2.69
    ## 24      38       0.27     1016       78     16.07       1.64
    ## 25    1159       0.00     1010       42     -2.48      15.52
    ## 26    1246       0.04     1014       57      0.60      12.17
    ## 27    1335       0.08     1003       44      1.52      15.28
    ## 28    1140       0.12     1000       45      3.41      13.28
    ## 29    1423       0.16     1008       49     -1.77      10.89
    ## 30      67       0.19     1008       41     -3.15       6.09
    ## 31     138       0.23      997       38     -2.15      10.28
    ## 32     193       0.25      999       65     -0.01      13.99
    ## 33    1154       0.00     1001       64      0.94      10.06
    ## 34    1247       0.04     1007       58     -1.13       8.71
    ## 35    1341       0.08      993       62     -0.90      11.50
    ## 36    1140       0.12      993       76      3.51      10.69
    ## 37    1436       0.16      999       47     -3.11      10.92
    ## 38      83       0.19     1002       67      1.89       8.90
    ## 39     157       0.23      993       43     -0.83       7.91
    ## 40     210       0.25      992       76      1.52       5.25
    ##    wind_deg wind_gust clouds  pop mintemp maxtemp
    ## 1       352      3.00      3 0.00    8.15   21.41
    ## 2       249      3.50     32 0.00    9.98   21.70
    ## 3       104      9.04      5 0.00    8.52   20.77
    ## 4       137      6.65     15 0.00    4.17   14.24
    ## 5       208      5.71    100 0.00    4.98   15.54
    ## 6       214      4.72     70 0.00    8.40   18.45
    ## 7       208      5.35    100 0.00   11.38   21.04
    ## 8       187      5.04     28 0.22   12.15   20.94
    ## 9       100     12.72      9 0.34   28.46   28.63
    ## 10       81     10.57      2 0.75   27.89   28.63
    ## 11       87      8.27      6 0.77   27.72   28.52
    ## 12      107      8.57     24 0.87   27.97   28.64
    ## 13      118      9.79     31 0.41   28.33   28.87
    ## 14      125      9.11     16 0.18   28.15   28.65
    ## 15       82     10.27     92 1.00   26.45   28.27
    ## 16       83      8.82    100 0.52   26.18   27.57
    ## 17      123      4.56    100 0.77   13.41   19.65
    ## 18      101      4.92     68 0.96   12.43   22.26
    ## 19      104      3.30     90 1.00   12.36   20.35
    ## 20       82      3.17     96 1.00   12.91   21.62
    ## 21      111      2.41     91 1.00   13.42   20.05
    ## 22      100      3.24     52 1.00   12.75   21.20
    ## 23      102      3.36     89 1.00   13.05   20.82
    ## 24      245      2.13     92 1.00   13.64   19.05
    ## 25      246     21.63    100 0.34    4.26   12.12
    ## 26      238     18.03    100 0.00    2.72   11.35
    ## 27      237     18.10     93 0.00    5.32   13.32
    ## 28       14     21.15    100 0.00    4.75   16.73
    ## 29      227     15.73     52 0.34    5.17   10.18
    ## 30      359      7.90     77 0.00    5.00   11.89
    ## 31      310     15.91      9 0.00    6.04   14.77
    ## 32      241     17.26    100 0.63    4.92   10.14
    ## 33      243     16.73    100 0.75    2.04    9.36
    ## 34      228     15.89    100 0.57    1.66    8.48
    ## 35      251     19.99     94 0.56    3.74    8.98
    ## 36      343     24.06    100 0.47    1.78   10.67
    ## 37      316     20.55      2 0.58    3.06    9.52
    ## 38      321     16.90    100 0.31    2.01   11.61
    ## 39      338     16.30     77 0.00    7.09   13.35
    ## 40      179      6.49     96 0.71    3.17    6.22
    ##    humidity.status cloud.coverage
    ## 1              Low            Low
    ## 2              Low         Medium
    ## 3              Low            Low
    ## 4              Low            Low
    ## 5           Medium           High
    ## 6           Medium         Medium
    ## 7           Medium           High
    ## 8           Medium         Medium
    ## 9           Medium            Low
    ## 10          Medium            Low
    ## 11          Medium            Low
    ## 12          Medium            Low
    ## 13          Medium         Medium
    ## 14          Medium            Low
    ## 15          Medium           High
    ## 16          Medium           High
    ## 17            High           High
    ## 18             Low         Medium
    ## 19          Medium           High
    ## 20             Low           High
    ## 21          Medium           High
    ## 22          Medium         Medium
    ## 23          Medium           High
    ## 24          Medium           High
    ## 25             Low           High
    ## 26             Low           High
    ## 27             Low           High
    ## 28             Low           High
    ## 29             Low         Medium
    ## 30             Low           High
    ## 31             Low            Low
    ## 32          Medium           High
    ## 33          Medium           High
    ## 34             Low           High
    ## 35          Medium           High
    ## 36          Medium           High
    ## 37             Low            Low
    ## 38          Medium           High
    ## 39             Low           High
    ## 40          Medium           High

The scatterplot below allows us to look at how sunrise time changes over
the course of 8 days in each location.

``` r
g4 <- ggplot(weather, aes(x = Day, y = Sunrise))

g4 + geom_point(aes(color = location), size = 2) + scale_color_discrete(name = "Location") + labs(title = "8 Day Forecast for Sunrise Time by Location", x = "Day of the Month", y = "Sunrise Time (minutes after 12am)")
```

![](README_files/figure-gfm/sunrise%20scatterplot-1.png)<!-- -->

I think this plot is especially cool to look at, since one can clearly
distinguish which locations are in the southern hemisphere, and which
are in the northern hemisphere. While the sun is rising increasingly
later in the northern hemisphere, it’s rising increasingly earlier in
the southern hemisphere.

Lastly, we’ll explore the relationship between maximum daily temperature
and daily pressure in the scatterplot below.

``` r
g5 <- ggplot(weather, aes(x = maxtemp, y = pressure))

g5 + geom_point(aes(color = location), size = 2) + geom_smooth(method = lm, formula = y~poly(x,2), color = "black") + scale_color_discrete(name = "Location") + labs(title = "8 Day Forecast: Maximum Daily Temperature vs. Daily Pressure", x = "Maximum Daily Temperature (C)", y = "Daily Pressure (millibars)") + scale_size_continuous()
```

![](README_files/figure-gfm/maxtemp%20vs%20pressure%20scatterplot-1.png)<!-- -->

There seems to be a medium-strength positive relationship between
temperature and pressure. However, the data from the Turks and Caicos
Islands appears to deviate slightly from the general trend, which is why
I decided to fit a quadratic model. To fit a more accurate model, we may
need to pull data from more locations from the API.

## Final Thoughts

Extracting data from an API follows a generally simple process, although
different APIs have varying syntax for how to assemble the URL.
Hopefully, this vignette was helpful in exemplifying how to access a
typical API, clean the data, and perform a few basic analyses.

To read my blog post about this project, visit
<http://atbiggie.github.io>.
