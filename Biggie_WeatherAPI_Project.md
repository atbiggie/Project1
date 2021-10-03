How to Access an API with an Example
================
Autumn Biggie
10/3/2021

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

\[insert picture here\]

Locations:

  - 44.19’N, 69.47’W (Augusta, Maine, US)  
  - 21.28’N, 71.08’W (Cockburn Town, Turks & Caicos Islands, UK)  
  - 7.54’N, 72.3’W (Cucata, Colombia)  
  - 55.05’S, 67.05’W (Puerto Toro, Chile)  
  - 51.38’S, 69.13’W (Rio Gallegos, Argentina)

<!-- end list -->

``` r
Maine <- weather.api(latitude = "44.19", longitude = "-69.47", api.id = "27667529a1629f208f81fded8f7552af", exclude = "current, minutely, hourly")

Maine.day <- Maine$daily

Maine.day
```

    ##           dt    sunrise     sunset   moonrise    moonset moon_phase temp.day
    ## 1 1633276800 1633257486 1633299320 1633244100 1633296240       0.90    11.55
    ## 2 1633363200 1633343957 1633385611 1633334820 1633384140       0.93    14.67
    ## 3 1633449600 1633430429 1633471902 1633425720 1633471920       0.97    16.49
    ## 4 1633536000 1633516901 1633558193 1633516680 1633559760       0.00    18.56
    ## 5 1633622400 1633603373 1633644485 1633607760 1633647660       0.05    19.79
    ## 6 1633708800 1633689846 1633730778 1633698900 1633735860       0.08    16.51
    ## 7 1633795200 1633776319 1633817071 1633790160 1633824300       0.12    13.56
    ## 8 1633881600 1633862792 1633903365 1633881360 1633913280       0.16    15.69
    ##   temp.min temp.max temp.night temp.eve temp.morn feels_like.day
    ## 1    10.29    12.14      10.94    11.59     10.44          11.14
    ## 2     5.21    16.24       9.60    11.29      5.42          13.68
    ## 3     8.63    16.87       8.70    12.33      8.80          15.66
    ## 4     7.96    21.01      10.91    18.60      7.96          18.04
    ## 5    10.43    22.15      12.66    20.08     10.43          19.39
    ## 6     5.97    17.20       5.97    14.11     10.71          15.68
    ## 7     3.67    15.98       6.18    13.01      3.67          12.07
    ## 8     5.96    18.72      10.64    14.56      5.96          14.86
    ##   feels_like.night feels_like.eve feels_like.morn pressure humidity dew_point
    ## 1            10.60          11.31           10.05     1017       91      9.93
    ## 2             9.60          10.59            3.75     1021       57      6.22
    ## 3             8.70          11.76            8.80     1026       56      7.37
    ## 4            10.51          18.37            7.96     1028       60     10.46
    ## 5            12.25          19.92           10.01     1024       60     11.50
    ## 6             4.85          12.83           10.16     1024       56      7.54
    ## 7             6.18          11.88            1.81     1034       42      0.67
    ## 8            10.11          14.06            4.75     1028       59      7.68
    ##   wind_speed wind_deg wind_gust                            weather clouds  pop
    ## 1       1.74       36      3.96         500, Rain, light rain, 10d    100 0.85
    ## 2       2.78       35      7.79  804, Clouds, overcast clouds, 04d    100 0.06
    ## 3       1.53      195      2.09  804, Clouds, overcast clouds, 04d    100 0.16
    ## 4       1.44      219      1.90         800, Clear, clear sky, 01d      4 0.00
    ## 5       1.92      308      3.13 802, Clouds, scattered clouds, 03d     35 0.00
    ## 6       4.50       27     10.19       801, Clouds, few clouds, 02d     13 0.00
    ## 7       2.06      203      3.55         800, Clear, clear sky, 01d      0 0.00
    ## 8       3.07      225      9.26       801, Clouds, few clouds, 02d     13 0.00
    ##   rain  uvi
    ## 1 1.09 0.71
    ## 2   NA 3.64
    ## 3   NA 3.87
    ## 4   NA 4.08
    ## 5   NA 3.93
    ## 6   NA 0.23
    ## 7   NA 1.00
    ## 8   NA 1.00

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
Maine.day1 <- data.frame(Maine.day, Maine.day$temp) %>% select(Year:moon_phase, pressure:wind_gust, clouds:rain, min, max) %>% rename(mintemp = min, maxtemp = max)

Maine.day1
```

    ##   Year Month Day  Sunrise   Sunset Moonrise  Moonset moon_phase pressure
    ## 1 2021    10   3 06:38:06 18:15:20 02:55:00 17:24:00       0.90     1017
    ## 2 2021    10   4 06:39:17 18:13:31 04:07:00 17:49:00       0.93     1021
    ## 3 2021    10   5 06:40:29 18:11:42 05:22:00 18:12:00       0.97     1026
    ## 4 2021    10   6 06:41:41 18:09:53 06:38:00 18:36:00       0.00     1028
    ## 5 2021    10   7 06:42:53 18:08:05 07:56:00 19:01:00       0.05     1024
    ## 6 2021    10   8 06:44:06 18:06:18 09:15:00 19:31:00       0.08     1024
    ## 7 2021    10   9 06:45:19 18:04:31 10:36:00 20:05:00       0.12     1034
    ## 8 2021    10  10 06:46:32 18:02:45 11:56:00 20:48:00       0.16     1028
    ##   humidity dew_point wind_speed wind_deg wind_gust clouds  pop rain mintemp
    ## 1       91      9.93       1.74       36      3.96    100 0.85 1.09   10.29
    ## 2       57      6.22       2.78       35      7.79    100 0.06   NA    5.21
    ## 3       56      7.37       1.53      195      2.09    100 0.16   NA    8.63
    ## 4       60     10.46       1.44      219      1.90      4 0.00   NA    7.96
    ## 5       60     11.50       1.92      308      3.13     35 0.00   NA   10.43
    ## 6       56      7.54       4.50       27     10.19     13 0.00   NA    5.97
    ## 7       42      0.67       2.06      203      3.55      0 0.00   NA    3.67
    ## 8       59      7.68       3.07      225      9.26     13 0.00   NA    5.96
    ##   maxtemp
    ## 1   12.14
    ## 2   16.24
    ## 3   16.87
    ## 4   21.01
    ## 5   22.15
    ## 6   17.20
    ## 7   15.98
    ## 8   18.72

Awesome\! Remember to clean the data returned from the other locations
too.

``` r
Turks.day1
```

    ##   Year Month Day  Sunrise   Sunset Moonrise  Moonset moon_phase pressure
    ## 1 2021    10   3 06:36:00 18:30:19 03:44:00 16:57:00       0.90     1014
    ## 2 2021    10   4 06:36:17 18:29:23 04:42:00 17:36:00       0.93     1014
    ## 3 2021    10   5 06:36:35 18:28:29 05:41:00 18:15:00       0.97     1015
    ## 4 2021    10   6 06:36:52 18:27:34 06:40:00 18:54:00       0.00     1016
    ## 5 2021    10   7 06:37:11 18:26:40 07:42:00 19:35:00       0.05     1016
    ## 6 2021    10   8 06:37:29 18:25:47 08:45:00 20:19:00       0.08     1014
    ## 7 2021    10   9 06:37:48 18:24:55 09:51:00 21:09:00       0.12     1014
    ## 8 2021    10  10 06:38:08 18:24:02 10:58:00 22:03:00       0.16     1013
    ##   humidity dew_point wind_speed wind_deg wind_gust clouds  pop  rain mintemp
    ## 1       68     22.12       9.87      124     10.16     99 0.96  2.40   27.01
    ## 2       73     23.49      11.29      135     12.25    100 0.96  3.53   28.01
    ## 3       72     23.30      10.91      102     11.65    100 0.76  2.74   28.32
    ## 4       72     23.37      10.57      103     11.43     11 0.74  2.22   28.36
    ## 5       74     23.54       9.65       98     10.22      9 0.46  1.31   28.37
    ## 6       74     23.49       7.80      107      8.31      6 0.84  2.29   28.28
    ## 7       75     23.87       7.67      114      7.61     84 0.87  3.88   28.36
    ## 8       76     23.93       8.59      128      8.63    100 0.98 10.94   28.24
    ##   maxtemp
    ## 1   28.74
    ## 2   28.93
    ## 3   28.98
    ## 4   28.92
    ## 5   28.72
    ## 6   28.72
    ## 7   28.90
    ## 8   28.73

``` r
Colombia.day1
```

    ##   Year Month Day  Sunrise   Sunset Moonrise  Moonset moon_phase pressure
    ## 1 2021    10   3 06:36:51 18:39:13 04:06:00 16:47:00       0.90     1013
    ## 2 2021    10   4 06:36:45 18:38:41 04:59:00 17:33:00       0.93     1013
    ## 3 2021    10   5 06:36:38 18:38:10 05:51:00 18:18:00       0.97     1013
    ## 4 2021    10   6 06:36:32 18:37:40 06:44:00 19:04:00       0.00     1016
    ## 5 2021    10   7 06:36:26 18:37:10 07:38:00 19:52:00       0.05     1015
    ## 6 2021    10   8 06:36:21 18:36:41 08:34:00 20:43:00       0.08     1014
    ## 7 2021    10   9 06:36:16 18:36:12 09:34:00 21:38:00       0.12     1016
    ## 8 2021    10  10 06:36:12 18:35:43 10:35:00 22:36:00       0.16     1016
    ##   humidity dew_point wind_speed wind_deg wind_gust clouds  pop  rain mintemp
    ## 1       70     15.43       2.70      103      4.02     86 0.96 12.99   12.33
    ## 2       63     14.25       2.85      101      4.19     85 0.81  0.59   12.67
    ## 3       63     14.26       2.85      103      3.87    100 0.92  2.10   12.75
    ## 4       85     15.86       2.36      125      4.42     98 1.00  8.80   12.94
    ## 5       56     12.64       3.43      107      4.86     90 0.67  2.97   12.55
    ## 6       53     12.56       2.65      105      3.75     53 1.00  4.88   12.22
    ## 7       83     15.75       1.40      214      1.76     99 1.00 13.48   12.78
    ## 8       86     16.06       1.20       83      2.09     98 1.00 21.65   13.56
    ##   maxtemp
    ## 1   20.26
    ## 2   21.69
    ## 3   21.12
    ## 4   17.47
    ## 5   22.75
    ## 6   21.60
    ## 7   18.06
    ## 8   18.16

``` r
Chile.day1
```

    ##   Year Month Day  Sunrise   Sunset Moonrise  Moonset moon_phase pressure
    ## 1 2021    10   3 05:47:35 18:46:29 05:35:00 14:50:00       0.90      988
    ## 2 2021    10   4 05:45:02 18:48:25 05:49:00 16:16:00       0.93      998
    ## 3 2021    10   5 05:42:28 18:50:21 06:01:00 17:44:00       0.97     1016
    ## 4 2021    10   6 05:39:55 18:52:17 06:13:00 19:14:00       0.00      998
    ## 5 2021    10   7 05:37:23 18:54:14 06:25:00 20:47:00       0.04     1004
    ## 6 2021    10   8 05:34:51 18:56:11 06:40:00 22:21:00       0.08      987
    ## 7 2021    10   9 05:32:20 18:58:09 06:58:00 19:00:00       0.12      988
    ## 8 2021    10  10 05:29:49 19:00:07 07:24:00 23:56:00       0.16     1000
    ##   humidity dew_point wind_speed wind_deg wind_gust clouds  pop rain snow  uvi
    ## 1       70      1.78      10.00      276     19.69    100 1.00 1.07 0.17 2.68
    ## 2       83      1.24      13.46      260     24.23     42 0.54 0.27 0.77 2.74
    ## 3       55     -3.20      10.13      226     16.04     39 0.34   NA 0.26 3.70
    ## 4       68      2.38      14.35      236     23.76     85 0.53 1.09   NA 2.92
    ## 5       51     -1.74      10.89      239     18.16     94 0.34 0.56   NA 3.44
    ## 6       78      3.64       9.57      340     18.40    100 0.86 2.74   NA 0.74
    ## 7       75      2.75      10.19      341     23.63    100 0.69 1.45   NA 1.00
    ## 8       60      1.13       4.76      246      8.61     92 0.32 0.45   NA 1.00
    ##   mintemp maxtemp
    ## 1    2.32    9.12
    ## 2    2.98    5.29
    ## 3    2.90    7.03
    ## 4    3.02    8.47
    ## 5    4.41    8.21
    ## 6    5.19    8.76
    ## 7    3.28   10.97
    ## 8    2.70    9.91

``` r
Argentina.day1
```

    ##   Year Month Day  Sunrise   Sunset Moonrise  Moonset moon_phase pressure
    ## 1 2021    10   3 05:59:21 18:51:22 05:30:00 15:10:00       0.90     1000
    ## 2 2021    10   4 05:57:04 18:53:01 05:49:00 16:32:00       0.93     1008
    ## 3 2021    10   5 05:54:48 18:54:39 06:06:00 17:54:00       0.97     1021
    ## 4 2021    10   6 05:52:32 18:56:19 06:23:00 19:19:00       0.00     1011
    ## 5 2021    10   7 05:50:17 18:57:58 06:40:00 20:46:00       0.04     1014
    ## 6 2021    10   8 05:48:02 18:59:38 06:59:00 22:15:00       0.08     1000
    ## 7 2021    10   9 05:45:48 19:01:19 07:22:00 19:00:00       0.12      998
    ## 8 2021    10  10 05:43:34 19:03:00 07:54:00 23:43:00       0.16     1003
    ##   humidity dew_point wind_speed wind_deg wind_gust clouds  pop  uvi mintemp
    ## 1       41     -1.53      15.66      247     23.06     89 0.02 3.93    3.87
    ## 2       40     -3.72      13.93      250     21.83     62 0.00 3.70    3.43
    ## 3       44     -3.89      12.86      230     18.04      7 0.00 4.36    1.37
    ## 4       49     -1.12      14.03      242     20.32    100 0.08 4.04    4.92
    ## 5       50     -0.79      12.39      238     18.88     47 0.00 5.04    1.84
    ## 6       50      2.89      15.40      241     20.63    100 0.00 0.71    6.64
    ## 7       43      1.98      12.75        9     21.18    100 0.00 1.00    4.63
    ## 8       50      3.00       5.99      266      9.34    100 0.00 1.00    6.34
    ##   maxtemp
    ## 1   11.72
    ## 2    9.02
    ## 3    9.52
    ## 4   12.89
    ## 5   12.14
    ## 6   12.90
    ## 7   17.26
    ## 8   13.51
