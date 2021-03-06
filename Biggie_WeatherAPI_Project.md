How to Access an API with an Example
================
Autumn Biggie
10/3/2021

<head>
 <script src="https://rstudio.github.io/leaflet/assets/jquery-1.11.1/jquery.min.js"></script>
 <meta name="viewport" content="width=device-width, initial-scale=1" /> 
 <link href="https://rstudio.github.io/leaflet/assets/bootstrap-3.3.1/css/bootstrap.min.css" rel="stylesheet" />
 <script src="https://rstudio.github.io/leaflet/assets/bootstrap-3.3.1/js/bootstrap.min.js"></script>
 <script src="https://rstudio.github.io/leaflet/assets/bootstrap-3.3.1/shim/html5shiv.min.js"></script>
 <script src="https://rstudio.github.io/leaflet/assets/bootstrap-3.3.1/shim/respond.min.js"></script>
 <script src="https://rstudio.github.io/leaflet/assets/htmlwidgets-0.5/htmlwidgets.js"></script>
 <link href="https://rstudio.github.io/leaflet/assets/leaflet-0.7.3/leaflet.css" rel="stylesheet" />
 <script src="https://rstudio.github.io/leaflet/assets/leaflet-0.7.3/leaflet.js"></script>
 <link href="https://rstudio.github.io/leaflet/assets/leafletfix-1.0.0/leafletfix.css" rel="stylesheet" />
 <script src="https://rstudio.github.io/leaflet/assets/leaflet-binding-1.0.1/leaflet.js"></script>
</head>

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

<div id="htmlwidget-41770cd5477d700326f1" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-41770cd5477d700326f1">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"setView":[[0,-80],1,[]],"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addMarkers","args":[[44.19,21.28,7.54,-55.05,-51.38],[-69.47,-71.08,-72.3,-67.05,-69.13],null,null,null,{"interactive":true,"draggable":false,"keyboard":true,"title":"","alt":"","zIndexOffset":0,"opacity":1,"riseOnHover":false,"riseOffset":250},["Augusta, Maine, US","Cockburn Town, Turks & Caicos Islands","Cucata, Colombia","Puerto Toro, Chile","Rio Gallegos, Argentina"],null,null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]}],"limits":{"lat":[-55.05,44.19],"lng":[-72.3,-67.05]}},"evals":[],"jsHooks":[]}</script>

Locations:

  - 44.19’N, 69.47’W (Augusta, Maine, US)  
  - 21.28’N, 71.08’W (Cockburn Town, Turks & Caicos Islands, UK)  
  - 7.54’N, 72.3’W (Cucata, Colombia)  
  - 51.38’S, 69.13’W (Rio Gallegos, Argentina)
  - 55.05’S, 67.05’W (Puerto Toro, Chile)

<!-- end list -->

``` r
Maine <- weather.api(latitude = "44.19", longitude = "-69.47", api.id = "input_your_key", exclude = "current, minutely, hourly")

Maine.day <- Maine$daily

Maine.day
```

    ##           dt    sunrise     sunset   moonrise    moonset moon_phase temp.day
    ## 1 1633449600 1633430429 1633471902 1633425720 1633471920       0.97    17.91
    ## 2 1633536000 1633516901 1633558193 1633516680 1633559760       0.00    20.26
    ## 3 1633622400 1633603373 1633644485 1633607760 1633647660       0.05    20.83
    ## 4 1633708800 1633689846 1633730778 1633698900 1633735860       0.08    20.83
    ## 5 1633795200 1633776319 1633817071 1633790160 1633824300       0.12    13.56
    ## 6 1633881600 1633862792 1633903365 1633881360 1633913280       0.16    14.10
    ## 7 1633968000 1633949266 1633989660 1633972140 1634002920       0.20    18.76
    ## 8 1634054400 1634035740 1634075955 1634062320 1634093100       0.25    23.38
    ##   temp.min temp.max temp.night temp.eve temp.morn feels_like.day
    ## 1     7.96    19.43      10.08    13.11      8.29          17.17
    ## 2     8.60    21.52      11.16    15.22      8.60          19.67
    ## 3     9.90    21.79      11.91    15.33      9.92          20.38
    ## 4     9.99    21.93      12.22    19.40     10.07          20.27
    ## 5     6.71    14.33       6.71    11.61     10.10          12.43
    ## 6     5.98    16.11      10.73    13.93      5.98          13.34
    ## 7     9.66    21.78      15.50    18.94      9.66          18.39
    ## 8    14.01    23.38      14.01    20.06     15.49          23.42
    ##   feels_like.night feels_like.eve feels_like.morn pressure humidity dew_point
    ## 1             9.55          12.57            7.53     1024       54      8.24
    ## 2            10.73          14.78            8.06     1027       51      9.69
    ## 3            11.64          14.96            9.92     1025       54     10.99
    ## 4            11.33          19.09            9.51     1022       50      9.93
    ## 5             6.71          10.52            8.89     1031       56      4.57
    ## 6            10.39          13.39            5.98     1029       68      8.04
    ## 7            15.56          18.87            9.66     1022       65     11.85
    ## 8            13.71          20.08           15.50     1015       63     15.63
    ##   wind_speed wind_deg wind_gust                           weather clouds  pop
    ## 1       1.89       23      3.60 804, Clouds, overcast clouds, 04d     94 0.04
    ## 2       1.87      345      3.02        800, Clear, clear sky, 01d      4 0.00
    ## 3       1.56      192      2.64        800, Clear, clear sky, 01d      2 0.00
    ## 4       4.48      105     11.21        800, Clear, clear sky, 01d      2 0.00
    ## 5       3.27       82      7.51   803, Clouds, broken clouds, 04d     77 0.00
    ## 6       3.26      199      7.43 804, Clouds, overcast clouds, 04d     99 0.00
    ## 7       2.34      201      5.02 804, Clouds, overcast clouds, 04d     98 0.00
    ## 8       4.35      319      9.96        500, Rain, light rain, 10d     15 0.39
    ##    uvi rain
    ## 1 3.98   NA
    ## 2 4.06   NA
    ## 3 3.85   NA
    ## 4 3.75   NA
    ## 5 3.41   NA
    ## 6 0.19   NA
    ## 7 1.00   NA
    ## 8 1.00  0.4

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

    ##   Year Month Day  Sunrise   Sunset Moonrise  Moonset moon_phase pressure
    ## 1 2021    10   5 06:40:29 18:11:42 05:22:00 18:12:00       0.97     1024
    ## 2 2021    10   6 06:41:41 18:09:53 06:38:00 18:36:00       0.00     1027
    ## 3 2021    10   7 06:42:53 18:08:05 07:56:00 19:01:00       0.05     1025
    ## 4 2021    10   8 06:44:06 18:06:18 09:15:00 19:31:00       0.08     1022
    ## 5 2021    10   9 06:45:19 18:04:31 10:36:00 20:05:00       0.12     1031
    ## 6 2021    10  10 06:46:32 18:02:45 11:56:00 20:48:00       0.16     1029
    ## 7 2021    10  11 06:47:46 18:01:00 13:09:00 21:42:00       0.20     1022
    ## 8 2021    10  12 06:49:00 17:59:15 14:12:00 22:45:00       0.25     1015
    ##   humidity dew_point wind_speed wind_deg wind_gust clouds  pop mintemp maxtemp
    ## 1       54      8.24       1.89       23      3.60     94 0.04    7.96   19.43
    ## 2       51      9.69       1.87      345      3.02      4 0.00    8.60   21.52
    ## 3       54     10.99       1.56      192      2.64      2 0.00    9.90   21.79
    ## 4       50      9.93       4.48      105     11.21      2 0.00    9.99   21.93
    ## 5       56      4.57       3.27       82      7.51     77 0.00    6.71   14.33
    ## 6       68      8.04       3.26      199      7.43     99 0.00    5.98   16.11
    ## 7       65     11.85       2.34      201      5.02     98 0.00    9.66   21.78
    ## 8       63     15.63       4.35      319      9.96     15 0.39   14.01   23.38

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

    ##          location Year Month Day  Sunrise   Sunset Moonrise  Moonset moon_phase
    ## 1       Maine, US 2021    10   5 06:40:29 18:11:42 05:22:00 18:12:00       0.97
    ## 2       Maine, US 2021    10   6 06:41:41 18:09:53 06:38:00 18:36:00       0.00
    ## 3       Maine, US 2021    10   7 06:42:53 18:08:05 07:56:00 19:01:00       0.05
    ## 4       Maine, US 2021    10   8 06:44:06 18:06:18 09:15:00 19:31:00       0.08
    ## 5       Maine, US 2021    10   9 06:45:19 18:04:31 10:36:00 20:05:00       0.12
    ## 6       Maine, US 2021    10  10 06:46:32 18:02:45 11:56:00 20:48:00       0.16
    ## 7       Maine, US 2021    10  11 06:47:46 18:01:00 13:09:00 21:42:00       0.20
    ## 8       Maine, US 2021    10  12 06:49:00 17:59:15 14:12:00 22:45:00       0.25
    ## 9  Turks & Caicos 2021    10   5 06:36:35 18:28:29 05:41:00 18:15:00       0.97
    ## 10 Turks & Caicos 2021    10   6 06:36:52 18:27:34 06:40:00 18:54:00       0.00
    ## 11 Turks & Caicos 2021    10   7 06:37:11 18:26:40 07:42:00 19:35:00       0.05
    ## 12 Turks & Caicos 2021    10   8 06:37:29 18:25:47 08:45:00 20:19:00       0.08
    ## 13 Turks & Caicos 2021    10   9 06:37:48 18:24:55 09:51:00 21:09:00       0.12
    ## 14 Turks & Caicos 2021    10  10 06:38:08 18:24:02 10:58:00 22:03:00       0.16
    ## 15 Turks & Caicos 2021    10  11 06:38:27 18:23:11 12:03:00 23:02:00       0.20
    ## 16 Turks & Caicos 2021    10  12 06:38:48 18:22:20 13:06:00 19:00:00       0.25
    ## 17       Colombia 2021    10   5 06:36:38 18:38:10 05:51:00 18:18:00       0.97
    ## 18       Colombia 2021    10   6 06:36:32 18:37:40 06:44:00 19:04:00       0.00
    ## 19       Colombia 2021    10   7 06:36:26 18:37:10 07:38:00 19:52:00       0.05
    ## 20       Colombia 2021    10   8 06:36:21 18:36:41 08:34:00 20:43:00       0.08
    ## 21       Colombia 2021    10   9 06:36:16 18:36:12 09:34:00 21:38:00       0.12
    ## 22       Colombia 2021    10  10 06:36:12 18:35:43 10:35:00 22:36:00       0.16
    ## 23       Colombia 2021    10  11 06:36:08 18:35:16 11:39:00 23:38:00       0.20
    ## 24       Colombia 2021    10  12 06:36:05 18:34:48 12:40:00 19:00:00       0.25
    ## 25      Argentina 2021    10   6 05:52:32 18:56:19 06:23:00 19:19:00       0.00
    ## 26      Argentina 2021    10   7 05:50:17 18:57:58 06:40:00 20:46:00       0.04
    ## 27      Argentina 2021    10   8 05:48:02 18:59:38 06:59:00 22:15:00       0.08
    ## 28      Argentina 2021    10   9 05:45:48 19:01:19 07:22:00 19:00:00       0.12
    ## 29      Argentina 2021    10  10 05:43:34 19:03:00 07:54:00 23:43:00       0.16
    ## 30      Argentina 2021    10  11 05:41:21 19:04:41 08:37:00 01:07:00       0.19
    ## 31      Argentina 2021    10  12 05:39:09 19:06:23 09:34:00 02:18:00       0.23
    ## 32      Argentina 2021    10  13 05:36:57 19:08:05 10:44:00 03:13:00       0.25
    ## 33          Chile 2021    10   6 05:39:55 18:52:17 06:13:00 19:14:00       0.00
    ## 34          Chile 2021    10   7 05:37:23 18:54:14 06:25:00 20:47:00       0.04
    ## 35          Chile 2021    10   8 05:34:51 18:56:11 06:40:00 22:21:00       0.08
    ## 36          Chile 2021    10   9 05:32:20 18:58:09 06:58:00 19:00:00       0.12
    ## 37          Chile 2021    10  10 05:29:49 19:00:07 07:24:00 23:56:00       0.16
    ## 38          Chile 2021    10  11 05:27:18 19:02:06 08:02:00 01:23:00       0.19
    ## 39          Chile 2021    10  12 05:24:49 19:04:05 08:57:00 02:37:00       0.23
    ## 40          Chile 2021    10  13 05:22:19 19:06:05 10:09:00 03:30:00       0.25
    ##    pressure humidity dew_point wind_speed wind_deg wind_gust clouds  pop
    ## 1      1024       54      8.24       1.89       23      3.60     94 0.04
    ## 2      1027       51      9.69       1.87      345      3.02      4 0.00
    ## 3      1025       54     10.99       1.56      192      2.64      2 0.00
    ## 4      1022       50      9.93       4.48      105     11.21      2 0.00
    ## 5      1031       56      4.57       3.27       82      7.51     77 0.00
    ## 6      1029       68      8.04       3.26      199      7.43     99 0.00
    ## 7      1022       65     11.85       2.34      201      5.02     98 0.00
    ## 8      1015       63     15.63       4.35      319      9.96     15 0.39
    ## 9      1015       76     23.91      11.38       96     12.72     98 0.97
    ## 10     1017       73     23.48      11.38      100     12.80      3 0.46
    ## 11     1016       74     23.33       9.85       88     10.70      3 0.69
    ## 12     1014       76     23.69       8.10       89      8.54      2 0.64
    ## 13     1014       76     23.87       8.69      110      9.06      5 0.66
    ## 14     1015       75     23.82       9.18      120      9.56     99 0.40
    ## 15     1015       77     23.89       8.51      124      8.99     95 0.40
    ## 16     1015       77     23.95       8.72       80      9.39     75 0.96
    ## 17     1014       82     16.23       2.80      111      4.36     95 0.97
    ## 18     1016       83     15.39       2.63      113      4.41    100 0.82
    ## 19     1014       50     12.54       3.31      102      5.07     65 0.85
    ## 20     1014       72     15.23       2.01      104      2.94     80 1.00
    ## 21     1015       60     13.54       2.12       95      2.72    100 1.00
    ## 22     1015       66     14.73       1.83      111      2.81     43 1.00
    ## 23     1016       77     15.67       1.97      109      3.03     94 1.00
    ## 24     1016       73     15.98       2.15      106      2.98    100 1.00
    ## 25     1010       39     -4.04      14.48      241     20.62    100 0.20
    ## 26     1014       59      1.03      13.21      242     18.16     43 0.00
    ## 27     1000       43      2.19      13.13      230     16.16    100 0.00
    ## 28      997       46      3.73      13.20      318     21.65    100 0.35
    ## 29     1006       62      2.28      10.85       25     13.41     12 0.89
    ## 30     1007       42     -2.38       6.71       25      8.64     81 0.00
    ## 31      988       38     -1.03      11.42      332     15.32    100 0.01
    ## 32      992       41     -4.98      12.41      251     16.71      5 0.39
    ## 33     1001       59      1.10       7.10      244     13.10     96 0.90
    ## 34     1008       58     -0.61       8.44      236     14.17     98 0.00
    ## 35      989       59      2.31      10.15      257     19.04     99 0.60
    ## 36      988       85      3.47       8.70      333     20.81    100 0.64
    ## 37     1000       61     -0.51      10.79      322     21.31     17 0.13
    ## 38     1000       55     -0.98       8.14      330     15.48     59 0.07
    ## 39      986       63      2.60       5.21      359     10.83    100 0.00
    ## 40      984       53     -3.25       7.81      320     13.95     96 0.84
    ##    mintemp maxtemp humidity.status cloud.coverage
    ## 1     7.96   19.43             Low           High
    ## 2     8.60   21.52             Low            Low
    ## 3     9.90   21.79             Low            Low
    ## 4     9.99   21.93             Low            Low
    ## 5     6.71   14.33             Low           High
    ## 6     5.98   16.11          Medium           High
    ## 7     9.66   21.78          Medium           High
    ## 8    14.01   23.38          Medium            Low
    ## 9    28.42   28.76          Medium           High
    ## 10   28.52   28.76          Medium            Low
    ## 11   28.09   28.64          Medium            Low
    ## 12   27.88   28.54          Medium            Low
    ## 13   28.11   28.76          Medium            Low
    ## 14   28.55   28.90          Medium           High
    ## 15   28.39   28.70          Medium           High
    ## 16   28.23   28.66          Medium         Medium
    ## 17   13.02   18.85            High           High
    ## 18   12.94   20.04            High           High
    ## 19   12.16   22.78             Low         Medium
    ## 20   12.22   20.53          Medium           High
    ## 21   13.24   20.51             Low           High
    ## 22   12.73   20.25          Medium         Medium
    ## 23   12.55   19.68          Medium           High
    ## 24   14.04   20.23          Medium           High
    ## 25    1.34   12.61             Low           High
    ## 26    3.24    9.67             Low         Medium
    ## 27    6.48   14.69             Low           High
    ## 28    5.83   16.58             Low           High
    ## 29    3.94   12.20          Medium            Low
    ## 30    5.28   11.35             Low           High
    ## 31    6.66   14.54             Low           High
    ## 32    3.21    8.22             Low            Low
    ## 33    1.72    9.94             Low           High
    ## 34    1.95    8.16             Low           High
    ## 35    4.42   10.65             Low           High
    ## 36    2.05    9.95            High           High
    ## 37    2.07    8.73          Medium            Low
    ## 38    3.08    8.76             Low         Medium
    ## 39    3.99   11.44          Medium           High
    ## 40    1.52    6.84             Low           High

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
    ##                  Low      4      3   12
    ##                  Medium   7      2    9
    ##                  High     0      0    3

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
    ##                  Medium   1      0    0
    ##                  High     0      0    0
    ## 
    ## , , weather$location = Chile
    ## 
    ##                        weather$cloud.coverage
    ## weather$humidity.status Low Medium High
    ##                  Low      0      1    4
    ##                  Medium   1      0    1
    ##                  High     0      0    1
    ## 
    ## , , weather$location = Colombia
    ## 
    ##                        weather$cloud.coverage
    ## weather$humidity.status Low Medium High
    ##                  Low      0      1    1
    ##                  Medium   0      1    3
    ##                  High     0      0    2
    ## 
    ## , , weather$location = Maine, US
    ## 
    ##                        weather$cloud.coverage
    ## weather$humidity.status Low Medium High
    ##                  Low      3      0    2
    ##                  Medium   1      0    2
    ##                  High     0      0    0
    ## 
    ## , , weather$location = Turks & Caicos
    ## 
    ##                        weather$cloud.coverage
    ## weather$humidity.status Low Medium High
    ##                  Low      0      0    0
    ##                  Medium   4      1    3
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
    ## 1 Chile             9.31   2.6   1.47  1.10  1.54 
    ## 2 Argentina        12.5    4.50  2.76  1.87  3.65 
    ## 3 Colombia         20.4   12.9   1.12  0.607 0.565
    ## 4 Turks & Caicos   28.7   28.3   0.107 0.236 0.105
    ## 5 Maine, US        20.0    9.10  3.19  2.47  3.23

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
    ## 1 Chile                  61.6        9.96
    ## 2 Argentina              46.2        9.16
    ## 3 Colombia               70.4       11.3 
    ## 4 Turks & Caicos         75.5        1.41
    ## 5 Maine, US              57.6        6.78

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

![](Biggie_WeatherAPI_Project_files/figure-gfm/bar%20graph-1.png)<!-- -->

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

![](Biggie_WeatherAPI_Project_files/figure-gfm/maxtemp%20boxplot-1.png)<!-- -->

``` r
min.means <- weather %>% group_by(location) %>% summarise(average = mean(mintemp))

g1 <- ggplot(weather, aes(x = location, y = mintemp))

g1 + geom_boxplot(fill = "grey") + geom_point(min.means, mapping = aes(x = location, y = average), color = "green") + geom_line(min.means, mapping = aes(x = location, y = average, group = 1), color = "green") + labs(title = "8 Day Forecast: Minimum Daily Temperature", x = "Location (highest to lowest latitude)", y = "Minimum Daily Temperature (C)") + coord_flip()
```

![](Biggie_WeatherAPI_Project_files/figure-gfm/mintemp%20boxplot-1.png)<!-- -->

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

![](Biggie_WeatherAPI_Project_files/figure-gfm/humidity%20hist%20and%20density%20plot-1.png)<!-- -->

``` r
hum.means <- weather %>% group_by(location) %>% summarise(average = mean(humidity))

g3 <- ggplot(weather, aes(x = location, y = humidity))

g3 + geom_boxplot(fill = "grey") + geom_point(hum.means, mapping = aes(x = location, y = average), color = "red") + geom_line(hum.means, mapping = aes(x = location, y = average, group = 1), color = "red") + labs(title = "8 Day Forecast: Average Daily Humidity by Location", x = "Location (highest to lowest latitude)", y = "Average Daily Humidity (C)") + coord_flip()
```

![](Biggie_WeatherAPI_Project_files/figure-gfm/humidity%20boxplot-1.png)<!-- -->

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

    ##          location Year Month Day Sunrise  Sunset Moonrise Moonset moon_phase
    ## 1       Maine, US 2021    10   5  400.48 1091.70      322    1092       0.97
    ## 2       Maine, US 2021    10   6  401.68 1089.88      398    1116       0.00
    ## 3       Maine, US 2021    10   7  402.88 1088.08      476    1141       0.05
    ## 4       Maine, US 2021    10   8  404.10 1086.30      555    1171       0.08
    ## 5       Maine, US 2021    10   9  405.32 1084.52      636    1205       0.12
    ## 6       Maine, US 2021    10  10  406.53 1082.75      716    1248       0.16
    ## 7       Maine, US 2021    10  11  407.77 1081.00      789    1302       0.20
    ## 8       Maine, US 2021    10  12  409.00 1079.25      852    1365       0.25
    ## 9  Turks & Caicos 2021    10   5  396.58 1108.48      341    1095       0.97
    ## 10 Turks & Caicos 2021    10   6  396.87 1107.57      400    1134       0.00
    ## 11 Turks & Caicos 2021    10   7  397.18 1106.67      462    1175       0.05
    ## 12 Turks & Caicos 2021    10   8  397.48 1105.78      525    1219       0.08
    ## 13 Turks & Caicos 2021    10   9  397.80 1104.92      591    1269       0.12
    ## 14 Turks & Caicos 2021    10  10  398.13 1104.03      658    1323       0.16
    ## 15 Turks & Caicos 2021    10  11  398.45 1103.18      723    1382       0.20
    ## 16 Turks & Caicos 2021    10  12  398.80 1102.33      786    1140       0.25
    ## 17       Colombia 2021    10   5  396.63 1118.17      351    1098       0.97
    ## 18       Colombia 2021    10   6  396.53 1117.67      404    1144       0.00
    ## 19       Colombia 2021    10   7  396.43 1117.17      458    1192       0.05
    ## 20       Colombia 2021    10   8  396.35 1116.68      514    1243       0.08
    ## 21       Colombia 2021    10   9  396.27 1116.20      574    1298       0.12
    ## 22       Colombia 2021    10  10  396.20 1115.72      635    1356       0.16
    ## 23       Colombia 2021    10  11  396.13 1115.27      699    1418       0.20
    ## 24       Colombia 2021    10  12  396.08 1114.80      760    1140       0.25
    ## 25      Argentina 2021    10   6  352.53 1136.32      383    1159       0.00
    ## 26      Argentina 2021    10   7  350.28 1137.97      400    1246       0.04
    ## 27      Argentina 2021    10   8  348.03 1139.63      419    1335       0.08
    ## 28      Argentina 2021    10   9  345.80 1141.32      442    1140       0.12
    ## 29      Argentina 2021    10  10  343.57 1143.00      474    1423       0.16
    ## 30      Argentina 2021    10  11  341.35 1144.68      517      67       0.19
    ## 31      Argentina 2021    10  12  339.15 1146.38      574     138       0.23
    ## 32      Argentina 2021    10  13  336.95 1148.08      644     193       0.25
    ## 33          Chile 2021    10   6  339.92 1132.28      373    1154       0.00
    ## 34          Chile 2021    10   7  337.38 1134.23      385    1247       0.04
    ## 35          Chile 2021    10   8  334.85 1136.18      400    1341       0.08
    ## 36          Chile 2021    10   9  332.33 1138.15      418    1140       0.12
    ## 37          Chile 2021    10  10  329.82 1140.12      444    1436       0.16
    ## 38          Chile 2021    10  11  327.30 1142.10      482      83       0.19
    ## 39          Chile 2021    10  12  324.82 1144.08      537     157       0.23
    ## 40          Chile 2021    10  13  322.32 1146.08      609     210       0.25
    ##    pressure humidity dew_point wind_speed wind_deg wind_gust clouds  pop
    ## 1      1024       54      8.24       1.89       23      3.60     94 0.04
    ## 2      1027       51      9.69       1.87      345      3.02      4 0.00
    ## 3      1025       54     10.99       1.56      192      2.64      2 0.00
    ## 4      1022       50      9.93       4.48      105     11.21      2 0.00
    ## 5      1031       56      4.57       3.27       82      7.51     77 0.00
    ## 6      1029       68      8.04       3.26      199      7.43     99 0.00
    ## 7      1022       65     11.85       2.34      201      5.02     98 0.00
    ## 8      1015       63     15.63       4.35      319      9.96     15 0.39
    ## 9      1015       76     23.91      11.38       96     12.72     98 0.97
    ## 10     1017       73     23.48      11.38      100     12.80      3 0.46
    ## 11     1016       74     23.33       9.85       88     10.70      3 0.69
    ## 12     1014       76     23.69       8.10       89      8.54      2 0.64
    ## 13     1014       76     23.87       8.69      110      9.06      5 0.66
    ## 14     1015       75     23.82       9.18      120      9.56     99 0.40
    ## 15     1015       77     23.89       8.51      124      8.99     95 0.40
    ## 16     1015       77     23.95       8.72       80      9.39     75 0.96
    ## 17     1014       82     16.23       2.80      111      4.36     95 0.97
    ## 18     1016       83     15.39       2.63      113      4.41    100 0.82
    ## 19     1014       50     12.54       3.31      102      5.07     65 0.85
    ## 20     1014       72     15.23       2.01      104      2.94     80 1.00
    ## 21     1015       60     13.54       2.12       95      2.72    100 1.00
    ## 22     1015       66     14.73       1.83      111      2.81     43 1.00
    ## 23     1016       77     15.67       1.97      109      3.03     94 1.00
    ## 24     1016       73     15.98       2.15      106      2.98    100 1.00
    ## 25     1010       39     -4.04      14.48      241     20.62    100 0.20
    ## 26     1014       59      1.03      13.21      242     18.16     43 0.00
    ## 27     1000       43      2.19      13.13      230     16.16    100 0.00
    ## 28      997       46      3.73      13.20      318     21.65    100 0.35
    ## 29     1006       62      2.28      10.85       25     13.41     12 0.89
    ## 30     1007       42     -2.38       6.71       25      8.64     81 0.00
    ## 31      988       38     -1.03      11.42      332     15.32    100 0.01
    ## 32      992       41     -4.98      12.41      251     16.71      5 0.39
    ## 33     1001       59      1.10       7.10      244     13.10     96 0.90
    ## 34     1008       58     -0.61       8.44      236     14.17     98 0.00
    ## 35      989       59      2.31      10.15      257     19.04     99 0.60
    ## 36      988       85      3.47       8.70      333     20.81    100 0.64
    ## 37     1000       61     -0.51      10.79      322     21.31     17 0.13
    ## 38     1000       55     -0.98       8.14      330     15.48     59 0.07
    ## 39      986       63      2.60       5.21      359     10.83    100 0.00
    ## 40      984       53     -3.25       7.81      320     13.95     96 0.84
    ##    mintemp maxtemp humidity.status cloud.coverage
    ## 1     7.96   19.43             Low           High
    ## 2     8.60   21.52             Low            Low
    ## 3     9.90   21.79             Low            Low
    ## 4     9.99   21.93             Low            Low
    ## 5     6.71   14.33             Low           High
    ## 6     5.98   16.11          Medium           High
    ## 7     9.66   21.78          Medium           High
    ## 8    14.01   23.38          Medium            Low
    ## 9    28.42   28.76          Medium           High
    ## 10   28.52   28.76          Medium            Low
    ## 11   28.09   28.64          Medium            Low
    ## 12   27.88   28.54          Medium            Low
    ## 13   28.11   28.76          Medium            Low
    ## 14   28.55   28.90          Medium           High
    ## 15   28.39   28.70          Medium           High
    ## 16   28.23   28.66          Medium         Medium
    ## 17   13.02   18.85            High           High
    ## 18   12.94   20.04            High           High
    ## 19   12.16   22.78             Low         Medium
    ## 20   12.22   20.53          Medium           High
    ## 21   13.24   20.51             Low           High
    ## 22   12.73   20.25          Medium         Medium
    ## 23   12.55   19.68          Medium           High
    ## 24   14.04   20.23          Medium           High
    ## 25    1.34   12.61             Low           High
    ## 26    3.24    9.67             Low         Medium
    ## 27    6.48   14.69             Low           High
    ## 28    5.83   16.58             Low           High
    ## 29    3.94   12.20          Medium            Low
    ## 30    5.28   11.35             Low           High
    ## 31    6.66   14.54             Low           High
    ## 32    3.21    8.22             Low            Low
    ## 33    1.72    9.94             Low           High
    ## 34    1.95    8.16             Low           High
    ## 35    4.42   10.65             Low           High
    ## 36    2.05    9.95            High           High
    ## 37    2.07    8.73          Medium            Low
    ## 38    3.08    8.76             Low         Medium
    ## 39    3.99   11.44          Medium           High
    ## 40    1.52    6.84             Low           High

The scatterplot below allows us to look at how sunrise time changes over
the course of 8 days in each location.

``` r
g4 <- ggplot(weather, aes(x = Day, y = Sunrise))

g4 + geom_point(aes(color = location)) + scale_color_discrete(name = "Location") + labs(title = "8 Day Forecast for Sunrise Time by Location", x = "Day of the Month", y = "Sunrise Time (minutes after 12am)")
```

![](Biggie_WeatherAPI_Project_files/figure-gfm/sunrise%20scatterplot-1.png)<!-- -->

I think this plot is especially cool to look at, since one can clearly
distinguish which locations are in the southern hemisphere, and which
are in the northern hemisphere. While the sun is rising increasingly
later in the northern hemisphere, it’s rising increasingly earlier in
the southern hemisphere.

Lastly, we’ll explore the relationship between maximum daily temperature
and daily pressure in the scatterplot below.

``` r
g5 <- ggplot(weather, aes(x = maxtemp, y = pressure))

g5 + geom_point(aes(color = location)) + geom_smooth(method = lm, formula = y~poly(x,2), color = "black") + scale_color_discrete(name = "Location") + labs(title = "8 Day Forecast: Maximum Daily Temperature vs. Daily Pressure", x = "Maximum Daily Temperature (C)", y = "Daily Pressure (millibars)")
```

![](Biggie_WeatherAPI_Project_files/figure-gfm/maxtemp%20vs%20pressure%20scatterplot-1.png)<!-- -->

There seems to be a medium-strength, positive relationship between
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
