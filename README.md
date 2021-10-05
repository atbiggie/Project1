How to Access an API with an Example
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

<div id="htmlwidget-3e7349e0cdada530d782" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-3e7349e0cdada530d782">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"setView":[[0,-80],1,[]],"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addMarkers","args":[[44.19,21.28,7.54,-55.05,-51.38],[-69.47,-71.08,-72.3,-67.05,-69.13],null,null,null,{"interactive":true,"draggable":false,"keyboard":true,"title":"","alt":"","zIndexOffset":0,"opacity":1,"riseOnHover":false,"riseOffset":250},["Augusta, Maine, US","Cockburn Town, Turks & Caicos Islands","Cucata, Colombia","Puerto Toro, Chile","Rio Gallegos, Argentina"],null,null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]}],"limits":{"lat":[-55.05,44.19],"lng":[-72.3,-67.05]}},"evals":[],"jsHooks":[]}</script>

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
    ## 1 1633363200 1633343957 1633385611 1633334820 1633384140       0.93    15.84
    ## 2 1633449600 1633430429 1633471902 1633425720 1633471920       0.97    16.05
    ## 3 1633536000 1633516901 1633558193 1633516680 1633559760       0.00    19.72
    ## 4 1633622400 1633603373 1633644485 1633607760 1633647660       0.05    20.57
    ## 5 1633708800 1633689846 1633730778 1633698900 1633735860       0.08    19.44
    ## 6 1633795200 1633776319 1633817071 1633790160 1633824300       0.12    12.77
    ## 7 1633881600 1633862792 1633903365 1633881360 1633913280       0.16    13.15
    ## 8 1633968000 1633949266 1633989660 1633972140 1634002920       0.20    16.66
    ##   temp.min temp.max temp.night temp.eve temp.morn feels_like.day
    ## 1     5.47    17.14       9.91    11.06      5.47          14.79
    ## 2     8.28    17.15       8.82    12.05      9.30          15.17
    ## 3     7.71    20.90      10.25    14.28      7.71          19.03
    ## 4     9.41    21.69      11.59    19.28      9.41          20.07
    ## 5     9.83    21.58      10.21    17.51      9.83          18.85
    ## 6     5.68    14.04       5.68    11.77      7.03          11.46
    ## 7     4.72    15.20       7.81    12.15      4.72          12.01
    ## 8     8.19    17.46      15.40    15.59      8.41          16.21
    ##   feels_like.night feels_like.eve feels_like.morn pressure humidity dew_point
    ## 1             9.91          10.36            3.69     1021       50      5.14
    ## 2             8.82          11.45            8.93     1025       56      7.05
    ## 3             9.71          13.72            6.52     1027       49      8.53
    ## 4            11.02          18.91            9.41     1023       53     10.34
    ## 5             9.71          17.07            9.11     1021       54      9.74
    ## 6             5.68          10.62            5.68     1033       52      3.09
    ## 7             6.92          11.33            4.72     1033       57      4.64
    ## 8            15.50          15.40            7.89     1025       70     10.92
    ##   wind_speed wind_deg wind_gust                           weather clouds  pop
    ## 1       2.52       36      8.33   803, Clouds, broken clouds, 04d     83 0.29
    ## 2       1.63       22      2.84 804, Clouds, overcast clouds, 04d    100 0.07
    ## 3       2.04      356      3.30        800, Clear, clear sky, 01d      3 0.00
    ## 4       1.72      286      2.72        800, Clear, clear sky, 01d      0 0.00
    ## 5       3.31      357      7.93        800, Clear, clear sky, 01d      6 0.00
    ## 6       2.84      147      5.98        800, Clear, clear sky, 01d      6 0.00
    ## 7       3.26      183      5.54 804, Clouds, overcast clouds, 04d    100 0.00
    ## 8       4.45      210     13.61 804, Clouds, overcast clouds, 04d     90 0.00
    ##    uvi
    ## 1 4.00
    ## 2 3.41
    ## 3 4.10
    ## 4 3.91
    ## 5 3.58
    ## 6 0.13
    ## 7 1.00
    ## 8 1.00

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
    ## 1 2021    10   4 06:39:17 18:13:31 04:07:00 17:49:00       0.93     1021
    ## 2 2021    10   5 06:40:29 18:11:42 05:22:00 18:12:00       0.97     1025
    ## 3 2021    10   6 06:41:41 18:09:53 06:38:00 18:36:00       0.00     1027
    ## 4 2021    10   7 06:42:53 18:08:05 07:56:00 19:01:00       0.05     1023
    ## 5 2021    10   8 06:44:06 18:06:18 09:15:00 19:31:00       0.08     1021
    ## 6 2021    10   9 06:45:19 18:04:31 10:36:00 20:05:00       0.12     1033
    ## 7 2021    10  10 06:46:32 18:02:45 11:56:00 20:48:00       0.16     1033
    ## 8 2021    10  11 06:47:46 18:01:00 13:09:00 21:42:00       0.20     1025
    ##   humidity dew_point wind_speed wind_deg wind_gust clouds  pop mintemp maxtemp
    ## 1       50      5.14       2.52       36      8.33     83 0.29    5.47   17.14
    ## 2       56      7.05       1.63       22      2.84    100 0.07    8.28   17.15
    ## 3       49      8.53       2.04      356      3.30      3 0.00    7.71   20.90
    ## 4       53     10.34       1.72      286      2.72      0 0.00    9.41   21.69
    ## 5       54      9.74       3.31      357      7.93      6 0.00    9.83   21.58
    ## 6       52      3.09       2.84      147      5.98      6 0.00    5.68   14.04
    ## 7       57      4.64       3.26      183      5.54    100 0.00    4.72   15.20
    ## 8       70     10.92       4.45      210     13.61     90 0.00    8.19   17.46

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

I’ll begin with humidity. Lets say that if \(humidity \le 60\), there is
low humidity, if \(60 < humidity \le 80\), there is medium humidity, and
if \(humidity > 80\), there is high humidity.

``` r
weather <- weather %>% mutate(humidity.status = as.factor(ifelse(humidity > 80, "High", ifelse(humidity >60, "Medium", "Low"))))

weather$humidity.status <- ordered(weather$humidity.status, levels = c("Low", "Medium", "High"))
```

Great\! Now let’s look at `clouds`. If \(clouds \le 25\), then cloud
coverage is low. If \(clouds > 75\) then cloud coverage is high, and
anything in between is medium.

``` r
weather <- weather %>% mutate(cloud.coverage = as.factor(ifelse(clouds > 75, "High", ifelse(clouds > 25, "Medium", "Low"))))

weather$cloud.coverage <- ordered(weather$cloud.coverage, levels = c("Low", "Medium", "High"))
```

Here’s our cleaned dataset:

``` r
weather
```

    ##          location Year Month Day  Sunrise   Sunset Moonrise  Moonset moon_phase
    ## 1       Maine, US 2021    10   4 06:39:17 18:13:31 04:07:00 17:49:00       0.93
    ## 2       Maine, US 2021    10   5 06:40:29 18:11:42 05:22:00 18:12:00       0.97
    ## 3       Maine, US 2021    10   6 06:41:41 18:09:53 06:38:00 18:36:00       0.00
    ## 4       Maine, US 2021    10   7 06:42:53 18:08:05 07:56:00 19:01:00       0.05
    ## 5       Maine, US 2021    10   8 06:44:06 18:06:18 09:15:00 19:31:00       0.08
    ## 6       Maine, US 2021    10   9 06:45:19 18:04:31 10:36:00 20:05:00       0.12
    ## 7       Maine, US 2021    10  10 06:46:32 18:02:45 11:56:00 20:48:00       0.16
    ## 8       Maine, US 2021    10  11 06:47:46 18:01:00 13:09:00 21:42:00       0.20
    ## 9  Turks & Caicos 2021    10   4 06:36:17 18:29:23 04:42:00 17:36:00       0.93
    ## 10 Turks & Caicos 2021    10   5 06:36:35 18:28:29 05:41:00 18:15:00       0.97
    ## 11 Turks & Caicos 2021    10   6 06:36:52 18:27:34 06:40:00 18:54:00       0.00
    ## 12 Turks & Caicos 2021    10   7 06:37:11 18:26:40 07:42:00 19:35:00       0.05
    ## 13 Turks & Caicos 2021    10   8 06:37:29 18:25:47 08:45:00 20:19:00       0.08
    ## 14 Turks & Caicos 2021    10   9 06:37:48 18:24:55 09:51:00 21:09:00       0.12
    ## 15 Turks & Caicos 2021    10  10 06:38:08 18:24:02 10:58:00 22:03:00       0.16
    ## 16 Turks & Caicos 2021    10  11 06:38:27 18:23:11 12:03:00 23:02:00       0.20
    ## 17       Colombia 2021    10   4 06:36:45 18:38:41 04:59:00 17:33:00       0.93
    ## 18       Colombia 2021    10   5 06:36:38 18:38:10 05:51:00 18:18:00       0.97
    ## 19       Colombia 2021    10   6 06:36:32 18:37:40 06:44:00 19:04:00       0.00
    ## 20       Colombia 2021    10   7 06:36:26 18:37:10 07:38:00 19:52:00       0.05
    ## 21       Colombia 2021    10   8 06:36:21 18:36:41 08:34:00 20:43:00       0.08
    ## 22       Colombia 2021    10   9 06:36:16 18:36:12 09:34:00 21:38:00       0.12
    ## 23       Colombia 2021    10  10 06:36:12 18:35:43 10:35:00 22:36:00       0.16
    ## 24       Colombia 2021    10  11 06:36:08 18:35:16 11:39:00 23:38:00       0.20
    ## 25      Argentina 2021    10   4 05:57:04 18:53:01 05:49:00 16:32:00       0.93
    ## 26      Argentina 2021    10   5 05:54:48 18:54:39 06:06:00 17:54:00       0.97
    ## 27      Argentina 2021    10   6 05:52:32 18:56:19 06:23:00 19:19:00       0.00
    ## 28      Argentina 2021    10   7 05:50:17 18:57:58 06:40:00 20:46:00       0.04
    ## 29      Argentina 2021    10   8 05:48:02 18:59:38 06:59:00 22:15:00       0.08
    ## 30      Argentina 2021    10   9 05:45:48 19:01:19 07:22:00 19:00:00       0.12
    ## 31      Argentina 2021    10  10 05:43:34 19:03:00 07:54:00 23:43:00       0.16
    ## 32      Argentina 2021    10  11 05:41:21 19:04:41 08:37:00 01:07:00       0.19
    ## 33          Chile 2021    10   4 05:45:02 18:48:25 05:49:00 16:16:00       0.93
    ## 34          Chile 2021    10   5 05:42:28 18:50:21 06:01:00 17:44:00       0.97
    ## 35          Chile 2021    10   6 05:39:55 18:52:17 06:13:00 19:14:00       0.00
    ## 36          Chile 2021    10   7 05:37:23 18:54:14 06:25:00 20:47:00       0.04
    ## 37          Chile 2021    10   8 05:34:51 18:56:11 06:40:00 22:21:00       0.08
    ## 38          Chile 2021    10   9 05:32:20 18:58:09 06:58:00 19:00:00       0.12
    ## 39          Chile 2021    10  10 05:29:49 19:00:07 07:24:00 23:56:00       0.16
    ## 40          Chile 2021    10  11 05:27:18 19:02:06 08:02:00 01:23:00       0.19
    ##    pressure humidity dew_point wind_speed wind_deg wind_gust clouds  pop
    ## 1      1021       50      5.14       2.52       36      8.33     83 0.29
    ## 2      1025       56      7.05       1.63       22      2.84    100 0.07
    ## 3      1027       49      8.53       2.04      356      3.30      3 0.00
    ## 4      1023       53     10.34       1.72      286      2.72      0 0.00
    ## 5      1021       54      9.74       3.31      357      7.93      6 0.00
    ## 6      1033       52      3.09       2.84      147      5.98      6 0.00
    ## 7      1033       57      4.64       3.26      183      5.54    100 0.00
    ## 8      1025       70     10.92       4.45      210     13.61     90 0.00
    ## 9      1014       74     23.78      10.19      123     11.06    100 1.00
    ## 10     1015       72     23.19      11.24      100     12.18     96 0.96
    ## 11     1016       72     23.21      11.30      101     12.27     20 0.58
    ## 12     1016       75     23.68       9.80       96     10.61     12 0.60
    ## 13     1015       75     23.64       7.17      101      7.55      2 0.40
    ## 14     1014       75     23.93       8.30      117      8.66     18 1.00
    ## 15     1014       75     23.89       9.21      117      9.63     97 0.71
    ## 16     1014       76     23.78       8.69      122      9.06    100 0.85
    ## 17     1013       58     13.69       2.96      103      4.12     89 0.77
    ## 18     1014       80     16.16       2.56      102      3.97     95 0.97
    ## 19     1016       83     15.60       2.65      119      4.62    100 0.92
    ## 20     1014       53     12.58       3.14      102      4.99     77 0.85
    ## 21     1015       65     13.92       2.26      108      3.27    100 1.00
    ## 22     1015       64     14.01       1.84       93      2.45     97 0.97
    ## 23     1015       67     15.04       1.71       80      2.44     79 1.00
    ## 24     1015       73     15.72       1.97      104      2.97    100 1.00
    ## 25     1007       41     -3.51      14.83      249     23.00     58 0.00
    ## 26     1021       43     -4.53      13.24      226     18.51     35 0.00
    ## 27     1011       41     -4.16      12.92      239     19.07    100 0.02
    ## 28     1014       54      1.49      10.69      249     16.84     38 0.00
    ## 29     1004       48      1.21      11.13      229     13.73    100 0.17
    ## 30     1001       49      3.74      13.54      319     20.16    100 0.00
    ## 31     1009       58      0.06      13.70      246     21.13     44 0.78
    ## 32     1011       41     -3.60      10.15      249     18.55     65 0.00
    ## 33      997       66     -0.99      12.76      258     23.26     45 0.75
    ## 34     1017       66     -1.29      10.45      228     17.19     65 0.84
    ## 35     1001       61      1.44      10.27      245     17.37     99 0.84
    ## 36     1009       47     -3.03       8.89      241     16.62     54 0.26
    ## 37      997       79      3.23       7.50      338     15.03     97 0.44
    ## 38      997       86      2.95      10.54      326     20.98    100 0.56
    ## 39     1000       44     -4.03      10.43      291     18.43     75 0.31
    ## 40     1001       53     -2.29      11.14      336     20.05     22 0.50
    ##    mintemp maxtemp humidity.status cloud.coverage
    ## 1     5.47   17.14             Low           High
    ## 2     8.28   17.15             Low           High
    ## 3     7.71   20.90             Low            Low
    ## 4     9.41   21.69             Low            Low
    ## 5     9.83   21.58             Low            Low
    ## 6     5.68   14.04             Low            Low
    ## 7     4.72   15.20             Low           High
    ## 8     8.19   17.46          Medium           High
    ## 9    26.97   28.87          Medium           High
    ## 10   28.41   28.78          Medium           High
    ## 11   28.20   28.73          Medium            Low
    ## 12   28.36   28.65          Medium            Low
    ## 13   28.16   28.64          Medium            Low
    ## 14   28.00   28.77          Medium            Low
    ## 15   28.62   28.85          Medium           High
    ## 16   28.07   28.74          Medium           High
    ## 17   12.85   21.43             Low           High
    ## 18   12.85   19.54          Medium           High
    ## 19   12.88   19.37            High           High
    ## 20   12.76   22.68             Low           High
    ## 21   12.38   19.99          Medium           High
    ## 22   13.33   19.94          Medium           High
    ## 23   12.92   20.52          Medium           High
    ## 24   12.49   19.63          Medium           High
    ## 25    3.34   10.00             Low         Medium
    ## 26    1.85    9.09             Low         Medium
    ## 27    4.22   12.12             Low           High
    ## 28    4.45   10.37             Low         Medium
    ## 29    5.87   14.51             Low           High
    ## 30    6.37   17.03             Low           High
    ## 31    4.35   10.89             Low         Medium
    ## 32    4.06   10.42             Low         Medium
    ## 33    2.11    5.49          Medium         Medium
    ## 34    1.99    6.22          Medium         Medium
    ## 35    1.53    9.19          Medium           High
    ## 36    3.95    8.65             Low         Medium
    ## 37    4.77    7.70          Medium           High
    ## 38    2.54   10.86            High           High
    ## 39    3.96    9.06             Low         Medium
    ## 40    2.43    8.02             Low            Low

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
    ##                  Low      5      7    8
    ##                  Medium   4      2   12
    ##                  High     0      0    2

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
    ##                  Low      0      5    3
    ##                  Medium   0      0    0
    ##                  High     0      0    0
    ## 
    ## , , weather$location = Chile
    ## 
    ##                        weather$cloud.coverage
    ## weather$humidity.status Low Medium High
    ##                  Low      1      2    0
    ##                  Medium   0      2    2
    ##                  High     0      0    1
    ## 
    ## , , weather$location = Colombia
    ## 
    ##                        weather$cloud.coverage
    ## weather$humidity.status Low Medium High
    ##                  Low      0      0    2
    ##                  Medium   0      0    5
    ##                  High     0      0    1
    ## 
    ## , , weather$location = Maine, US
    ## 
    ##                        weather$cloud.coverage
    ## weather$humidity.status Low Medium High
    ##                  Low      4      0    3
    ##                  Medium   0      0    1
    ##                  High     0      0    0
    ## 
    ## , , weather$location = Turks & Caicos
    ## 
    ##                        weather$cloud.coverage
    ## weather$humidity.status Low Medium High
    ##                  Low      0      0    0
    ##                  Medium   4      0    4
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
    ##   location       avghigh avglow sdhigh sdlow    IQR
    ##   <ord>            <dbl>  <dbl>  <dbl> <dbl>  <dbl>
    ## 1 Chile             8.15   2.91 1.71   1.16  1.76  
    ## 2 Argentina        11.8    4.31 2.68   1.40  2.44  
    ## 3 Colombia         20.4   12.8  1.14   0.288 1.14  
    ## 4 Turks & Caicos   28.8   28.1  0.0830 0.498 0.0875
    ## 5 Maine, US        18.1    7.41 2.93   1.90  4.41

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
    ## 1 Chile                  62.8       14.8 
    ## 2 Argentina              46.9        6.53
    ## 3 Colombia               67.9       10.3 
    ## 4 Turks & Caicos         74.2        1.49
    ## 5 Maine, US              55.1        6.60

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

![](C:\\Users\\autum\\DOCUME~1\\ST558~1\\Project1\\README~1/figure-gfm/bar%20graph-1.png)<!-- -->

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

![](C:\\Users\\autum\\DOCUME~1\\ST558~1\\Project1\\README~1/figure-gfm/maxtemp%20boxplot-1.png)<!-- -->

``` r
min.means <- weather %>% group_by(location) %>% summarise(average = mean(mintemp))

g1 <- ggplot(weather, aes(x = location, y = mintemp))

g1 + geom_boxplot(fill = "grey") + geom_point(min.means, mapping = aes(x = location, y = average), color = "green") + geom_line(min.means, mapping = aes(x = location, y = average, group = 1), color = "green") + labs(title = "8 Day Forecast: Minimum Daily Temperature", x = "Location (highest to lowest latitude)", y = "Minimum Daily Temperature (C)") + coord_flip()
```

![](C:\\Users\\autum\\DOCUME~1\\ST558~1\\Project1\\README~1/figure-gfm/mintemp%20boxplot-1.png)<!-- -->

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

![](C:\\Users\\autum\\DOCUME~1\\ST558~1\\Project1\\README~1/figure-gfm/humidity%20hist%20and%20density%20plot-1.png)<!-- -->

``` r
hum.means <- weather %>% group_by(location) %>% summarise(average = mean(humidity))

g3 <- ggplot(weather, aes(x = location, y = humidity))

g3 + geom_boxplot(fill = "grey") + geom_point(hum.means, mapping = aes(x = location, y = average), color = "red") + geom_line(hum.means, mapping = aes(x = location, y = average, group = 1), color = "red") + labs(title = "8 Day Forecast: Average Daily Humidity by Location", x = "Location (highest to lowest latitude)", y = "Average Daily Humidity (C)") + coord_flip()
```

![](C:\\Users\\autum\\DOCUME~1\\ST558~1\\Project1\\README~1/figure-gfm/humidity%20boxplot-1.png)<!-- -->

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
    ## 1       Maine, US 2021    10   4  399.28 1093.52      247    1069       0.93
    ## 2       Maine, US 2021    10   5  400.48 1091.70      322    1092       0.97
    ## 3       Maine, US 2021    10   6  401.68 1089.88      398    1116       0.00
    ## 4       Maine, US 2021    10   7  402.88 1088.08      476    1141       0.05
    ## 5       Maine, US 2021    10   8  404.10 1086.30      555    1171       0.08
    ## 6       Maine, US 2021    10   9  405.32 1084.52      636    1205       0.12
    ## 7       Maine, US 2021    10  10  406.53 1082.75      716    1248       0.16
    ## 8       Maine, US 2021    10  11  407.77 1081.00      789    1302       0.20
    ## 9  Turks & Caicos 2021    10   4  396.28 1109.38      282    1056       0.93
    ## 10 Turks & Caicos 2021    10   5  396.58 1108.48      341    1095       0.97
    ## 11 Turks & Caicos 2021    10   6  396.87 1107.57      400    1134       0.00
    ## 12 Turks & Caicos 2021    10   7  397.18 1106.67      462    1175       0.05
    ## 13 Turks & Caicos 2021    10   8  397.48 1105.78      525    1219       0.08
    ## 14 Turks & Caicos 2021    10   9  397.80 1104.92      591    1269       0.12
    ## 15 Turks & Caicos 2021    10  10  398.13 1104.03      658    1323       0.16
    ## 16 Turks & Caicos 2021    10  11  398.45 1103.18      723    1382       0.20
    ## 17       Colombia 2021    10   4  396.75 1118.68      299    1053       0.93
    ## 18       Colombia 2021    10   5  396.63 1118.17      351    1098       0.97
    ## 19       Colombia 2021    10   6  396.53 1117.67      404    1144       0.00
    ## 20       Colombia 2021    10   7  396.43 1117.17      458    1192       0.05
    ## 21       Colombia 2021    10   8  396.35 1116.68      514    1243       0.08
    ## 22       Colombia 2021    10   9  396.27 1116.20      574    1298       0.12
    ## 23       Colombia 2021    10  10  396.20 1115.72      635    1356       0.16
    ## 24       Colombia 2021    10  11  396.13 1115.27      699    1418       0.20
    ## 25      Argentina 2021    10   4  357.07 1133.02      349     992       0.93
    ## 26      Argentina 2021    10   5  354.80 1134.65      366    1074       0.97
    ## 27      Argentina 2021    10   6  352.53 1136.32      383    1159       0.00
    ## 28      Argentina 2021    10   7  350.28 1137.97      400    1246       0.04
    ## 29      Argentina 2021    10   8  348.03 1139.63      419    1335       0.08
    ## 30      Argentina 2021    10   9  345.80 1141.32      442    1140       0.12
    ## 31      Argentina 2021    10  10  343.57 1143.00      474    1423       0.16
    ## 32      Argentina 2021    10  11  341.35 1144.68      517      67       0.19
    ## 33          Chile 2021    10   4  345.03 1128.42      349     976       0.93
    ## 34          Chile 2021    10   5  342.47 1130.35      361    1064       0.97
    ## 35          Chile 2021    10   6  339.92 1132.28      373    1154       0.00
    ## 36          Chile 2021    10   7  337.38 1134.23      385    1247       0.04
    ## 37          Chile 2021    10   8  334.85 1136.18      400    1341       0.08
    ## 38          Chile 2021    10   9  332.33 1138.15      418    1140       0.12
    ## 39          Chile 2021    10  10  329.82 1140.12      444    1436       0.16
    ## 40          Chile 2021    10  11  327.30 1142.10      482      83       0.19
    ##    pressure humidity dew_point wind_speed wind_deg wind_gust clouds  pop
    ## 1      1021       50      5.14       2.52       36      8.33     83 0.29
    ## 2      1025       56      7.05       1.63       22      2.84    100 0.07
    ## 3      1027       49      8.53       2.04      356      3.30      3 0.00
    ## 4      1023       53     10.34       1.72      286      2.72      0 0.00
    ## 5      1021       54      9.74       3.31      357      7.93      6 0.00
    ## 6      1033       52      3.09       2.84      147      5.98      6 0.00
    ## 7      1033       57      4.64       3.26      183      5.54    100 0.00
    ## 8      1025       70     10.92       4.45      210     13.61     90 0.00
    ## 9      1014       74     23.78      10.19      123     11.06    100 1.00
    ## 10     1015       72     23.19      11.24      100     12.18     96 0.96
    ## 11     1016       72     23.21      11.30      101     12.27     20 0.58
    ## 12     1016       75     23.68       9.80       96     10.61     12 0.60
    ## 13     1015       75     23.64       7.17      101      7.55      2 0.40
    ## 14     1014       75     23.93       8.30      117      8.66     18 1.00
    ## 15     1014       75     23.89       9.21      117      9.63     97 0.71
    ## 16     1014       76     23.78       8.69      122      9.06    100 0.85
    ## 17     1013       58     13.69       2.96      103      4.12     89 0.77
    ## 18     1014       80     16.16       2.56      102      3.97     95 0.97
    ## 19     1016       83     15.60       2.65      119      4.62    100 0.92
    ## 20     1014       53     12.58       3.14      102      4.99     77 0.85
    ## 21     1015       65     13.92       2.26      108      3.27    100 1.00
    ## 22     1015       64     14.01       1.84       93      2.45     97 0.97
    ## 23     1015       67     15.04       1.71       80      2.44     79 1.00
    ## 24     1015       73     15.72       1.97      104      2.97    100 1.00
    ## 25     1007       41     -3.51      14.83      249     23.00     58 0.00
    ## 26     1021       43     -4.53      13.24      226     18.51     35 0.00
    ## 27     1011       41     -4.16      12.92      239     19.07    100 0.02
    ## 28     1014       54      1.49      10.69      249     16.84     38 0.00
    ## 29     1004       48      1.21      11.13      229     13.73    100 0.17
    ## 30     1001       49      3.74      13.54      319     20.16    100 0.00
    ## 31     1009       58      0.06      13.70      246     21.13     44 0.78
    ## 32     1011       41     -3.60      10.15      249     18.55     65 0.00
    ## 33      997       66     -0.99      12.76      258     23.26     45 0.75
    ## 34     1017       66     -1.29      10.45      228     17.19     65 0.84
    ## 35     1001       61      1.44      10.27      245     17.37     99 0.84
    ## 36     1009       47     -3.03       8.89      241     16.62     54 0.26
    ## 37      997       79      3.23       7.50      338     15.03     97 0.44
    ## 38      997       86      2.95      10.54      326     20.98    100 0.56
    ## 39     1000       44     -4.03      10.43      291     18.43     75 0.31
    ## 40     1001       53     -2.29      11.14      336     20.05     22 0.50
    ##    mintemp maxtemp humidity.status cloud.coverage
    ## 1     5.47   17.14             Low           High
    ## 2     8.28   17.15             Low           High
    ## 3     7.71   20.90             Low            Low
    ## 4     9.41   21.69             Low            Low
    ## 5     9.83   21.58             Low            Low
    ## 6     5.68   14.04             Low            Low
    ## 7     4.72   15.20             Low           High
    ## 8     8.19   17.46          Medium           High
    ## 9    26.97   28.87          Medium           High
    ## 10   28.41   28.78          Medium           High
    ## 11   28.20   28.73          Medium            Low
    ## 12   28.36   28.65          Medium            Low
    ## 13   28.16   28.64          Medium            Low
    ## 14   28.00   28.77          Medium            Low
    ## 15   28.62   28.85          Medium           High
    ## 16   28.07   28.74          Medium           High
    ## 17   12.85   21.43             Low           High
    ## 18   12.85   19.54          Medium           High
    ## 19   12.88   19.37            High           High
    ## 20   12.76   22.68             Low           High
    ## 21   12.38   19.99          Medium           High
    ## 22   13.33   19.94          Medium           High
    ## 23   12.92   20.52          Medium           High
    ## 24   12.49   19.63          Medium           High
    ## 25    3.34   10.00             Low         Medium
    ## 26    1.85    9.09             Low         Medium
    ## 27    4.22   12.12             Low           High
    ## 28    4.45   10.37             Low         Medium
    ## 29    5.87   14.51             Low           High
    ## 30    6.37   17.03             Low           High
    ## 31    4.35   10.89             Low         Medium
    ## 32    4.06   10.42             Low         Medium
    ## 33    2.11    5.49          Medium         Medium
    ## 34    1.99    6.22          Medium         Medium
    ## 35    1.53    9.19          Medium           High
    ## 36    3.95    8.65             Low         Medium
    ## 37    4.77    7.70          Medium           High
    ## 38    2.54   10.86            High           High
    ## 39    3.96    9.06             Low         Medium
    ## 40    2.43    8.02             Low            Low

The scatterplot below allows us to look at how sunrise time changes over
the course of 8 days in each location.

``` r
g4 <- ggplot(weather, aes(x = Day, y = Sunrise))

g4 + geom_point(aes(color = location)) + scale_color_discrete(name = "Location") + labs(title = "8 Day Forecast for Sunrise Time by Location", x = "Day of the Month", y = "Sunrise Time (minutes after 12am)")
```

![](C:\\Users\\autum\\DOCUME~1\\ST558~1\\Project1\\README~1/figure-gfm/sunrise%20scatterplot-1.png)<!-- -->

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

![](C:\\Users\\autum\\DOCUME~1\\ST558~1\\Project1\\README~1/figure-gfm/maxtemp%20vs%20pressure%20scatterplot-1.png)<!-- -->

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
