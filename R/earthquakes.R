#' Print "Earthquakes Clean Data"
#'
#' This function reads a raw dataset of significant earthquake data provide by NOAA (National Centers for Environmental Information).
#'
#' @param raw_data The raw data loaded into R environment after reading in from https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1
#'
#' @return This function returns a tibble with a date field, numeric latitude and longitude values and a city name.
#'
#' @note Earthquakes that occurred BCE (Before Current Epoch) have been removed from the dataset because negative year values are not handled well.
#'
#' @examples
#' \dontrun{
#' eq_clean_data(raw)
#' }
#'
#' @importFrom readr parse_date parse_double
#' @importFrom dplyr %>% filter select
#' @importFrom stringr str_pad
#'
#' @export
eq_clean_data <- function(raw_data){
  earthquakes <- raw_data

  # step 1. create a date field

  # handle NA days and months
  earthquakes[is.na(earthquakes$MONTH),]$MONTH = 1
  # NA days
  earthquakes[is.na(earthquakes$DAY),]$DAY = 1

  # filter out BCE dates for now, which have negative year values
  earthquakes <- earthquakes %>%
    dplyr::filter(YEAR > 0)

  # pad all date parts before converting to YMD
  earthquakes$YEAR_FULL <- stringr::str_pad(earthquakes$YEAR,4,"left","0")
  earthquakes$MONTH_FULL <- stringr::str_pad(earthquakes$MONTH,2,"left","0")
  earthquakes$DAY_FULL <- stringr::str_pad(earthquakes$DAY,2,"left","0")
  earthquakes$DATE_CHAR <- paste0(earthquakes$YEAR_FULL,"-",earthquakes$MONTH_FULL,"-",earthquakes$DAY_FULL)

  # parse as YMD date
  earthquakes$DATE <- readr::parse_date(earthquakes$DATE_CHAR,format = "%Y-%m-%d")

  # step 2. convert LAT/LNG as numbers(doubles)
  earthquakes$LATITUDE <- parse_double(earthquakes$LATITUDE)
  earthquakes$LONGITUDE <- parse_double(earthquakes$LONGITUDE)

  # step 3. strip out country name and colon and title case the city
  earthquakes$LOCATION_CITY <- eq_location_clean(earthquakes$COUNTRY,earthquakes$LOCATION_NAME)

  # step 4. set values to correct format to support visualizations - better to set this when reading in data file
  earthquakes$EQ_PRIMARY <- parse_double(earthquakes$EQ_PRIMARY)
  earthquakes$EQ_MAG_MB <- parse_double(earthquakes$EQ_MAG_MB)
  earthquakes$EQ_MAG_MFA <- parse_double(earthquakes$EQ_MAG_MFA)
  earthquakes$EQ_MAG_ML <- parse_double(earthquakes$EQ_MAG_ML)
  earthquakes$EQ_MAG_MS <- parse_double(earthquakes$EQ_MAG_MS)
  earthquakes$EQ_MAG_MW <- parse_double(earthquakes$EQ_MAG_MW)
  earthquakes$EQ_MAG_UNK <- parse_double(earthquakes$EQ_MAG_UNK)
  earthquakes$DEATHS <- parse_integer(earthquakes$DEATHS)
  earthquakes$MISSING <- parse_integer(earthquakes$MISSING)
  earthquakes$INJURIES <- parse_integer(earthquakes$INJURIES)
  earthquakes$DAMAGE_MILLIONS_DOLLARS <- parse_double(earthquakes$DAMAGE_MILLIONS_DOLLARS)
  earthquakes$HOUSES_DAMAGED <- parse_integer(earthquakes$HOUSES_DAMAGED)
  earthquakes$HOUSES_DESTROYED <- parse_integer(earthquakes$HOUSES_DESTROYED)

  # step 5. drop nas

  # final cleanup: drop helper fields
  earthquakes <- earthquakes %>%
    dplyr::select(-YEAR_FULL,-MONTH_FULL,-DAY_FULL,-DATE_CHAR)

  earthquakes
}
# test_data <- eq_clean_data(raw)
# summary(test_data)

#' Print "Earthquakes City Location"
#'
#' This function reads in the country and full name for an earthquake as 2 string fields like ITALY: POMPEI
#'
#' @param country The name of the country.
#' @param full_name The full name of the country and city.
#'
#' @return This function returns a city name without the country name and returned in Title Case format.
#'
#' @note Some earthquakes have impacted multiple city locations and in these instances, country name is currently returned with all subsequent cities.
#'
#' @examples
#' \dontrun{
#' eq_location_clean(country, full_name)
#' }
#' @importFrom stringr str_replace str_to_title
#'
#' @export
eq_location_clean <- function(country, full_name){
  location_city <- str_replace(full_name, paste0(country,":"),"")
  location_city <- trimws(location_city, "both")
  location_city <- str_to_title(location_city)
  location_city
}

#' Print "Earthquakes Timeline Visualization"
#'
#' This function reads a clean dataset of significant earthquake data provide by NOAA (National Centers for Environmental Information).
#'
#' @param data The data has been cleaned using eq_clean_data function
#' @param start_date The first date to return data for viz
#' @param end_date The last date to return data for viz
#' @param country Provide country name like ITALY, NEW ZEALAND
#'
#' @return This function returns a ggplot timeline visualization with DATE on the x-axis, earthquake count on the y-axis where size of point indicates magnitude(EQ_MAG_ML) and color signifies Number of Deaths due to earthquake
#'
#' @note Earthquakes that occurred BCE (Before Current Epoch) have been removed from the dataset because negative year values are not handled well.
#'
#' @examples
#' \dontrun{
#' geom_timeline(data, "2000-01-01", "2018-01-01", "NEW ZEALAND")
#' }
#'
#' @importFrom ggplot2 ggplot
#' @importFrom readr parse_integer
#' @importFrom dplyr %>% filter
#' @importFrom stringr str_to_upper
#'
#' @export
geom_timeline <- function(data, start_date, end_date, country){
  # ensure that country name is upper case
  country = stringr::str_to_upper(country)
  sub <- data %>%
    dplyr::filter(DATE >= start_date) %>%
    dplyr::filter(DATE <= end_date) %>%
    dplyr::filter(COUNTRY == country)

  g <- ggplot2::ggplot(data = sub, ggplot2::aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, color = DEATHS, alpha = .5)) +
    ggplot2::geom_point() +
    ggplot2::xlab("Date") +
    ggplot2::ylab("")
  g
}
# test_g <- geom_timeline(test_data, "2000-01-01", "2018-01-01", "NEW ZEALAND")
# test_g

#' Print "Earthquakes Timeline Visualization with Label"
#'
#' This function reads a clean dataset of significant earthquake data provide by NOAA (National Centers for Environmental Information).
#'
#' @param data The data has been cleaned using eq_clean_data function
#' @param start_date The first date to return data for viz
#' @param end_date The last date to return data for viz
#' @param country Provide country name like ITALY, NEW ZEALAND
#' @param max_magnitude Max Magnitude
#'
#' @return This function returns a ggplot timeline visualization with DATE on the x-axis, earthquake count on the y-axis where size of point indicates magnitude(EQ_MAG_ML) and color signifies Number of Deaths due to earthquake
#'
#' @note Earthquakes that occurred BCE (Before Current Epoch) have been removed from the dataset because negative year values are not handled well.
#'
#' @examples
#' \dontrun{
#' geom_timeline_label(data, "2000-01-01", "2018-01-01", "NEW ZEALAND", 5)
#' }
#'
#' @importFrom ggplot2 ggplot
#' @importFrom readr parse_integer
#' @importFrom dplyr %>% filter
#' @importFrom stringr str_to_upper
#'
#' @export
geom_timeline_label <- function(data, start_date, end_date, country, max_magnitude){
# ensure that country name is upper case
country = stringr::str_to_upper(country)
sub <- data %>%
  dplyr::filter(DATE >= start_date) %>%
  dplyr::filter(DATE <= end_date) %>%
  dplyr::filter(COUNTRY == country)

# convert DEATHS to number
sub$DEATHS <- parse_integer(sub$DEATHS)

g <- ggplot2::ggplot(data = sub, ggplot2::aes(x = DATE, y = COUNTRY, label = LOCATION_CITY, size = EQ_PRIMARY, color = DEATHS, alpha = .5)) +
  ggplot2::geom_point() +
  ggplot2::geom_text(angle = 45) +
  ggplot2::xlab("Date") +
  ggplot2::ylab("")
g
}

# test_gt <- geom_timeline_label(test_data, "2000-01-01", "2018-01-01", "NEW ZEALAND", 5)
# test_gt
