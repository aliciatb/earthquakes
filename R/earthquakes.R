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
#' @importFrom dplyr  %>% filter select
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

  # final cleanup: drop helper fields
  earthquakes <- earthquakes %>%
    dplyr::select(-YEAR_FULL,-MONTH_FULL,-DAY_FULL,-DATE_CHAR)

  earthquakes
}

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