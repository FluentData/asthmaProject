#' Get data frame of weather stations in Indiana
#'
#' @export
getWeatherStations <- function(isd_history_file = "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv",
                               start_year = 1999, end_year = as.numeric(format(Sys.Date(), "%Y")),
                               by = c("state", "radius"), states = "IN"){
  # isd_history_file = "C:/Repositories/asthma_data/data/isd-history.csv"
  st <- read.csv(isd_history_file, stringsAsFactors = FALSE) %>%
    rename(NAME = STATION.NAME, ELEV = ELEV.M.) %>%
    mutate(WBAN = str_pad(WBAN, 5, "left", "0")) %>%
    mutate(Station_Code = paste(USAF, WBAN, sep = "-")) %>%
    mutate(Begin_Year = as.numeric(substr(BEGIN, 1, 4))) %>%
    mutate(End_Year = as.numeric(substr(END, 1, 4))) %>%
    filter(Begin_Year >= 1999)

  if(by[1] == "state"){
    st <- filter(st, CTRY == "US", STATE %in% states)
  }
  st

}
