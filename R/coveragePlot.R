#' @export
coveragePlot <- function(data, begin_year, end_year, id_column, begin_year_column,
                         end_year_column, main_title) {

  # x <- getWeatherStations()
  # data <- x; id_column = "Station_Code"; begin_year_column = "Begin_Year"; end_year_column = "End_Year"; main_title ="Weather Stations"
  if(missing(begin_year)){
    begin_year <- min(as.numeric(data[[begin_year_column]]), na.rm = TRUE)
  }
  if(missing(end_year)){
    end_year <- max(as.numeric(data[[end_year_column]]), na.rm = TRUE)
  }


  plot_data <- select_(data, id_column, begin_year_column, end_year_column)
  names(plot_data) <- c("id", "begin", "end")
  plot_data <- plot_data %>%
    mutate(begin = as.numeric(begin)) %>%
    mutate(end = as.numeric(end) - begin) %>%
    melt(id.vars = "id") %>%
    mutate(value = as.numeric(as.character(value)))

  na_monitors <- unique(filter(plot_data, is.na(value))[["id"]])
  if(length(na_moitors) > 0){
    print("Removing these monitors because they have NA begin year or end year:",
          na_monitors)
    plot_data <- filter(plot_data, !id %in% na_monitors)
  }

  ggp <- ggplot(plot_data, aes(x = id, y = value, fill = variable)) +
    geom_bar(stat = "identity") + coord_flip(ylim = c(begin_year, end_year)) +
    scale_fill_manual(values = c(alpha("blue", 0), "blue")) +
    theme(legend.position="none") + ggtitle(main_title) + ylab("Year") +
    xlab("Monitor ID")

  plot(ggp)

}
