validate_input <- function(time_series, under_candles_layers) {
  assert_data_frame(time_series)
  if (!test_subset(c('Time', 'Open', 'High', 'Low', 'Close'),
                   colnames(time_series))) {
    stop("Time series must contain 'Time', 'Open', 'High', 'Low' and 'Close' columns.")
  }
  time <- time_series[, c('Time')]
  assert_posixct(time)
  assert_list(under_candles_layers)
}

timeframe <- function(time_series) {
  most_popular_diff <- NULL
  times <- time_series[, c('Time')]
  candles_time_diffs <- c()
  for (index in 2:length(times)) {
    time_1 <- times[index - 1]
    time_2 <- times[index]
    diff <- as.numeric(difftime(time_2, time_1, units = 'secs'))
    candles_time_diffs <- c(diff, candles_time_diffs)
    most_popular_diff <- as.numeric(names(sort(table(candles_time_diffs), decreasing = TRUE))[1])
  }

  return(most_popular_diff)
}

prettyCandlePlot <-
  function(time_series,
           chart_title = '',
           under_candles_layers = list()) {
    library(ggplot2)
    library(checkmate)

    validate_input(time_series, under_candles_layers)

    candle_duration <- timeframe(time_series)
    chart_data <- time_series
    chart_data$chg = ifelse(chart_data['Close'] > chart_data['Open'], "up", "dn")
    chart_data$flat_bar <- chart_data[, "High"] == chart_data[, "Low"]

    p <- ggplot(chart_data, aes(x = Time))
    p <- p + theme_bw()
    p <-
      p + theme(
        panel.background = element_rect(fill = 'black'),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#121212"),
        plot.margin = unit(c(10, 0, 0, 32), "pt"),
        plot.background = element_rect(fill = "black", colour = 'black'),
        plot.title = element_text(colour = "white"),
        axis.line = element_line(colour = "white"),
        axis.text.y = element_text(colour = 'white'),
        axis.text.x = element_text(
          colour = 'white',
          angle = 45,
          hjust = 1
        ),
        axis.ticks.x = element_line(colour = 'white'),
        axis.ticks.y = element_line(colour = 'white'),
        axis.ticks.length = unit(5, "pt")
      )
    p <- p + guides(fill = FALSE, colour = FALSE)
    p <- p + labs(title = chart_title, colour = 'white')
    for (layer in under_candles_layers) {
      p <- p + layer
    }
    p <-
      p + layer(
        geom = 'linerange',
        mapping = aes(ymin = Low, ymax = High),
        params = list(color = 'white'),
        stat = 'identity',
        position = 'identity'
      )
    p <- p + layer(
      geom = 'rect',
      mapping = aes(
        xmin = Time - candle_duration / 2 * 0.9,
        xmax = Time + candle_duration / 2 * 0.9,
        ymin = pmin(Open, Close),
        ymax = pmax(Open, Close),
        fill = chg
      ),
      stat = 'identity',
      position = 'identity'
    )
    p <-
      p + scale_fill_manual(values = c("dn" = "#656565", "up" = "#ededee"))
    p <-
      p + scale_x_datetime(breaks = scales::pretty_breaks(n = 20),
                           date_labels = "")
    p <-
      p + scale_y_continuous(position = 'right',
                             breaks = scales::pretty_breaks(n = 25))

    # Handle special case of drawing a flat bar where OHLC = Open:
    if (any(chart_data$flat_bar))
      p <- p + geom_segment(data = chart_data[chart_data$flat_bar, ],
                            aes(
                              x = Time - 1 / 2 * 0.9,
                              y = Close,
                              yend = Close,
                              xend = Time + 1 / 2 * 0.9
                            ))
    return(p)
  }