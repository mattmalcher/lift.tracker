make_cal_data <- function(break_data, start_date = end_date - 365, end_date = Sys.Date()) {
  break_data <-
    dplyr::left_join(
      x = data.frame(when = seq.Date(from = start_date,to =  end_date, by = "day")),
      y = break_data,
      by = "when"
    ) %>%
    dplyr::group_by(when) %>%
    dplyr::mutate(
      badness = 5 * sum(category == "Break", na.rm = TRUE) + sum(category == "Scary Occurance", na.rm = TRUE)) %>%
    dplyr::ungroup()

  sugrrants::frame_calendar(
    data = break_data,
    x = 1,
    y = 1,
    date = when,
    calendar = "monthly"
  )
}

make_cal_plot <- function(cal_data) {

  p <-
    ggplot2::ggplot(data = cal_data,
                    ggplot2::aes(x = .x, y = .y)) +
    ggiraph::geom_tile_interactive(
      aes(fill = badness),
      colour = "grey50",
      tooltip = cal_data$what
    ) +
    viridis::scale_fill_viridis() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      text = ggplot2::element_text(family = "sans"),
      legend.position = "none"
    )

  p <- sugrrants::prettify(p)

  p <- ggiraph::girafe(ggobj = p)

  # p <- ggiraph::girafe_options(
  #   x = p ,
  #   htmlwidgets::sizingPolicy(defaultWidth = "100%", defaultHeight = "300px")
  #   )

  p
}
