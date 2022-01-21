#' Make water deficit and surplus plots
#'
#' @param file_path A file path that points to a .csv file
#' @param pix_size Minimum number of country pixels
#' @param n_coun  Number of countries that will display on a plot
#' @param coun_area Country land area in square kilometers
#' @param min_pop Country population
#'
#' @return A water deficit plot and a water surplus plot
#' @export
#'
#' @examples
#' #' file_path <- "~/population_water_anomaly_summary_201801.csv"
#' pix_size <- 10
#' n_coun <- 10
anoms_plot <- function(file_path, pix_size=10, coun_area = 50000, n_coun=10, min_pop = 500000){

df_list <- wateranom::anoms_makedf(file_path, pix_size, coun_area, n_coun, min_pop) # create data frames

dfdl <- df_list[[1]] # deficit long data frame
dfsl <- df_list[[2]] # surplus long data frame

  file_str <- (substr(basename(file_path), 34, 39)) # YYYYMM from csv file name
  file_date <- lubridate::parse_date_time(file_str, "%Y%m") # create a date from file_str
  plot_title <- format(file_date, format = "%B %Y") # create a date string for plot title

  # create the deficit plot
  d_plot <- ggplot2::ggplot(data = dfdl, ggplot2::aes(x=country, y=pop_frac, fill = rp)) +

    ggplot2::geom_bar(stat="identity")+

    ggplot2::coord_flip() +

    ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(as.character(x), width = 10)) +

    ggplot2::scale_y_continuous(breaks = seq(0,1,0.25), labels = c(0,'25%','50%','75%','100%')) +

    ggplot2::theme(plot.title = ggplot2::element_text(size=16),
                   plot.subtitle = ggplot2:: element_text(size=14),
                   plot.caption = ggplot2::element_text(size=11),
                   plot.caption.position = "panel",
                   axis.text.x = ggplot2::element_text(angle = 0, size = 15),
                   axis.text.y = ggplot2::element_text(angle = 0, size = 15),
                   panel.background = ggplot2::element_rect(fill = "#FFFFFF", colour = "black", size = 0.75, linetype = "solid"),
                   panel.grid.major = ggplot2::element_line(size = 0, linetype = 'solid', color = NA),
                   panel.grid.minor = ggplot2::element_line(size = 0, linetype = 'solid', color = NA),
                   plot.background = ggplot2::element_rect(fill = "NA"),
                   legend.position = "right",
                   legend.title = ggplot2::element_text(size= 13),
                   legend.key.size = ggplot2::unit(1.1, 'cm'),
                   legend.key = ggplot2::element_rect(color = "#000000", fill = NA, size = 0.75),
                   legend.text = ggplot2::element_text(vjust = -.3, size = 12)
                   ) +

    ggplot2::labs(title="ISciences Forecast:  Percent of Population Exposure to Water Deficit",
                  subtitle=paste0("Countries with significant exceptional return periods in ", plot_title),
                  y = "",
                  x= "",
                  caption = paste0("Top ", n_coun, " countries with an area of at least ", scales::comma(coun_area), " square kilometers and at least ", scales::comma(min_pop), " people")
                  ) +

    ggplot2::guides(fill = ggplot2::guide_legend(title.position = "top",
                               title.hjust = 0.5,
                               label.position = "right",
                               nrow = 5)
                   ) +

    ggplot2::scale_fill_manual(name = "return period (years)\nwater deficit\n",
                      breaks = c(#"< 3",
                                 "> 40",
                                 "20-40",
                                 "10-20",
                                 "5-10",
                                 "3-5"),
                      values = c(#"#FFFFFF",
                                 "#9B0039",
                                 "#D44135",
                                 "#FF8D43",
                                 "#FFC754",
                                 "#FFEDA3"),
                      labels = c(#"   normal",
                                 "      exceptional\n40",
                                 "      extreme\n20",
                                 "      severe\n10",
                                 "      moderate\n5",
                                 "      abnormal\n3")
                              )

  # create the surplus plot
  s_plot <- ggplot2::ggplot(data = dfsl, ggplot2::aes(x=country, y=pop_frac, fill = rp)) +

    ggplot2::geom_bar(stat="identity")+

    ggplot2::coord_flip() +

    ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(as.character(x), width = 10)) +

    ggplot2::scale_y_continuous(breaks = seq(0,1,0.25), labels = c(0,'25%','50%','75%','100%')) +

    ggplot2::theme(plot.title = ggplot2::element_text(size=16),
                   plot.subtitle = ggplot2:: element_text(size=14),
                   plot.caption = ggplot2::element_text(size=11),
                   plot.caption.position = "panel",
                   axis.text.x = ggplot2::element_text(angle = 0, size = 15),
                   axis.text.y = ggplot2::element_text(angle = 0, size = 15),
                   panel.background = ggplot2::element_rect(fill = "#FFFFFF", colour = "black", size = .75, linetype = "solid"),
                   panel.grid.major = ggplot2::element_line(size = 0, linetype = 'solid', color = NA),
                   panel.grid.minor = ggplot2::element_line(size = 0, linetype = 'solid', color = NA),
                   plot.background = ggplot2::element_rect(fill = "NA"),
                   legend.position = "right",
                   legend.title = ggplot2::element_text(size=13),
                   legend.key.size = ggplot2::unit(1.1, 'cm'),
                   legend.key = ggplot2::element_rect(color = "#000000", fill = NA, size = 0.75),
                   legend.text = ggplot2::element_text(vjust = -.3, size=12)
                   ) +

    ggplot2::labs(title="ISciences Forecast:  Percent of Population Exposure to Water Surplus",
                  subtitle=paste0("Countries with significant exceptional return periods in ", plot_title),
                  y = "",
                  x= "",
                  caption = paste0("Top ", n_coun, " countries with an area of at least ", scales::comma(coun_area), " square kilometers and at least ", scales::comma(min_pop), " people")
                  ) +

    ggplot2::guides(fill = ggplot2::guide_legend(title.position = "top",
                                        title.hjust = 0.5,
                                        label.position = "right")
                   ) +

    ggplot2::scale_fill_manual(name = "return period (years)\nwater surplus\n",
                      breaks = c(#"< 3",
                                 "> 40",
                                 "20-40",
                                 "10-20",
                                 "5-10",
                                 "3-5"),
                      values = c(#"#FFFFFF",
                                 "#0045B5",
                                 "#009ADE",
                                 "#00CFCE",
                                 "#00F3B5",
                                 "#CEFFAD"),
                      labels = c(#"   normal",
                                 "      exceptional\n40",
                                 "      extreme\n20",
                                 "      severe\n10",
                                 "      moderate\n5",
                                 "      abnormal\n3")
                              )

  # arrange plots into 1 row with same width plot area
  gd <- ggplot2::ggplotGrob(d_plot)
  gs <- ggplot2::ggplotGrob(s_plot)
  grid::grid.newpage()
  grid::grid.draw(cbind(gd, gs))
}
