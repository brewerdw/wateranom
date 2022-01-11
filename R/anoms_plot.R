#' Make deficit and surplus plots
#'
#' @param file_path A file path that points to a .csv file
#' @param pix_size Minimum number of country pixels
#' @param n_coun  Number of countries that will display on a plot
#'
#' @return A water deficit plot and a surplus plot
#' @export
#'
#' @examples
#' #' file_path <- "~/population_water_anomaly_summary_201801.csv"
#' pix_size <- 10
#' n_coun <- 10
library(ggplot2)
anoms_plot <- function(file_path, pix_size=10, n_coun=10){

  df_list <- wateranom::anoms_makedf(file_path, pix_size, n_coun) # create data frames

  dfdl <- df_list[[1]] # deficit data frame
  dfsl <- df_list[[2]] # surplus data frame

  plot_title <- substr(basename(file_path), 34, 39) # YYYYMM from csv file name

  d_plot <- ggplot2::ggplot(data = dfdl, ggplot2::aes(x=country, y=pop_frac, fill = rp)) +

    ggplot2::geom_bar(stat="identity")+

    scale_y_continuous(breaks = seq(0,1,0.25), labels = c(0,'25%','50%','75%','100%')) +

    theme(axis.text.x = element_text(angle = 90, hjust=0.2, vjust=0.2, size = 10),
          panel.background = element_rect(fill = "#EFEFEF", colour = "black", size = .1, linetype = "solid"),
          panel.grid.major = element_line(size = 0, linetype = 'solid', color = NA),
          panel.grid.minor = element_line(size = 0, linetype = 'solid', color = NA),
          plot.background = element_rect(fill = "NA"),
          legend.position = "right",
          legend.title = element_text(size=10),
          legend.key.size = unit(1, 'cm'),
          legend.key = element_rect(color = "#000000", fill = NA)
    ) +

    labs(title="\nPopulations Exposed to Water Deficit",
         subtitle=paste0("Countries with significant exceptional return periods in ", plot_title),
         y = "Percent of Country Population",
         x= ""
    ) +

    guides(color = guide_legend(title.position = "top",
                                title.hjust = 0.25,
                                label.position = "",
                                nrow = 12),
           fill = guide_legend(title.position = "top",
                               title.hjust = 0,
                               label.position = "right",
                               nrow = 12)
    ) +

    scale_fill_manual(name = "Return Period (years)\n",
                      breaks = c("< 3",
                                 "3-5",
                                 "5-10",
                                 "10-20",
                                 "20-40",
                                 "> 40"),
                      values = c("#FFFFFF",
                                 "#FFEDA3",
                                 "#FFC754",
                                 "#FF8D43",
                                 "#D44135",
                                 "#9B0039"),
                      labels = c("   normal",
                                 " 3 abnormal",
                                 " 5 moderate",
                                 "10 severe",
                                 "20 extreme",
                                 "40 exceptional")
    )

  s_plot <- ggplot2::ggplot(data = dfsl, ggplot2::aes(x=country, y=pop_frac, fill = rp)) +

    ggplot2::geom_bar(stat="identity")+

    scale_y_continuous(breaks = seq(0,1,0.25), labels = c(0,'25%','50%','75%','100%')) +

    theme(axis.text.x = element_text(angle = 90, hjust=0.2, vjust=0.2, size = 10),
          panel.background = element_rect(fill = "#EFEFEF", colour = "black", size = .1, linetype = "solid"),
          panel.grid.major = element_line(size = 0, linetype = 'solid', color = NA),
          panel.grid.minor = element_line(size = 0, linetype = 'solid', color = NA),
          plot.background = element_rect(fill = "NA"),
          legend.position = "right",
          legend.title = element_text(size=10),
          legend.key.size = unit(1, 'cm'),
          legend.key = element_rect(color = "#000000", fill = NA)
    ) +

    labs(title="\nPopulations Exposed to Water Surplus",
         subtitle=paste0("Countries with significant exceptional return periods in ", plot_title),
         y = "Percent of Country Population",
         x= ""
    ) +

    guides(color = guide_legend(title.position = "top",
                                title.hjust = 0.25,
                                label.position = "",
                                nrow = 12),
           fill = guide_legend(title.position = "top",
                               title.hjust = 0,
                               label.position = "right",
                               nrow = 12)
    ) +

    scale_fill_manual(name = "Return Period (years)\n",
                      breaks = c("< 3",
                                 "3-5",
                                 "5-10",
                                 "10-20",
                                 "20-40",
                                 "> 40"),
                      values = c("#FFFFFF",
                                 "#CEFFAD",
                                 "#00F3B5",
                                 "#00CFCE",
                                 "#009ADE",
                                 "#0045B5"),
                      labels = c("   normal",
                                 " 3 abnormal",
                                 " 5 moderate",
                                 "10 severe",
                                 "20 extreme",
                                 "40 exceptional")
    )

  gridExtra::grid.arrange(d_plot, s_plot, ncol = 2)
}
