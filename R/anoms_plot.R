anoms_plot <- function(){

  plot_title <- substr(file_path_sans_ext(basename(file_path)), 34, 39)

  d_plot <- ggplot(data = dfdl, aes(x=country, y=pop_frac, fill = rp)) +

    geom_bar(stat="identity")+

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
                                 "#FDF0B1",
                                 "#F7C029",
                                 "#FA9A3A",
                                 "#CC3300",
                                 "#921C40"),
                      labels = c("   normal",
                                 " 3 abnormal",
                                 " 5 moderate",
                                 "10 severe",
                                 "20 extreme",
                                 "40 exceptional")
    )

  s_plot <- ggplot(data = dfsl, aes(x=country, y=pop_frac, fill = rp)) +

    geom_bar(stat="identity")+

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
                                 "#CCFF99",
                                 "#00CC99",
                                 "#66CCCC",
                                 "#3399CC",
                                 "#000099"),
                      labels = c("   normal",
                                 " 3 abnormal",
                                 " 5 moderate",
                                 "10 severe",
                                 "20 extreme",
                                 "40 exceptional")
    )

  grid.arrange(d_plot, s_plot, ncol = 2)
}
