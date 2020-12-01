dplot <- function(df1, date1, df2, date2, name1 = "Series 1", name2 = "Series 2", xlab = "xlab", ylab = "ylab", ylab2 = "ylab", breaks = "2 years") {
  cols <- c("#004DBA", "#6BA3FF")
  k <- df1[!is.na(df1)]
  l <- df2[!is.na(df2)]
  
  date1 <- as.Date(date1) 
  date2 <- as.Date(date2)
  
  fact <- (max(k)-min(k))/(max(l)-min(l))
  
  mean_1 <- mean(k)
  mean_2 <- mean(l)
  diff <- mean_1-mean_2
  
  ggplot2::ggplot()+
    geom_line(aes(x = date1, y = ((df1-mean_1)/fact)+mean_1-diff, color = name1))+
    geom_line(aes(x = date2, y = df2, color = name2))+
    scale_y_continuous(sec.axis = sec_axis(~(.+diff-mean_1)*fact+mean_1, name = "hej"))+
    labs(x = xlab, y = ylab)+
    theme_light()+
    scale_colour_manual(values=cols)+
    theme(legend.position="bottom", legend.box = "horizontal")+
    theme(legend.title=element_blank())+
    theme(panel.grid.major.x = element_blank())+
    theme(panel.grid.minor.x = element_blank())+
    scale_x_date(breaks = breaks, date_labels = "%Y")
}
