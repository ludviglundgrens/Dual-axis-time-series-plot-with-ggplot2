dplot <- function(df1, date1, df2, date2) {
  k <- df1[!is.na(df1)]
  l <- df2[!is.na(df2)]
  
  fact <- (max(k)-min(k))/(max(l)-min(l))
  
  mean_1 <- mean(k)
  mean_2 <- mean(l)
  diff <- mean_1-mean_2
  
  ggplot2::ggplot()+
    geom_line(aes(x = date, y = ((df1-mean_1)/fact)+mean_1-diff))+
    geom_line(aes(x = date, y = df2), col = "red")+
    scale_y_continuous(sec.axis = sec_axis(~(.+diff-mean_1)*fact+mean_1))+
    labs(x = "xlabs", y = "ylabs")
}