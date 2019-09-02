## source functions for timer app
## Lizzie Pearmain, Sept 2019

makePie <- function(time.ori, time.left) {
  if (time.left>0 & time.ori>10){
    time.elapsed <- time.ori - time.left
    pie(c(time.elapsed, time.left), labels=NA, col=c("red","white"), clockwise=T)
  } else {
    pie(c(1,0), labels=NA, col=c("red","white"), clockwise=T)
  }
}

printTime <- function(seconds) {
  word <- sprintf("%02d:%02d", minute(seconds_to_period(seconds)), second(seconds_to_period(seconds)))
  df2 <- data.frame(timer = word)
  p <- ggplot(df2, aes(x=1, y=1, label=timer)) +  # (slice(df, i)
    geom_text(size=50, colour="black") + # family="Source Sans Pro Light", 
    theme_bw() + 
    theme(line = element_blank(), #rect = element_blank(),
          strip.text = element_blank(), axis.text = element_blank(),
          plot.title = element_blank(), axis.title = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), unit = "cm"),
          panel.background = element_rect(fill="white"),
          panel.border=element_blank())
  print(p)
}
