plotCorr <- function(df, xvar, yvar, col){
  library(ggplot2)
  
  df <- as.data.frame(df)
  t1<-theme(                              
    axis.title.x = element_text(face="bold", color="black", size=10),
    axis.title.y = element_text(face="bold", color="black", size=10),
    plot.title = element_text(face="bold", color = "black", size=12)
  )
  df[,xvar] <- as.numeric(df[,xvar])
  df[,yvar] <- as.numeric(df[,yvar])
  df[,col] <- as.numeric(df[,col])
  
  cor_p <- ggplot(df, aes(x = df[,xvar], y = df[,yvar])) +
    geom_point(size=2, aes(color = abs(df[,col]))) +
    geom_hline(yintercept = 0, color="black") +
    scale_color_continuous(name=col,   
                           breaks = with(df, c(min(abs(df[,col])), median(abs(df[,col])), max(abs(df[,col])))),
                           labels = c("min", "median", "max"),
                           low = "pink",                           
                           high = "red") +
    geom_smooth() + 
    theme_bw() + t1 + labs(x=xvar, y = yvar, title= paste0(xvar, ' vs ', yvar))
  
  return(cor_p)
}
