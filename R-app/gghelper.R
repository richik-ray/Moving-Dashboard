library(ggplot2)

getEdu <- function(df, counties, indic) {
  switch(indic,
    "Education" = df <- subset(df, df$Type == "GradRate"),
    "Crime Rate" = df <- subset(df, df$Type == "Crime"),
    "Political" =  df <- subset(df, df$Type == "Political"),
    "Climate" = df <- subset(df, df$Type == "Temp"),
    "Housing Cost" = df <- subset(df, df$Type == "HousPrices"))
  df <- subset(df, counties == countstate)
  return(df$Value)
}

rightSt2 <- function(str){
  substr(str, nchar(str)-1, nchar(str))
}


makeLine <- function(housingdatabase, county, boolean){
  df <- subset(housingdatabase, countstate == county)
  integ <- df$col
  for (i in integ){
    housingdatabase$label[i] = "one"
  }
  if (boolean) {
    housingdatabase <- subset(housingdatabase, state == rightSt2(county))
  }
ggplot(housingdatabase, aes(x = Type, y = Value, group = countstate)) +
  geom_line(data = subset(housingdatabase, label == "zero"), color = "black", alpha = 0.05, size = 1) +
  geom_line(data = subset(housingdatabase, label == "one"), color = "blue", alpha = 1, size = 3) +
  #scale_color_manual(values = c("one" = "blue", "zero" = "black")) +
  #scale_alpha_manual(values = c("one" = 1, "zero" = 0.05))+
  #scale_size_manual(values = c("one" = 3, "zero" = 1)) +
  theme(panel.background = element_blank(),
      panel.grid = element_line(color = "gray75"),
      panel.spacing = margin(6,6,6,6, "mm"),
      plot.margin = margin(6,6,6,6, "mm"),
      axis.title.y = element_text(size=18, color = "black"),
      axis.title.x = element_text(size=18, color = "black"),
      legend.position = "none") +
    labs(x = "Date",
      y = "Cost (In Dollars)")
}
