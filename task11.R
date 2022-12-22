library(ggplot2)

setwd("C:\\Users\\Evgeniy\\Desktop\\study\\R\\lab-3")
df <- read.csv(file = "data\\data_tula.csv", sep=";")

make_charts <- function(df) {
  chart1 <- ggplot(data = df, mapping = aes(x = Dominant_breed, y = Value_indicator, fill = Dominant_breed))
  chart1 <- chart1 + xlab("Породы") 
  chart1 <- chart1 + ylab("Тыс. га")
  chart1 <- chart1 + ggtitle('Площадь земель, занятых лесными насаждениями') 
  chart1 <- chart1 + geom_col() 
  chart1 <- chart1 + coord_flip()
  chart1 <- chart1 + geom_text(aes(label = Value_indicator), vjust = 0.5)
  chart1 <- chart1 + guides(fill = guide_legend(title = "Породы", override.aes = aes(label = "")))
  print(chart1)
  
  chart2 <- ggplot(data = df, mapping = aes(x = '', y = Value_indicator, fill = Dominant_breed))
  chart2 <- chart2 + ylab('Тыс. га')
  chart2 <- chart2 + ggtitle('Площадь земель, занятых лесными насаждениями') 
  chart2 <- chart2 + geom_col() 
  chart2 <- chart2 + coord_polar(theta = 'y')
  chart2 <- chart2 + guides(fill = guide_legend(title = "Породы", override.aes = aes(label = "")))
  print(chart2)
}

colnames(df) <- c("Dominant_breed", "Name_indicator", "Measure", "Value_indicator")
df <- subset(df, Name_indicator == "Площадь земель, занятых лесными насаждениями (покрытых лесной растительностью), всего")
df[[4]] <- sub(',', '.', df[[4]])
df[[4]] <- as.numeric(df[[4]])

make_charts(df)
