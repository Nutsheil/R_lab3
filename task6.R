library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd("C:\\Users\\Evgeniy\\Desktop\\study\\R\\lab-3")
load("data\\ExpImp.RData")

make_charts <- function(df, region, flag) {
  df <- df[complete.cases(df),]
  for (i in 2:length(names(df))) {
    df[[i]] <- gsub("-", 0, df[[i]])
    df[[i]] <- as.numeric(df[[i]])
  }
  
  filter1 <- str_detect(df$Регион, 'федеральный округ')
  df <- mutate(df, Округ = if_else(filter1, Регион, NULL))
  df <- fill(df, Округ)
  filter2 <- !str_detect(df$Регион, 'Федерация|федеральный округ')
  df <- filter(df, filter2)
  
  export <- select_at(df, vars(matches("Экспорт")))
  import <- select_at(df, vars(matches("Импорт")))
  
  export$Сумма <- rowSums(export, na.rm = TRUE)
  import$Сумма <- rowSums(import, na.rm = TRUE)
  
  df$Export <- export$Сумма
  df$Import <- import$Сумма
  
  if (flag == TRUE) {
    df[,"Import"] <- -df[,"Import"]
  }
  
  df <- filter(df, Округ == region)
  df <- df[, c("Регион", "Export", "Import")]
  df <- pivot_longer(df, !Регион, names_to = "Export/Export", values_to = "Value")
  
  
  result <- df %>% group_by(Регион, `Export/Export`) 
  result <- result %>% summarise(sum = sum(`Value`))
  
  result |> ggplot(mapping = aes(x = Регион, y = sum, fill = `Export/Export`)) +
            geom_col(color = 'black', size = 0, position = 'dodge') + 
            ggtitle(region) + 
            ylab('млн долл. США') + 
            coord_flip()
}

make_charts(ExpImp, 'Северо-Западный федеральный округ', FALSE)
make_charts(ExpImp, 'Северо-Западный федеральный округ', TRUE)
