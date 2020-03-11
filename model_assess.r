
###STATS for modeling assessment
### Author: Erik Bolch

#RMSE, MAE, Pbias

model_assess <- function (mydata, obs, pred){ #Provide data tibble, column name of observed data, and column name of modeled or predicted dat
  ## example: model_assess(dataframe, "Col1", "Col2"
  
  obs <- select(mydata, obs)
  predname<-pred
  pred <- select(mydata, pred) 
  mae <- sum(abs(obs-pred))/nrow(obs)
  rmse <- sqrt(sum(pred-obs)^2/nrow(obs))
  pbias <- 100*(sum(pred-obs)/sum(obs))
  
  df <- data.frame(c("MAE", "RMSE", "Pbias"), c(mae, rmse, pbias))
  names(df)<- c("Statistic", paste0(predname)) #no idea how to paste a value into tibble column names
  df <- as_tibble(df)
  return(df)
  }















