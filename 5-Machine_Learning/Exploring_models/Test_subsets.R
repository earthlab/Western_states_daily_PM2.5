test_subsets<- function(model, test){
  test_preds <- data.frame(predict(model, test))
  # test_preds<- data.frame(predict(model$models, test[,-(which(names(test)=="PM2.5_Obs"))]))
  compare<- cbind(test_preds, test[,"PM2.5_Obs"])
  
  compare<- apply(compare, MARGIN = 2, 
                  function(y){y*stats$std["PM2.5_Obs"] + stats$mean["PM2.5_Obs"]})
  
  years<- 2008:2018
  states<- unique(test$State)
  
  for(y in years){
    print(y)
    data<- compare[which(test$Year == y),]
    resids<- (data[,1] - data[,2])
    print(paste("RMSE =", round(sqrt(mean(resids^2)), digits = 4)))
    print(paste("R^2 =", round(R2(pred = data[,1], obs = data[,2]), digits = 4)))
  }
  
  for(s in states){
    print(s)
    data<- compare[which(test$State == s),]
    resids<- (data[,1] - data[,2])
    print(paste("RMSE =", round(sqrt(mean(resids^2)), digits = 4)))
    print(paste("R^2 =", round(R2(pred = data[,1], obs = data[,2]), digits = 4)))
  }
}

