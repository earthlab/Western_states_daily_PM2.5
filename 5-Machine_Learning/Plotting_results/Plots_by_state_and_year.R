plot_with_colors<- function(Preds){ #preds must have preds, obs, State, Date
  resids<- Preds$preds - Preds$PM2.5_Obs
  region<- c()
  for(i in 1:length(resids)){
    if(Preds[i, "State"] %in% c("california", "nevada")){
      region<- append(region, 1)
    }else if(Preds[i, "State"] %in% c("colorado", "utah", "new mexico",
                                      "arizona")){
      region<- append(region, 2)
    }else if(Preds[i, "State"] %in% c("washington", "oregon")){
      region<- append(region, 3)
    }else if(Preds[i, "State"] %in% c("idaho", "montana", "wyoming")){
      region<- append(region, 4)
    }
  }
  plot(Preds$Date, resids, col = region, xlab = "Date", ylab = "Residuals (Predicted Minus Observed)",
       main = "Residuals Over Time, Colored by State")
}

# GAM<- readRDS("GAM1.rds")
# GLM0<- readRDS("Ens_GLM_ranger-xgbt_std.rds")

model<- GLM0
test_preds<- data.frame(predict(model$models, test[,-(which(names(test)=="PM2.5_Obs"))]))
Preds<- cbind(test_preds[,1], test[,c("PM2.5_Obs", "Date", "State")])
names(Preds)[1]<- "preds"
plot_with_colors(Preds)

