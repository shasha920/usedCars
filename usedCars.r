useCars_df<-read.csv("Data-UsedCars.csv")
#Adjust categorical variables
#Met_Color set "no" as baseline
table(useCars_df$Met_Color)
class(useCars_df$Met_Color)
levels(useCars_df$Met_Color)
levels.order<-c("no","yes")
useCars_df$Met_Color<-factor(useCars_df$Met_Color,levels.order)
levels(useCars_df$Met_Color)
#Fuel_Type set "Petrol" as baseline
table(useCars_df$Fuel_Type)
class(useCars_df$Fuel_Type)
levels(useCars_df$Fuel_Type)
levels.order<-c("Petrol","CNG","Diesel")
useCars_df$Fuel_Type<-factor(useCars_df$Fuel_Type,levels.order)
levels(useCars_df$Fuel_Type)
#Build multiple linear regression
useCars_lm<-lm(Price~Age_08_04+Met_Color+Weight+HP+KM+Quarterly_Tax+Fuel_Type,
               data=useCars_df)
useCars_lm_summary<-summary(useCars_lm)
print(useCars_lm_summary)
#Compute VIF of all independent variables
vif(useCars_lm)
#Residual Analysis
useCars_dv_est<-useCars_lm$fitted.values
useCars_res_std<-rstudent(useCars_lm)
lb<-min(-3,-max(abs(useCars_res_std)))
ub<-max(3,max(abs(useCars_res_std)))
plot(useCars_res_std~useCars_dv_est,pch=19,xlab="Fitted Value of DV",
     ylab="Standardized Residual",main="Residual Analysis",ylim=c(lb,ub))
abline(h=-1.5,lty=3)
abline(h=1.5,lty=3)