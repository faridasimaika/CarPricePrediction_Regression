library(olsrr)
library(fpc)

df<-read.csv("CarPrice.csv", header=TRUE)

#we discovered that the category "other fuel system" are two typos spfi instead of spdi and mfi instead of mpfi
unique(df$fuelsystem)

which(df=="spfi",arr.ind=TRUE) #47,18
which(df == "mfi", arr.ind=TRUE) #30,18

#correcting the typos
df$fuelsystem[which(df$fuelsystem=="spfi")]<-"spdi"
df$fuelsystem[which(df$fuelsystem=="mfi")]<-"mpfi"

#dropping the carId and carName Columns
df<-df[,-c(1,3)]

#converting the categorical variables into factor type
df$fueltype<-as.factor(df$fueltype)
df$aspiration<-as.factor(df$aspiration)
df$doornumber<-as.factor(df$doornumber)
df$carbody<-as.factor(df$carbody)
df$drivewheel<-as.factor(df$drivewheel)
df$enginelocation<-as.factor(df$enginelocation)
df$enginetype<-as.factor(df$enginetype)
#df$cylindernumber<-as.factor(df$cylindernumber)
df$fuelsystem<-as.factor(df$fuelsystem)

#converting the entries of the cylinder number column in numberes
library(dplyr)
df<-df%>%
  mutate(cylindernumber=case_when(
    cylindernumber=="eight" ~ 8,
    cylindernumber=="twelve" ~ 12,
    cylindernumber=="five" ~ 5,
    cylindernumber=="four" ~ 4,
    cylindernumber=="two" ~ 2,
    cylindernumber=="three" ~ 3,
    cylindernumber=="six"~6
  ))

df<-df%>%
  mutate(doornumber=case_when(
    doornumber=="two" ~ 2,
    doornumber=="four" ~ 4,
  ))

#creating dummy variables for for each categorical variable in the dataset
df[sapply(df,is.factor)]<-data.matrix(df[sapply(df,is.factor)])

#Graphs before fitting
boxplot(df$price,pch=19,xlab="Price",main="Boxplot of the Response Variable Price")

#subsetting the columns that contain quantitative continous variables
df_cont=df[,c(24,8,9,10,11,12,15,17,18,19,20,21,22,23)]
pairs(df_cont,pch=19)

#Iteration-1
reg1=lm(df$price~.,data=df)
summary(reg1)

n=nrow(df)        # Sample size
model=reg1$call    # Fitted model
betahat=reg1$coef  # Estimated regression coefficients
yhat=reg1$fitted   # Fitted values
yhat=fitted(reg1)  # Fitted values
e=reg1$resid       # Ordinary residuals
IStudRes=rstandard(reg1) # Internally studentized residuals
EStudRes=rstudent(reg1)  # Externally Studentized Residuals
p=reg1$rank-1      # Number of predictors
d.f=reg1$df      # Residusla degrees of freedom = n - p - 1

op <- par(mfrow = c(2,2)) 
hist(IStudRes,xlab="IStudRes")
# hist(EStudRes,xlab="EStudRes")
qqnorm(IStudRes, ylab = "IStudRes",pch=19)
qqline(EStudRes, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7)
# qqnorm(EStudRes, ylab = "EStudRes",pch=19)
#  qqline(IStudRes, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7)
plot(rstandard(reg1),pch=19,xlab="Index",ylab="IStudRes")       # Index plot of internally studentized Residuals
# plot(rstudent(reg),pch=19,xlab="Index",ylab="EStudRes")      # Index plot of internally studentized Residuals
plot(reg1$fitted,IStudRes,pch=19,xlab="Fitted Values",ylab="IStudres")
par(op)

op <- par(mfrow = c(2,2)) 
plot(cooks.distance(reg1),pch=19,xlab="Index",ylab="Cook's Distance") # Index plot of Cook's distance
plot(ols_leverage(reg1),pch=19,xlab="Index",ylab="Leverage Values")
Hinf=ols_hadi(reg1)        # Computes Hadi's Influence
plot(Hinf$hadi,pch=19,ylab="Hadi's Influence")  # Index plotof Hadi's Influence
#ols_plot_hadi(reg)       # Index plot of Hadi's Influence
plot(Hinf$res,Hinf$p,pch=19,xlab="Resiuals",ylab="Potential")  # PR Plot
title("Potentia-Residual Plot")
# ols_plot_resid_pot(reg) # Potential residual Plot
par(op)


#converting the categorical variables into binary variables
df_new=cat2bin(df,categorical=c(2,3,5,6,7,13,16))$data
df_new=as.data.frame(df_new)
names(df_new)=c("symboling","diesel","gas","std","turbo","doornumber","convertible","hardtop","hatchback","sedan","wagon","fourwd","fwd","rwd","front","rear","wheelbase","carlength","carwidth","carheight","curbweight","dohc","dohcv","l","ohc","ohcf","ohcv","rotor","cylindernumber","enginesize","1bbl","twobbl","fourbbl","idi","mpfi","spdi","boreratio","stroke","compressionratio","horsepower","peakrpm","citympg","highwaympg","price")

#dropping a categroy from each group to avoid collinearity
df_new<-df_new[,-c(3,4,11,14,16,28,36)]

#The data is ready for the 2nd regression iteration:
reg2=lm(df_new$price~.,data=df_new)
summary(reg2)

#message: 1 not defined because of singularities, meaning that 2 or more predictor variables
#have an exact linear relationship


#Checking Collinearity
cor(df_new)
cor(df_new$diesel,df_new$idi)
#After examining the correlation matrix of all variables it seems that there is a perfect linear relationship 
#between cars that have fuel type diesel and engine type idi.
plot(df_new$diesel,df_new$idi,pch=19)

#droping the column with the fuel system equal idi
df_new=df_new[,-28]

#Multiple Regression after dropping the idi column Iteration 2
reg2=lm(df_new$price~.,data=df_new)
summary(reg2)

#Graphs after fitting
#Validating the assumptions
n=nrow(df_new)        # Sample size
model=reg2$call    # Fitted model
betahat=reg2$coef  # Estimated regression coefficients
yhat=reg2$fitted   # Fitted values
yhat=fitted(reg2)  # Fitted values
e=reg2$resid       # Ordinary residuals
IStudRes=rstandard(reg2) # Internally studentized residuals
EStudRes=rstudent(reg2)  # Externally Studentized Residuals
p=reg2$rank-1      # Number of predictors
d.f=reg2$df_new      # Residusla degrees of freedom = n - p - 1

op <- par(mfrow = c(2,2)) 
hist(IStudRes,xlab="IStudRes")
# hist(EStudRes,xlab="EStudRes")
qqnorm(IStudRes, ylab = "IStudRes",pch=19)
qqline(EStudRes, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7)
# qqnorm(EStudRes, ylab = "EStudRes",pch=19)
#  qqline(IStudRes, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7)
plot(rstandard(reg2),pch=19,xlab="Index",ylab="IStudRes")       # Index plot of internally studentized Residuals
# plot(rstudent(reg),pch=19,xlab="Index",ylab="EStudRes")      # Index plot of internally studentized Residuals
plot(reg2$fitted,IStudRes,pch=19,xlab="Fitted Values",ylab="IStudres")
par(op)

op <- par(mfrow = c(2,2)) 
plot(cooks.distance(reg2),pch=19,xlab="Index",ylab="Cook's Distance") # Index plot of Cook's distance
plot(ols_leverage(reg2),pch=19,xlab="Index",ylab="Leverage Values")
Hinf=ols_hadi(reg2)        # Computes Hadi's Influence
plot(Hinf$hadi,pch=19,ylab="Hadi's Influence")  # Index plotof Hadi's Influence
#ols_plot_hadi(reg)       # Index plot of Hadi's Influence
plot(Hinf$res,Hinf$p,pch=19,xlab="Resiuals",ylab="Potential")  # PR Plot
title("Potentia-Residual Plot")
# ols_plot_resid_pot(reg) # Potential residual Plot
par(op)


df_new=df_new[,c(36,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35)]
for(j in 1:p){  # Assumes that Y is the first column of df
  plot(df_new[,j+1],IStudRes,pch=19,xlab = names(df_new)[j+1],main=paste("Plot of IStudRes v.",names(df_new)[j+1]),ylab = "IStudRes")
}

#Transformation: Y^1 the best symmetric dist of residuals around 0: model is an accurate representation of data
#Assumption 1: validated

# Ladder of transformation
lambda=c(-2,-1.5,-1,-0.5,0, 0.5,1,1.5,2)

#Power Transformation of Price
power.transf2=function(df_new,lambda){
  op <- par(mfrow = c(3,3))  # 3 x 3 matrix of plots
  for(i in 1:9){
    df1=df_new
    df1$price=df_new[,1]^lambda[i] 
    if(lambda[i]==0) {df1$price=log(df_new[,1])}
    reg=lm(df_new$price ~.,data=df1)
    IStudRes=rstandard(reg)  # Internally studentized residuals
    plot(df1[,1],IStudRes,pch=19,main=paste("lambda =",lambda[i]),
         xlab="Price",ylab = "IStudRes")
  }
  par(op)
}

power.transf2(df_new,lambda);

df_transformed=df_new
df_transformed$price=log(df_transformed$price)


reg3=lm(df_transformed$price ~.,data=df_transformed)
IStudRes=rstandard(reg3)
summary(reg3)
plot(reg3$fitted.values,IStudRes,pch=19,xlab="fitted values",main="IStudRes vs. Fitted Values")

#Power Transformation of compressionratio
power.transf2=function(df_transformed,lambda){
  op <- par(mfrow = c(3,3))  # 3 x 3 matrix of plots
  for(i in 1:9){
    df1=df_transformed
    df1$compressionratio=df_transformed[,32]^lambda[i] 
    if(lambda[i]==0) {df1$compressionratio=log(df_transformed[,32])}
    reg=lm(df_transformed$price ~.,data=df1)
    IStudRes=rstandard(reg)  # Internally studentized residuals
    plot(df1[,32],IStudRes,pch=19,main=paste("lambda =",lambda[i]),
         xlab="compressionratio",ylab = "IStudRes")
  }
  par(op)
}

power.transf2(df_transformed,lambda);

df_transformed1=df_transformed
df_transformed1$compressionratio=(df_transformed$compressionratio)^(-2)

reg=lm(df_transformed1$price~.,data=df_transformed1)
summary(reg)

n=nrow(df_transformed)        # Sample size
model=reg3$call    # Fitted model
betahat=reg3$coef  # Estimated regression coefficients
yhat=reg3$fitted   # Fitted values
yhat=fitted(reg3)  # Fitted values
e=reg3$resid       # Ordinary residuals
IStudRes=rstandard(reg3) # Internally studentized residuals
EStudRes=rstudent(reg3)  # Externally Studentized Residuals
p=reg3$rank-1      # Number of predictors
d.f=reg3$df_transformed       # Residusla degrees of freedom = n - p - 1

op <- par(mfrow = c(2,2)) 
hist(IStudRes,xlab="IStudRes")
# hist(EStudRes,xlab="EStudRes")
qqnorm(IStudRes, ylab = "IStudRes",pch=19)
qqline(EStudRes, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7)
# qqnorm(EStudRes, ylab = "EStudRes",pch=19)
#  qqline(IStudRes, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7)
plot(rstandard(reg3),pch=19,xlab="Index",ylab="IStudRes")       # Index plot of internally studentized Residuals
# plot(rstudent(reg),pch=19,xlab="Index",ylab="EStudRes")      # Index plot of internally studentized Residuals
plot(reg3$fitted,IStudRes,pch=19,xlab="Fitted Values",ylab="IStudres")
par(op)

op <- par(mfrow = c(2,2)) 
plot(cooks.distance(reg3),pch=19,xlab="Index",ylab="Cook's Distance") # Index plot of Cook's distance
plot(ols_leverage(reg3),pch=19,xlab="Index",ylab="Leverage Values")
Hinf=ols_hadi(reg3)        # Computes Hadi's Influence
plot(Hinf$hadi,pch=19,ylab="Hadi's Influence")  # Index plotof Hadi's Influence
#ols_plot_hadi(reg)       # Index plot of Hadi's Influence
plot(Hinf$res,Hinf$p,pch=19,xlab="Resiuals",ylab="Potential")  # PR Plot
title("Potentia-Residual Plot")
# ols_plot_resid_pot(reg) # Potential residual Plot
par(op)

#Interaction Variables:
Interaction1=df_transformed$compressionratio*df_transformed$horsepower
df_interaction=cbind(df_transformed,Interaction1)


reg7=lm(df_interaction$price~.,data=df_interaction)
summary(reg7)

plot(df_interaction$Interaction1,rstandard(reg7),pch=19,xlab="Interaction",ylab="IStudRes", main="IStudRes vs.Interaction")

#Detection of outliers
#Assumption 10: Detection of Outliers

plot(cooks.distance(reg3),pch=19,xlab="Index",ylab="Cook's Distance")
identify(cooks.distance(reg3))
plot(Hinf$hadi,pch=19,ylab="Hadi's Influence") 
identify(Hinf$hadi)
plot(Hinf$res,Hinf$p,pch=19,xlab="Resiuals",ylab="Potential")  # PR Plot
title("Potentia-Residual Plot")
identify(Hinf$res,Hinf$p)
df_nooutliers=df_transformed[-c(19,50,135),]

reg4=lm(df_nooutliers$price~.,data=df_nooutliers)
summary(reg4)

n=nrow(df_nooutliers)        # Sample size
model=reg4$call    # Fitted model
betahat=reg4$coef  # Estimated regression coefficients
yhat=reg4$fitted   # Fitted values
yhat=fitted(reg4)  # Fitted values
e=reg4$resid       # Ordinary residuals
IStudRes=rstandard(reg4) # Internally studentized residuals
EStudRes=rstudent(reg4)  # Externally Studentized Residuals
p=reg4$rank-1      # Number of predictors
d.f=reg4$df_nooutliers        # Residusla degrees of freedom = n - p - 1

op <- par(mfrow = c(2,2)) 
hist(IStudRes,xlab="IStudRes")
# hist(EStudRes,xlab="EStudRes")
qqnorm(IStudRes, ylab = "IStudRes",pch=19)
qqline(EStudRes, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7)
# qqnorm(EStudRes, ylab = "EStudRes",pch=19)
#  qqline(IStudRes, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7)
plot(rstandard(reg4),pch=19,xlab="Index",ylab="IStudRes")       # Index plot of internally studentized Residuals
# plot(rstudent(reg),pch=19,xlab="Index",ylab="EStudRes")      # Index plot of internally studentized Residuals
plot(reg4$fitted,IStudRes,pch=19,xlab="Fitted Values",ylab="IStudres")
par(op)

op <- par(mfrow = c(2,2)) 
plot(cooks.distance(reg4),pch=19,xlab="Index",ylab="Cook's Distance") # Index plot of Cook's distance
plot(ols_leverage(reg4),pch=19,xlab="Index",ylab="Leverage Values")
Hinf=ols_hadi(reg4)        # Computes Hadi's Influence
plot(Hinf$hadi,pch=19,ylab="Hadi's Influence")  # Index plotof Hadi's Influence
#ols_plot_hadi(reg)       # Index plot of Hadi's Influence
plot(Hinf$res,Hinf$p,pch=19,xlab="Resiuals",ylab="Potential")  # PR Plot
title("Potentia-Residual Plot")
# ols_plot_resid_pot(reg) # Potential residual Plot
par(op)

#Multicollinearity
df_transformed=scale(df_transformed)  # Standerdize the data. scale changes df to matrix
df_transformed=as.data.frame(df_transformed)  

library(car)
vif(reg3)

X=df_transformed[,-1];   X=as.matrix(X);   R=cor(X)
e=eigen(R)
L=e$val; V=e$vec
kappa=sqrt(L[1]/L)
kappa

W=X%*%V  # Principal components
reg5=lm(df_transformed$price~ W)
summary(reg5)
alpha=reg5$coef

alpha=as.matrix(alpha)

alpha=alpha[-1,]


V%*%alpha 
beta_pc=V[,-c(7,8,9,12,14,15,18,21,22,24,25,27,29,31,32,33,34)]%*%alpha[-c(7,8,9,12,14,15,18,21,22,24,25,27,29,31,32,33,34)] # PC Regression Coefficients


y_hat_pc=X%*%beta_pc       # PC Regression Fitted Values

plot(y_hat_pc,df_transformed$price,pch=19,xlab="yhat principal components",ylab="Price",main="Price vs. Fitted Values - PCR") 

#Multicollinearity without the outliers
df_nooutliers=scale(df_nooutliers)  
df_nooutliers=as.data.frame(df_nooutliers) 
View(df_nooutliers)
X=df_nooutliers[,-1];   X=as.matrix(X);   R=cor(X)
e=eigen(R)
L=e$val; V=e$vec
kappa=sqrt(L[1]/L)
kappa

W=X%*%V  # Principal components
reg6=lm(df_nooutliers$price~ W)
summary(reg6)
alpha=reg6$coef

alpha=as.matrix(alpha)

alpha=alpha[-1,]


V%*%alpha 
beta_pc=V[,-c(4,8,9,15,21,23,24,25,26,28,29,31,32,33,35)]%*%alpha[-c(4,8,9,15,21,23,24,25,26,28,29,31,32,33,35)] # PC Regression Coefficients


y_hat_pc=X%*%beta_pc       # PC Regression Fitted Values

plot(y_hat_pc,df_nooutliers$price,pch=19,xlab="yhat principal components",ylab="Price",main="Price vs. Fitted Values - PCR") 
