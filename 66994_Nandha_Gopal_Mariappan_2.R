##########Stochastic Methods for Materials Science#############
############Assignment Task-II################################


########################################################################
#Question_1
 datatimber=read.table("timber.txt",header =T)           
  v1 <- datatimber[[1]]   #v1 = Rigid
  v2 <- datatimber[[2]]   #v2 = Elast
  v3 <- datatimber[[3]]   #v3 = Dens
 ##Part_a##########3
  model1 = lm(v1 ~ (v2+v3)) ##Rigid = θ0 + θ1 Elast + θ2 Dens
  model2 = lm(v2 ~ (v1+v3)) ##Elast = θ0 + θ1 Rigid + θ2 Dens
  model3 = lm(v3 ~ (v1+v2)) ##Dens = θ0 + θ1 Rigid + θ2 Elast
  summary(model1)
  summary(model2)
  summary(model3)
  #model1(Rigid = θ0 + θ1 Elast + θ2 Dens) has the highest linear relationship because 
  #the adjusted R squared value is larger when compared among others
  
###Part_b#################
  par(mfrow=c(2,2))
  plot (model1,col='blue')
  #According to the graph(residuals vs Leverage),there is one influential point(40th data point) 
  #which is found between the cook's distance of 0.5 and 1.Thus it deviates from one of the assumption
  #Therefore we remove the influential point from the dataset.
  
  #Now removing the 40th data from the table and rerun regression
  newdatatimber <- datatimber[-40,]
  v1 <- newdatatimber[[1]]
  v2 <- newdatatimber[[2]]
  v3 <- newdatatimber[[3]]
  model11 = lm(v1 ~ (v2+v3))
  summary(model11) 
  #model11 will give us the linear regression model which follows all the assumptions of 
  #Gaussian linear model

  
####Part_c##########3
  model4 = lm(v1 ~ v2+v3+(v2*v3)) #Interaction term have been added
  summary(model4)
  anova(model11,model4) #Using ANOVA test to check whether the p-value is below or above 0.05
  
  #N0 -> Small model is Sufficient N1 -> Small model is not sufficient
  
  #By running the F-test for model comparison between model11 and model4,we conclude that
  #the model11 is not sufficient because p-value is less than 0.05 
  #(i.e,we accept the alternate hypotheses N1);
  
######Part_d########################

  newdat <- data.frame(v1=2078,v2=237.5,v3=70.8)
  #A prediction interval includes a wide range of values than a confidence interval.
  #To calculate for current situation,prediction interval can be used to check whether the new data point 
  #fits the regressed model or not
 
  predict(model4,newdat,level = 1 - 0.05,interval = "prediction")
  #Yes,the new data point fits the regressed model by concluding from 
  #the predict function(interval='prediction') 

#--------------------------------------------------------------------------------------------------------------------  
#Question_2:-
  
#####Part_a###############:-
  mm_signal <- read.table("signal.txt",header = T)
  x <- mm_signal[[1]]
  L <- mm_signal[[2]]
  plot(x,L,pch=15)
  
  #In this Non-linear regression model,a and x0 are the parameters of this model
  #Initial values for a and x0 are given as 3 and 255 respectively
  
  mm_reg.lm <- nls(L ~ (a/(1+((x-x0)^2))),data = mm_signal,start=list(a=3,x0=255),trace=TRUE) 
  summary(mm_reg.lm)
  
  #Extracting the values of parameters from summary
  a=summary(mm_reg.lm)$coefficients[1,1]
  x0=summary(mm_reg.lm)$coefficients[2,1]
  print(a) ##Parameter->a
  
  print(x0) ##Parameter->x0
  
  #for the above parameters A and B,the t-test values are less than 0.05,which proves that the initial values
  #A and B are optimal
  
  cor(L,predict(mm_reg.lm)) #To check the correlation
  plot(x,L,main="Signal data") #Graphically output for signal curve and data points
  lines(x,predict(mm_reg.lm),lty=2,col='red',lwd=3) 

####Part_b##########################:-
  alpha=0.05
  n=length(L)
  Lc=3 #Critical value 
  std_error=summary(mm_reg.lm)$coefficients[1,2] 
  
  #'a' parameter will determine the maximum signal strength which is why we have used the 
  #'coefficient value A from summary(mm_reg.lm)
  
  Delta_hat= std_error * qt(1-alpha,n-2)
  Calpha_upper=Lc+Delta_hat
  print(Calpha_upper)
  
  #Calpha_upper=3.028305
  #To check whether the parameter (a) is greater than critical value(Lc):-
  #We are using confidence interval and hypotheses testing
  #Considering one-sided interval test
  #Null hypothesis(Ho) : Parameter 'a' is less than or equal to critical value(Lc)-->(-inf,3.028305)
  #Alternate Hypothesis(Ha) : Parameter 'a' is greater than critical value(Lc)-->(3.028305,inf)
  #Since the parameter 'a' value is 3.164 which lies in the region(3.028305,inf)
  #and it concludes that we accept the alternate hypothesis(Ha).
  
  #Thus the parameter 'a' is significantly greater than critical value(Lc)

##----------------------------------------------------------------------------------------------------------  
    
     
  