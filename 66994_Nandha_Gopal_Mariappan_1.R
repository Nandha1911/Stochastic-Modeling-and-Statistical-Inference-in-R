##Stochastic Methods for Materials Science -- Assignment Task 1



# Question - 1
#Given data :
A = c(76.8,72.3,74.0,73.2,46.1,76.5,61.9,62.4,65.9,62.4)
B = c(64.4,60.0,59.4,61.2,52.0,58.1,62.0,57.8,57.2)

#Q1:a) 
# Box plot
boxplot(A,B,names=c("Location A","Location B"))
title(main = "Cd Contamination of Trout in a river", xlab = "X label", ylab = "Y label")
summary(A)
summary(B)
#For location A,the boxplot is wide and has a great difference between upper and lower whiskers
#For location B,the boxplot is narrow and has a small difference between upper and lower whiskers

#Q1:b)
# Normality test - considered Bonferroni correction alpha = 0.025

#Null-hypothesis -> The distribution of data(from Location A) follow the normal Distribution
#Alternative Hypothesis -> The distribution of data(from Location A) will not follow the 
#normal Distribution
shapiro.test(A)

#Since the p-value > 0.025 for samples collected from location A ,we can confirm that
#the data from location A follows the Normal Distribution.

#Null-hypothesis -> The distribution of data (from Location B) follow the normal Distribution
#Alternative Hypothesis -> The distribution of data(from Location B) will not follow the 
#normal Distribution
shapiro.test(B)


#Since the p-value > 0.025 for samples collected from location B ,we can confirm that
#the data from location B follows the Normal Distribution.
#Q1:c)
# F-test -> variances test
# Null-hypothesis -> var of A is equal to var of B
# Alternative-hypothesis -> var of A is not equal to var of B
var.test(A,B)
#Since the p < 0.05,we reject the Null hypothesis and accept the alternative hypothesis which is
#Var of A is not equal to Var of B

#Q1:d)
# t-test -> test of 'mean'
# Null-hypothesis -> mean of A is equal to mean of B
# Alternative-hypothesis -> mean of A is greater than mean of B
t.test(A,B,alternative="greater")
#Since the p < 0.05,we reject the Null hypothesis and accept the alternative hypothesis which is
#mean of A is greater than mean of B


#--------------------------------------------------------------------------------------------------------

# Question - 2
#Given data :
before = c(38.4,39.6,39.4,40.1,39.2,38.5,39.3,39.1,38.4,39.5)
after = c(37.6,37.9,39.1,39.4,38.6,38.9,38.7,38.7,38.9,38.7)

#Q2:a)
# we have "paired sample" here since the "before" and "after" drug effect temperature values 
# are obtained from the 10 different patients.

#Q2:b)
plot(before,after)
#After analyzing the scatterplot,we can able to see that
#there is no correlation in the paired sample.

#Q2:c)
cor(before,after)
#After calculating the correlation for the paired sample,we get a value of 0.3486
#which helps to conclude that there exists a weak positive correlation in the paired
#sample.


#Q2:d)
#Null-hypothesis -> The distribution of data(patients "before" eating tablet) follow the normal Distribution
#Alternative Hypothesis -> The distribution of data(patients "after" eating tablet) will not follow the 
#normal Distribution
shapiro.test(before)
#Since p-value > 0.05,it follows the Normal Distribution(Accepting Null hypothesis).

#Null-hypothesis -> The distribution of data(patients "before" eating tablet) follow the normal Distribution
#Alternative Hypothesis -> The distribution of data(patients "after" eating tablet) will not follow the 
#normal Distribution
shapiro.test(after)
#Since p-value > 0.05,it follows the Normal Distribution(Accepting Null hypothesis).

#Correlation test and suitable method:-

#In order to do correlation test,three methods("pearson","Kendall","Spearman") are there 
#and we are going to use "pearson" method(parametric measure of correlation) since the   
#paired sample follows linear relationship  and Both "before" and "after" data follow 
#Normal Distribution.

#"Kendall" and "Spearman" will be used for Non-parametric measure of correlation


# Null-hypothesis -> There is no correlation between those Samples (Before and After)
# Alternative-hypothesis -> There exists a correlation between those samples(Before and After)

cor.test(before,after,alternative="two.sided",method=c("pearson"))

#After using pearson method for correlation test,p-value is greater than 0.05 and thus proves that 
#there exists no correlation in the paired sample.


#-----------------------------------------------------------------------------------------------------


# Question - 3
#Q3:a)
coal_data = scan("coal_data.txt")          #"coal data" variable reads the value from 
                                           #coal_data.txt file

hist(coal_data,nclass=40, freq = 0 )       #For generating histogram from the "coal_data"

lines(density(coal_data),col="red",lwd=3)  #For generating KDE curve

#After analyzing the Kernel Density Estimation (KDE) curve,it appears to follow 
#positively skewed (unimodal) distribution.

#Q3:b)
#To find the confidence interval for the mean values of days between two disasters
mean_hat = mean(coal_data)   #mean of the data
std_hat = sd(coal_data)      #Standard Deviation of the data

n <- length(coal_data)            #Number of data
alpha=0.05                        #Confidence level 95%

Del_hat = std_hat/sqrt(n)*qt(1-(0.5*alpha),n-1)

Theta_low= mean_hat-Del_hat    #Lower confidence interval value
Theta_high= mean_hat+Del_hat   #Upper confidence interval value

print(Theta_high) #Upper confidence interval value = 258.2948
print(Theta_low)  #Lower confidence interval value = 168.5473

#Theta low and Theta_high gives us the lower and upper confidence interval values for the 
#mean value of days respectively

#--------------------------------------------------------------------------------------------------





