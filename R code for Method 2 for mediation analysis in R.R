# Method 2 for mediation analysis in R
# Mediation Analysis in R from Scratch 
# Youtube video: https://youtu.be/MIIF-ICF52Y

# read data
Mediation_data <- 
  read.csv('https://raw.githubusercontent.com/TidyPython/Mediation_analysis/main/mediation_hypothetical_data.csv')

# show the head of the data
head(Mediation_data)

library(boot)
set.seed(123)

# key function for indirect effect
Mediation_function<-function(data_used,i)
{
  # Sample a data 
  data_temp=data_used[i,]
  # a path 
  result_a<-lm(M~X, data = data_temp)
  a<-result_a$coefficients[2]
  
  # b path
  result_b<-lm(Y~M+X, data = data_temp)
  b<-result_b$coefficients[2]
  
  #calculating the indirect effect
  indirect_effect<-a*b
  return(indirect_effect)
}


# use boot() to do bootstrapping mediation analysis
boot_mediation <- boot(Mediation_data, Mediation_function, R=5000)
boot_mediation 

# print out confidence intervals for indirect effect
boot.ci(boot.out = boot_mediation, type = c("norm", "perc"))

# plot the 5000 indirect effects
plot(boot_mediation)

# print out top 6 indirect effects saved in "boot_mediation"
head(boot_mediation$t)

# calculate the standard deviation
sd(boot_mediation$t)
# print out the bootstrapping output again
boot_mediation

#The difference between the mean of the bootstrap estimates
# and the original sample estimate is the bias.
bias=mean(boot_mediation$t)-boot_mediation$t0
print(bias)

# calculate the confidence interval from scratch
print(boot_mediation$t0-bias + c(-1, 1) * 1.96 * sd(boot_mediation$t))
# Again, print out confidence interval based on normal distribution
boot.ci(boot.out = boot_mediation, type = c("norm"))
