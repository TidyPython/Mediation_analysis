##########################################
###  data simulation
##########################################

# set the size of the sample    
n=1000
# set seed      
set.seed(123)

# simulate x (normal distribution)   
X <- rnorm(n, 5, 4)
# calculate the mean of X   
mean_x=mean(X)

# simulate a residual for M   
residual_1<-rnorm(n,0,1)
M_1<-0.3+0.5*X+residual_1

residual_2<-rnorm(n,0,1.1)
M_2<-0.1+0.7*X+0.5*M_1+residual_2

# mu for Poisson regression via a log link   
mu_1 <- exp(0.2 + 0.3*M_1+0.3*M_2+0.01*X)
# use rpois to generate Y  
Y <- rpois(n, lambda=mu_1)

# combine into a dataframe and print out the first 6 rows  
data <- data.frame(X=X, M_1=M_1,M_2=M_2, Y=Y)
head(data)


##################################################
###  Serial Mediation Analysis for Count data
##################################################

library(boot)

Serial_Mediation_poisson<-function(data_used,i,x_predetermined=0)
{
  # Sample a data   
  data_temp=data_used[i,]
  
  # Deciding which X value to use   
  if(x_predetermined==0){x_predetermined=mean(data_temp$X)}
  else if (x_predetermined==-1){x_predetermined=mean(data_temp$X)-sd(data_temp$X)}
  else(x_predetermined=mean(data_temp$X)+sd(data_temp$X))
  
  # a path      
  result_a<-lm(M_1~X, data = data_temp)
  a_0<-result_a$coefficients[1]
  a_1<-result_a$coefficients[2]
  
  
  # d path    
  result_d<-lm(M_2~X+M_1, data = data_temp)
  d_0<-result_d$coefficients[1]
  d_1<-result_d$coefficients[2]
  d_2<-result_d$coefficients[3]
  
  # b path    
  result_b<-glm(Y~M_1+M_2+X, data = data_temp, family = quasipoisson)
  b_0<-result_b$coefficients[1]
  b_1<-result_b$coefficients[2]
  b_2<-result_b$coefficients[3]
  c_1_apostrophe<-result_b$coefficients[4]

  
  #calculating the indirect effect  
  M_1_estimated=a_0+a_1*x_predetermined
  M_2_estimated=d_0+d_1*x_predetermined+d_2*M_1_estimated
  indirect_effect<-a_1*d_2*b_2*exp(b_0+b_1*M_1_estimated+b_2*M_2_estimated+c_1_apostrophe*x_predetermined)
  return(indirect_effect)
}


# use boot() to do bootstrapping mediation analysis     
boot_mediation <- boot(data, Serial_Mediation_poisson, R=1000)
boot_mediation

# plot the 1000 indirect effects     
plot(boot_mediation)

# print out confidence intervals     
boot.ci(boot.out = boot_mediation, type = c("norm", "perc"))

##################################################
###  calculate the theoretical value 
##################################################

x_mean=mean(data$X)
M_1=0.3+0.5*x_mean
M_2=0.1+0.7*x_mean+0.5*M_1
0.5*0.5*0.3*exp(0.2 + 0.3*M_1+0.3*M_2+0.01*x_mean)


##################################################
###  calculate the original indirect value
##################################################
# a path      
result_a<-lm(M_1~X, data = data)
a_0<-result_a$coefficients[1]
a_1<-result_a$coefficients[2]
a_0
a_1

# d path    
result_d<-lm(M_2~X+M_1, data = data)
d_0<-result_d$coefficients[1]
d_1<-result_d$coefficients[2]
d_2<-result_d$coefficients[3]
d_0
d_1
d_2

# b path    
result_b<-glm(Y~M_1+M_2+X, data = data, family = quasipoisson)
b_0<-result_b$coefficients[1]
b_1<-result_b$coefficients[2]
b_2<-result_b$coefficients[3]
c_1_apostrophe<-result_b$coefficients[4]
b_0
b_1
b_2
c_1_apostrophe

#calculating the indirect effect  
M_1_estimated=a_0+a_1*x_mean
M_2_estimated=d_0+d_1*x_mean+d_2*M_1_estimated
indirect_effect<-a_1*d_2*b_2*exp(b_0+b_1*M_1_estimated+b_2*M_2_estimated+c_1_apostrophe*x_mean)
indirect_effect

