
################################################################################
### R code to simulate data for serial mediation (Normal Distribution Data)
################################################################################

# set sample size  
n=1000
# set seed   
set.seed(123)

# simulate x (normal distribution), namely IV 
X <- rnorm(n, 5, 4)

# simulate residual for M_1   
residual_1<-rnorm(n,0,1)
# simulate M_1     
M_1<-0.3+0.5*X+residual_1

# simulate residual for M_2  
# Note that, residuals for M_1 and M_2 should be different.
# Otherwise, there will be problem of singularities. 
residual_2<-rnorm(n,0,1.1)
# simulate M_2      
M_2<-0.1+0.7*X+0.5*M_1+residual_2

# simulate residual for Y  
residual_3<-rnorm(n,0,1.2)
# simulate Y     
Y<-0.5+0.8*X+0.6*M_1+0.9*M_2+residual_3

# combine into a dataframe and print out the first 6 rows         
data_normal <- data.frame(X=X, M_1=M_1,M_2=M_2, Y=Y)
head(data_normal)

write.csv(data_normal,"data_normal_serial_mediation.csv", row.names = FALSE)




##############################################################
### R code for serial mediation for normally distributed Y
##############################################################

library(boot)

Serial_Mediation_normal_distribution<-function(data_used,i,x_predetermined=0)
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
  result_b<-lm(Y~M_1+M_2+X, data = data_temp)
  b_0<-result_b$coefficients[1]
  b_1<-result_b$coefficients[2]
  b_2<-result_b$coefficients[3]
  c_1_apostrophe<-result_b$coefficients[4]
  
  
  #calculating the indirect effect  
  indirect_effect<-a_1*d_2*b_2
  return(indirect_effect)
}


# use boot() to do bootstrapping mediation analysis     
boot_mediation <- boot(data_normal, Serial_Mediation_normal_distribution, R=1000)
boot_mediation

# print out confidence intervals    
boot.ci(boot.out = boot_mediation, type = c("norm", "perc"))


