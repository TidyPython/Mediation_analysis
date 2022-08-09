# Method 1 for mediation analysis in R

# read data
Mediation_data <- 
  read.csv('https://raw.githubusercontent.com/TidyPython/Mediation_analysis/main/mediation_hypothetical_data.csv')

# show the head of the data
head(Mediation_data)

# c path linear regression 
c_path = lm(Y~X, data=Mediation_data)
summary(c_path)

# a path linear regression 
a_path = lm(M~X, data=Mediation_data)
summary(a_path)

# b path linear regression 
b_path = lm(Y~M+X, data=Mediation_data)
summary(b_path)


set.seed(123)
#install.packages("mediation") #in case not installed "mediation" yet
library(mediation)

# Mediation Result
#ACME:Average Causal Mediation Effects = Indirect Effect = a*b
#ADE:Average Direct Effects = c' 
#Total Effect: c 
#Prop.Mediated:ACME/Total Effect
results = mediate(a_path, b_path ,sims=5000, treat='X', mediator='M', boot=T)

# Print out the mediation result
summary(results)



