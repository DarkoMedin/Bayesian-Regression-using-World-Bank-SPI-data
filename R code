#Load the 'rstanarm' library
library(rstanarm)
library(dplyr)


#Import the Codon usage dataset
spi_index_labelled <- read.csv("C:/Users/DARKO/Desktop/Projects/World bank SPI project/spi_index_labelled.csv"
                               ,na.strings = "NA")



#Select the variables related to services and data products
#Dropping first row to easier specify names of the variables later
mat_spi=spi_index_labelled[-1,5:6]


#Data wrangling : Convert the dataframe variables to numeric and set names
#Dplyr pipe operators are ideal to speed up
spi <- mat_spi %>% mutate_all( as.numeric)
vname<-c("serv", "prod")
colnames(spi)<-vname




#Using Normal prior - Weak with scale of 120
prior=normal(location = 0, scale = 120)


#Monte Carlo simulations will be run and the prior used to derive a set of poseterior simularions
#3000 iterations should generally be enough for this analysis
mc_fit=stan_glm(serv~prod, data = spi, prior=prior, family = "gaussian", iter=3000)


#Summarize the mc_fit model with 7 decimal places
summary(mc_fit, digits=7)



#Plot the posterior distributions and chains diagnostics
plot(mc_fit, "combo")


#Find the 99% credible interval
posterior_interval(mc_fit, prob = 0.99)


#Store simulations as a matrix object and calculate the probability of a coefficient
#being > 1.1
sim=as.matrix(mc_fit)
betas=as.matrix(sim[,2])
prob=sum(betas>1.1)/6000
prob















