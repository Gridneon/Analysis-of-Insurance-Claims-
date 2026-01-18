# Analysis-of-Insurance-Claims-
Analysis of Insurance Claims (skewness and kurtosis  for heavy tail distributions )


# we want to state the relationship between 
# sample size of claims based on their skewness and kurtosis and fit heavy tail distributions 

#installing need packages--------------------------------------------------
install.packages(moments)#for kurtosis and skewness
install.package(fitdistrplus) # is to get fitdist a distribution and get the needed parameters
install.packages(actuar)

#load into our environment----------------------------------------------
library(moments)  
library(fitdistrplus)
library(actuar)


# read data-------------------------------------------------------------------------------
data<-read.csv("C:\\Users\\HP\\Downloads\\Miss. Nyarko\\claimdata18_24.csv")

claimdata<-data[,"claim_amt"] #extracting claim_amt from data
length(claimdata)  #selecting claim data in data


claimdata<-sort(claimdata)
data_1<-claimdata[-c(1,2,3,4)]


#test which distribution fits better using AIC and Likelihood value 
fitlnorm<-fitdist(data_1, distr="lnorm", method = c("mle"))

fitpareto<-fitdist(data_1, distr="pareto", method = c("mle"))

fitburr<-fitdist(data_1, distr="burr", method = c("mle"),
                 start = list(shape1 = 2, shape2 = 2, scale = 1) )

#using summary 
summary(fitlnorm)
summary(fitpareto)
summary(fitburr)

########### Goodness of fit #############
#summary on the parameters, AIC and Likelihood of each fitted distribution
gofstat(list(fitlnorm,fitpareto,fitburr))

#################### Estimate of each parametter##################
burr_params<-fitburr$estimate
pareto_params<-fitpareto$estimate
lnorm_params<-fitlnorm$estimate
##################################################################
########### histogram of empirical data)##################
par(mfrow=c(1,1))
hist(data_1,breaks = 12,probability = TRUE, 
     main = NULL, 
     xlab = "Claim Amount", 
     ylab = "Density",
     col = "gray")
##################################################
################curves of each distribution#####################

curve(dburr(x, shape1=burr_params[1], shape2=burr_params[2]),
      add = TRUE, col = "darkblue",lty = 3, lwd = 2)
curve(dlnorm(x, meanlog=lnorm_params[1],sdlog =lnorm_params[2]),
      add = TRUE, col = "seagreen",lty = 3, lwd = 2)
curve(dpareto(x, shape=pareto_params[1], scale=pareto_params[2]),
      add = TRUE, col = "darkorange", lty = 3,lwd = 2)  


###########legend#############
legend("topright", legend = c("burr","lnorm","Pareto"), 
       col = c("darkblue", "seagreen","darkorange"), lty=3, lwd = 2)




#generating data from lognormal because its the best fit for our data 
rlnorm_claimamt<-rlnorm(n =  12968, meanlog = 8.144106 , sdlog = 1.496993)

#---------------final function---------------------------------------
sample_sizes <- seq(50, 500, by = 50)
R <- 1000


# Loop through each sample size
set.seed(222)
PA_s_k<-function(x,par1,par2){
  mean_1 <- numeric(length(x));mean_2 <- numeric(length(x))
  for(i in seq_along(x)){
    n_size<-x[i]
  
    # Bootstrap samples
    #small tail index means heavy tail (Expect very frequent extreme events(massive insurance claims))
    #large tail index means lighter tail (Expect very low extreme events(massive insurance claims are rare))
   
     #-------large tail index(alpha) burr ---- = shape1 * shape2 >4 ----> low extreme event 
     
     #-------small tail index(alpha) burr ---- = shape1 * shape2 <4 ----> highly extreme event
     
     boot_samples <- sapply(1:R, function(j)rburr(n=n_size,shape1 = par1,shape2 =par2,scale = 1))
     ifelse(par1 * par2 < 4,print("Small Tail Index"),print("Large tail Index"))
     
    
    #skewness(sk) and kurtosis(g1) of each boot_sample
    sk<-apply(boot_samples,2,skewness);k<-apply(boot_samples,2,kurtosis)
    mean_1[i]<-mean(sk);mean_2[i]<-mean(k)
    
}
  #plot of Sample Size against skewness and kurtosis
  par(mfrow = c(1,2))
  plot(x,mean_1, type = "b", pch = 19, col = "darkblue",
       xlab = "Sample Size", ylab = "Average  Skewness")
  plot(x,mean_2, type = "b", pch = 19, col = "darkblue",
       xlab = "Sample Size", ylab = "Average  Kurtosis")
 
  
}

PA_s_k(sample_sizes,1,) 


