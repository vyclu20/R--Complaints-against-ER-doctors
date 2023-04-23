library(dplyr)
library(lattice)
library(ggplot2)
library(MASS)
library(DHARMa)
library(lmtest)
library(AER)
library(countreg)
library(magrittr)

# To clarify, some of the code here was not included. In reality, I did so many more tests and checks, but if I were to include them here it would be too much

# Step 1: Checking everything: summaries, outliers, zero inflation, simple plots between variables

 compdat <- read.delim("compdat.txt")
 compdat <- compdat[!duplicated(compdat[, -1]), ]
 compdat <- na.omit(compdat)
 summary(compdat)
 compdat[compdat$complaints > 5,1:6]
 compdat[compdat$visits>2271 & compdat$hours>1469 & compdat$complaints > 5,1:6]
 zeros<compday[compdat$complaints==0,1:6]
 
 sd(compdat$visits)>sd(compdat$revenue)
 sd(compdat$complaints)>sd(compdat$hours)
 
 #factoring categorical variables
 compdat$residency<- as.factor(compdat$residency)
 compdat$gender<-as.factor(compdat$gender)
 count(compdat, 'residency')
 count(compdat, 'gender')
 
 #checking for outliers and doing simple graphs with and without interactions
 plot(compdat$complaints ~ compdat$visits)
 plot(compdat$complaints ~ compdat$residency)
 plot(compdat$complaints ~ compdat$gender)
 plot(compdat$complaints ~ compdat$hours)
 plot(compdat$complaints ~ compdat$revenue)
 
 
 c.lm <- lm(complaints~visits+revenue+hours+gender+residency, compdat) 
 c.lm2g <-lm(complaints~visits+revenue+hours+gender,compdat)
 c.lm2r <- lm(complaints~visits+revenue+hours+residency,compdat)
 c.lm3g <- lm(complaints~(visits+revenue+hours)*gender,compdat)
 c.lm3r <- lm(complaints~(visits+revenue+hours)*residency,compdat)
 c.lm3rg <- lm(complaints~(visits+revenue+hours)*residency+gender,compdat)
 c.lm3gr <- lm(complaints~(visits+revenue+hours)*gender+residency,compdat)
 par(mfrow=c(1,1))
 plot(c.lm)
 
 histogram(~complaints | gender, compdat, breaks = 0:18 - 0.5, 
           main = "Complaints")
 
 #did not remove outliers, for after further inspection,
 #it became obvious that the data is zero inflated and that it would make the already limited dataset less reliable if I were to remove a value that was not 0
 
 #Step 2: Checking for potential models such as poisson, quasi and negative binomial. Dispersion as well
 
 #GLM Poisson model(not yet zero inflated)
 compdat.p <- glm(complaints~visits+revenue+hours+gender+residency | visits+revenue+hours+gender+residency,
                  data = compdat,
                  family = poisson)
 summary(compdat.p)
 compdat.p$deviance/compdat$p$df.residual
 
 #GLM Quasipoisson model (not yet zero inflated, there won't be one for quasi)
 compdat.q <- glm(complaints~visits+revenue+hours+gender+residency | visits+revenue+hours+gender+residency,
                  data = compdat,
                  family = quasipoisson)
 compdat.q$deviance/compdat.q$df.residual
 summary(compdat.q)
 
 #GLM Negative binomial model (not yet zero inflated)
 compdat.nb <- glm.nb(complaints~visits+revenue+hours+gender+residency | visits+revenue+hours+gender+residency,data = compdat, control = glm.control(maxit = 50))
 compdat.nb$deviance/compdat.nb$df.residual
 summary(compdat.nb)
 
 #Executing the codes above, dispersion was detected using the deviance to residual value. 
 #We can see that both the poisson models are overdispersed, and since we can't do much with quasi (no AIC, etc), 
 #we assume that quasi is not a suitable model for this data.
 dispersiontest(compdat.p)
 nb.dharm <- simulateResiduals(compdat.nb, refit=T, n=99)
 plotSimulatedResiduals(compdat.nb)
 testOverdispersion(nb.dharm)
 
 rootogram(compdat.p)
 box()
 rootogram(compdat.nb)
 box()
 AIC(compdat.p,compdat.nb)
 
 #Comparing the AIC's of the poisson and negative binomial models, it shows that the negative binomial model has a lower AIC, 
 #thus suggesting that negative binomial is a much better model than the poisson.
 
 #Step 3: Zero inflation models, with and without interactions. Doing tests such as likelihood tests and AIC tests to see which model is the best fit
 #Since it is zero-inflated, it is only fair we go for zero-inflated models of Poisson ad Negative Binomial
 
 #the codes included here are only the ones that were used in the final report.
 compdat.zip1 <- zeroinfl(complaints~revenue+gender+residency | visits+revenue+hours+gender+residency,
                          dist = 'poisson', data = compdat)
 summary(compdat.zip1)
 
 compdat.zip2 <- zeroinfl(complaints~visits+revenue+hours| gender,
                          dist = 'poisson', data = compdat)
 
 compdat.zinb <- zeroinfl(complaints~hours+revenue+gender+residency| revenue+gender+residency,
                          dist = 'negbin', data = compdat)
 summary(compdat.zinb)
  
 AIC(compdat.p,compdat.nb,compdat.zip1,compdat.zinb)
 rootogram(compdat.zip1)
 box()
 rootogram(compdat.zinb)
 box()
 
 #Step 4: Reducing the model to get the final model
 compdat.zinb1 <- zeroinfl(complaints~visits+revenue+hours | gender,
                           dist = 'negbin', data = compdat)
 summary(compdat.zinb1)
 rootogram(compdat.zinb1)
 box()
 
 compdat.zinb6 <- zeroinfl(complaints~hours*residency+gender:hours+residency:gender| visits+gender+visits:hender,
                           dist = 'negbin', data = compdat)
 summary(compdat.zinb6)
 rootogram(compdat.zinb6)
 box()
 
 compdat.zinb7 <- zeroinfl(complaints~hours*residency+gender*residency| visits*gender,
                           dist = 'negbin', data = compdat)
 
 options(warnings = -1)
 par(mfrow = c(1,2))
 
 plot(residuals(compdat.zinb1) ~ fitted.values(compdat.zinb1, 
                                               type = "pearson"),
      xlab = "Fitted values", ylab = "Pearson Residuals",
      main = "With Interactions")
 plot(residuals(compdat.zinb1, type = "pearson"), 
      ylab = "Pearson residuals")
 par(mfrow=c(1,1))
 options(warnings = 0)
 
 plot(residuals(compdat.zinb7) ~ fitted.values(compdat.zinb7, 
                                               type = "pearson"),
      xlab = "Fitted values", ylab = "Pearson Residuals",
      main = "With Interactions")
 plot(residuals(compdat.zinb7, type = "pearson"), 
      ylab = "Pearson residuals")
 par(mfrow=c(1,1))
 options(warnings = 0)
 anova(compdat.zinb7, compdat.zinb1, test = 'LRT')
 anova <- aov (complaints~ hours*residency, data = compdat)
 summary(anova)
 
 stepAIC(compdat.zip2, direction = "backward")
 stepAIC(compdat.zip2, direction = "forward")
 stepAIC(compdat.zip2, direction = "both")
 
 stepAIC(compdat.zinb7, direction = "backward")
 stepAIC(compdat.zinb7, direction = "forward")
 stepAIC(compdat.zinb7, direction = "both")
 AIC(compdat.zip2,compdat.zinb7)
 lrtest(compdat.zinb1,compdat.zinb8)
 
 #Checking AIC shows that the model without interactions has a lower AIC, but that is expected considering the fact that it is simpler. 
 #Since it may be conflicting to use a model without interactions than one with, we want to check for the goodness of fit of both models. 
 #Running the likelihood test shows that the p value is less than 0.05, so we will not reject the null hypothesis and take the simpler model: 
 #zinb1, the model without interactions, as our final model.
