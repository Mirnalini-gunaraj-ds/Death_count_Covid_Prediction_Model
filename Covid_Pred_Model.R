#COVID Death Prediction using Economic and Health factors

# set working directory
setwd(dirname(file.choose()))
getwd()

# read in data from csv file
Covid_Pred <- read.csv("u2195687_DS7006_CW2_data.csv", stringsAsFactors = FALSE,
                       fileEncoding = 'UTF-8-BOM',na.strings = '..')
head(Covid_Pred)
str(Covid_Pred)
summary(Covid_Pred)

#Data Exploration
#Highest and Lowest death as per region:
library("data.table") 
Covid_reg = Covid_Pred[c("LA_code","LA_name","Total_Deaths")]
Covid_reg <- Covid_reg[order(Covid_reg$Total_Deaths, decreasing = TRUE), ]

head(Covid_reg,n=2)
tail(Covid_reg,n=2)

#Checking Missing values
install.packages("VIM")
library(VIM)
miss_plot <- aggr(Covid_Pred, col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(Covid_Pred), cex.axis=0.3,
                    gap=1, ylab=c("Missing data","Pattern"))


# select a dependent variable and independent variables
Covid_Pred2 <- data.frame(Covid_Pred$Total_Deaths,Covid_Pred$Inemp_FT_limitedalot_Dis,Covid_Pred$Inemp_FT_limitedalittle_Dis,Covid_Pred$Inemp_FT_notlimited_Dis,
                          Covid_Pred$Inemp_PT_limitedalot_Dis,Covid_Pred$Inemp_PT_limitedalittle_Dis,Covid_Pred$Inemp_PT_notlimited_Dis,
                          Covid_Pred$Unemp_limitedalot_Dis,Covid_Pred$Unemp_limitedalittle_Dis,Covid_Pred$Unemp_notlimited_Dis,
                          Covid_Pred$verybadHlth_LTHP_Age0to15,Covid_Pred$verybadHlth_LTHP_Age16to49,Covid_Pred$verybadHlth_LTHP_Age50to64,
                            Covid_Pred$verybadHlth_LTHP_Age65andover,Covid_Pred$GenHlthDis_LTHP_Age0to15,Covid_Pred$GenHlthDis_LTHP_Age16to49,
                          Covid_Pred$GenHlthDis_LTHP_Age50to64,Covid_Pred$GenHlthDis_LTHP_Age65andover,Covid_Pred$Single_Living,
                          Covid_Pred$Joint_Living,Covid_Pred$Partner_Living)
colnames(Covid_Pred2) <- c("Total_Deaths","Inemp_FT_limitedalot_Dis","Inemp_FT_limitedalittle_Dis","Inemp_FT_notlimited_Dis",
                           "Inemp_PT_limitedalot_Dis","Inemp_PT_limitedalittle_Dis","Inemp_PT_notlimited_Dis",
                           "Unemp_limitedalot_Dis","Unemp_limitedalittle_Dis","Unemp_notlimited_Dis",
                           "verybadHlth_LTHP_Age0to15","verybadHlth_LTHP_Age16to49","verybadHlth_LTHP_Age50to64",
                           "verybadHlth_LTHP_Age65andover","GenHlthDis_LTHP_Age0to15","GenHlthDis_LTHP_Age16to49",
                           "GenHlthDis_LTHP_Age50to64","GenHlthDis_LTHP_Age65andover","Single_Living","Joint_Living","Partner_Living")

#Normality of data - Density plot - bell shape
library("ggpubr")
ggdensity(Covid_Pred$Total_Deaths, 
          main = "Density plot of Total Death",
          xlab = "Total Death",
          ylab = "Density")

#Normality using Shapiro test:
shapiro.test(Covid_Pred$Total_Deaths)

#Normality using boxplot
boxplot(Covid_Pred2)

#Standardisation
standardize = function(x){
  return ((x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE ))
}
Covid_Std = as.data.frame(apply(Covid_Pred2,2,standardize))
boxplot(Covid_Std)

#Normalisation
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
Covid_nom = as.data.frame(apply(Covid_Std,2,normalize))
boxplot(Covid_nom)

#Log - Transformations
Covid_log <- log(Covid_Pred2)
boxplot(Covid_log)

#Outlier Imputation
out1 <- function(x){
  for (i in which(sapply(x, is.numeric))) {
    quantiles <- quantile( x[,i], c(.05, .95 ), na.rm =TRUE)
    x[,i] = ifelse(x[,i] < quantiles[1] , quantiles[1], x[,i])
    x[,i] = ifelse(x[,i] > quantiles[2] , quantiles[2], x[,i])}
  x}

# Replacing extreme values with percentiles
Covid_out = out1(Covid_log)
boxplot(Covid_out)

#Correlation between variables - Corrgram
library(corrgram)
# corrgram works best with Pearson correlation
corrgram(Covid_out, order=FALSE, cor.method = "pearson", lower.panel=panel.conf,
         upper.panel=panel.pie, text.panel=panel.txt, main="Covid Vs Economic,Health and Living")

# test correlation of dependent variable with all independent variables
cor.test(Covid_out$Total_Deaths, Covid_out$Inemp_FT_limitedalot_Dis, method = "spearman",exact = FALSE)
cor.test(Covid_out$Total_Deaths, Covid_out$Inemp_FT_limitedalittle_Dis, method = "spearman",exact = FALSE)
cor.test(Covid_out$Total_Deaths, Covid_out$Unemp_limitedalot_Dis, method = "spearman",exact = FALSE)
cor.test(Covid_out$Total_Deaths, Covid_out$Unemp_limitedalittle_Dis, method = "spearman",exact = FALSE)
cor.test(Covid_out$Total_Deaths, Covid_out$Unemp_notlimited_Dis, method = "spearman",exact = FALSE)
cor.test(Covid_out$Total_Deaths, Covid_out$verybadHlth_LTHP_Age0to15, method = "spearman",exact = FALSE)
cor.test(Covid_out$Total_Deaths, Covid_out$verybadHlth_LTHP_Age16to49, method = "spearman",exact = FALSE)
cor.test(Covid_out$Total_Deaths, Covid_out$verybadHlth_LTHP_Age50to64, method = "spearman",exact = FALSE)
cor.test(Covid_out$Total_Deaths, Covid_out$verybadHlth_LTHP_Age65andover, method = "spearman",exact = FALSE)
cor.test(Covid_out$Total_Deaths, Covid_out$GenHlthDis_LTHP_Age0to15, method = "spearman",exact = FALSE)
cor.test(Covid_out$Total_Deaths, Covid_out$Joint_Living, method = "spearman",exact = FALSE)

# looking at internal correlations between three variables
#variable 1
cor.test(Covid_out$Inemp_FT_limitedalittle_Dis,Covid_out$Unemp_notlimited_Dis, method = "spearman",exact = FALSE)
cor.test(Covid_out$Inemp_FT_limitedalittle_Dis,Covid_out$verybadHlth_LTHP_Age0to15, method = "spearman",exact = FALSE)
cor.test(Covid_out$Inemp_FT_limitedalittle_Dis,Covid_out$verybadHlth_LTHP_Age50to64, method = "spearman",exact = FALSE)
cor.test(Covid_out$Inemp_FT_limitedalittle_Dis,Covid_out$verybadHlth_LTHP_Age65andover, method = "spearman",exact = FALSE)


#variable2
cor.test(Covid_out$Unemp_notlimited_Dis,Covid_out$verybadHlth_LTHP_Age0to15, method = "spearman",exact = FALSE)
cor.test(Covid_out$Unemp_notlimited_Dis,Covid_out$verybadHlth_LTHP_Age50to64, method = "spearman",exact = FALSE)
cor.test(Covid_out$Unemp_notlimited_Dis,Covid_out$verybadHlth_LTHP_Age65andover, method = "spearman",exact = FALSE)


#variable3
cor.test(Covid_out$verybadHlth_LTHP_Age0to15,Covid_out$verybadHlth_LTHP_Age50to64, method = "spearman",exact = FALSE)
cor.test(Covid_out$verybadHlth_LTHP_Age0to15,Covid_out$verybadHlth_LTHP_Age65andover, method = "spearman",exact = FALSE)

#variable 4
cor.test(Covid_out$verybadHlth_LTHP_Age50to64,Covid_out$verybadHlth_LTHP_Age65andover, method = "spearman",exact = FALSE)


#partial correlation
library(ppcor)

#calculate partial correlation using Pearson 
pcor.test(Covid_out$Total_Deaths, Covid_out$Unemp_notlimited_Dis,Covid_out$Inemp_FT_limitedalot_Dis)
pcor.test(Covid_out$Total_Deaths, Covid_out$Inemp_FT_limitedalot_Dis,Covid_out$Unemp_notlimited_Dis)

#calculate partial correlation using spearman 
pcor.test(Covid_out$Total_Deaths, Covid_out$Unemp_notlimited_Dis,Covid_out$Inemp_FT_limitedalot_Dis,method='spearman')
pcor.test(Covid_out$Total_Deaths, Covid_out$Inemp_FT_limitedalot_Dis,Covid_out$Unemp_notlimited_Dis,method='spearman')


# select variables by excluding those not required; the %in% operator means 'matching'
myvars <- names(Covid_out) %in% c("Total_Deaths",
                                   "Inemp_FT_limitedalittle_Dis","Inemp_FT_notlimited_Dis",
                                   "Inemp_PT_limitedalittle_Dis","Inemp_PT_notlimited_Dis",
                                   "Unemp_limitedalittle_Dis","verybadHlth_LTHP_Age0to15",
                                   "verybadHlth_LTHP_Age16to49","GenHlthDis_LTHP_Age0to15",
                                   "GenHlthDis_LTHP_Age16to49","Single_Living","Partner_Living")
# the ! operator means NOT
Covid_Pred3 <- Covid_out[!myvars]
str(Covid_Pred3)
rm(myvars)

# Kaiser-Meyer-Olkin statistics: if overall MSA > 0.6, proceed to factor analysis
library(psych)
KMO(cor(Covid_Pred3))

# Determine Number of Factors to Extract
library(nFactors)

# get eigenvalues: eigen() uses a correlation matrix
ev <- eigen(cor(Covid_Pred3))
ev$values
# plot a scree plot of eigenvalues
plot(ev$values, type="b", col="blue", xlab="variables")


# calculate cumulative proportion of eigenvalue and plot
ev.sum<-0
for(i in 1:length(ev$value)){
  ev.sum<-ev.sum+ev$value[i]
}
ev.list1<-1:length(ev$value)
for(i in 1:length(ev$value)){
  ev.list1[i]=ev$value[i]/ev.sum
}
ev.list2<-1:length(ev$value)
ev.list2[1]<-ev.list1[1]
for(i in 2:length(ev$value)){
  ev.list2[i]=ev.list2[i-1]+ev.list1[i]
}
plot (ev.list2, type="b", col="red", xlab="number of components", ylab ="cumulative proportion")

# Varimax Rotated Principal Components
# retaining 'nFactors' components
library(GPArotation)

# principal() uses a data frame or matrix of correlations
fit <- principal(Covid_Pred3, nfactors=3, rotate="varimax")
fit

# weed out further variables after first factor analysis
myvars <- names(Covid_Pred3) %in% c("Inemp_PT_limitedalot_Dis",
                                    "Joint_Living","verybadHlth_LTHP_Age50to64", 
                                    "GenHlthDis_LTHP_Age65andover")
Covid_Pred3 <- Covid_Pred3[!myvars]
str(Covid_Pred3)
rm(myvars)

library(psych)
KMO(cor(Covid_Pred3))

# get eigenvalues
ev <- eigen(cor(Covid_Pred3))
ev$values
# plot a scree plot of eigenvalues
plot(ev$values, type="b", col="blue", xlab="variables")

# calculate cumulative proportion of eigenvalue and plot
ev.sum<-0
for(i in 1:length(ev$value)){
  ev.sum<-ev.sum+ev$value[i]
}
ev.list1<-1:length(ev$value)
for(i in 1:length(ev$value)){
  ev.list1[i]=ev$value[i]/ev.sum
}
ev.list2<-1:length(ev$value)
ev.list2[1]<-ev.list1[1]
for(i in 2:length(ev$value)){
  ev.list2[i]=ev.list2[i-1]+ev.list1[i]
}
plot (ev.list2, type="b", col="red", xlab="number of components", ylab ="cumulative proportion")

# Varimax RLotated Principal Components
# retaining 'nFactors' components
fit <- principal(Covid_Pred3, nfactors=3, rotate="varimax")
fit

#Model 1 - Simple Linear Regression
#attach(Covid_Pred)
plot(Covid_out$Inemp_FT_limitedalot_Dis,Covid_out$Total_Deaths, main = "Scatterplot",
     xlab = "Full Time In employed", ylab = "Deaths",pch=19)
#Added Regression Line
abline(lm(Covid_out$Total_Deaths ~ Covid_out$Inemp_FT_limitedalot_Dis, data = mtcars), col = "blue")

# K-S test
ks.test(Covid_out$Total_Deaths, "pnorm", mean(Covid_out$Total_Deaths), sd(Covid_out$Total_Deaths))
# or... Shapiro-Wilk's test
shapiro.test(Covid_out$Total_Deaths)

model1 <- lm(Covid_out$Total_Deaths ~ Covid_out$Inemp_FT_limitedalot_Dis)
summary(model1)

hist(model1$residuals)
rug(model1$residuals)

# consider normality of residuals
plot(model1$residuals ~ model1$fitted.values, xlab = "fitted values", ylab = "residuals")
ks.test(model1$residuals, "pnorm", mean(model1$residuals), sd(model1$residuals))

# Model 2 - Multiple Regression
# model with all variables

model2 <- lm(Covid_out$Total_Deaths ~Covid_out$Inemp_FT_limitedalot_Dis +Covid_out$Inemp_FT_limitedalittle_Dis +Covid_out$Inemp_FT_notlimited_Dis +
              Covid_out$Inemp_PT_limitedalot_Dis +Covid_out$Inemp_PT_limitedalittle_Dis +Covid_out$Inemp_PT_notlimited_Dis  +
              Covid_out$Unemp_limitedalot_Dis +Covid_out$Unemp_limitedalittle_Dis +Covid_out$Unemp_notlimited_Dis +
              Covid_out$verybadHlth_LTHP_Age0to15 +Covid_out$verybadHlth_LTHP_Age16to49 +Covid_out$verybadHlth_LTHP_Age50to64 +
              Covid_out$verybadHlth_LTHP_Age65andover +Covid_out$GenHlthDis_LTHP_Age0to15 +Covid_out$GenHlthDis_LTHP_Age16to49 +
              Covid_out$GenHlthDis_LTHP_Age50to64 +Covid_out$GenHlthDis_LTHP_Age65andover +
              Covid_out$Single_Living +Covid_out$Partner_Living +Covid_out$Joint_Living )
summary(model2)

# calculate variance inflation factor
library(car)
vif(model2)
sqrt(vif(model2)) > 2  # if > 2 vif too high

plot(model2$residuals ~ model2$fitted.values, xlab = "fitted values", ylab = "residuals")
ks.test(model2$residuals, "pnorm", mean(model2$residuals), sd(model2$residuals))

hist(model2$residuals)
rug(model2$residuals)
# Model 3 - model with Factor analysis variables 
cor3 <- cor(Covid_out[, c(1,2,13,14,18)], method = "spearman")
round(cor3, 2)
corrplot(cor3, type = "upper", tl.col = "black", tl.srt = 45)

model3 <- lm(Covid_out$Total_Deaths ~ Covid_out$Inemp_FT_limitedalot_Dis + Covid_out$verybadHlth_LTHP_Age50to64 +
               Covid_out$verybadHlth_LTHP_Age65andover + Covid_out$GenHlthDis_LTHP_Age65andover)
summary(model3)
sqrt(vif(model3)) > 2

ks.test(model3$residuals, "pnorm", mean(model3$residuals), sd(model3$residuals))

#Model 3a - calculate partial correlation 
library(ppcor)
#pcor.test(Total_Deaths, , )
#pcor.test(Total_Deaths, , )

model3a <- lm(Covid_out$Total_Deaths ~
                Covid_out$Unemp_notlimited_Dis + Covid_out$Inemp_FT_limitedalot_Dis + Covid_out$verybadHlth_LTHP_Age65andover)
summary(model3a)
sqrt(vif(model3a)) > 2

ks.test(model3a$residuals, "pnorm", mean(model3a$residuals), sd(model3a$residuals))



# use a stepwise approach to search for a best model
library(RcmdrMisc)

# Model 3b - Other then PCA
cor3b <- cor(Covid_out[, c(1,3,4,5,6,7,8,9,10,11,12,15,16,17,19,20,21)], method = "spearman")
corrplot(cor3b, type = "upper", tl.col = "black", tl.srt = 45)

model3b <- lm (Covid_out$Total_Deaths ~ Covid_out$Inemp_FT_limitedalittle_Dis + Covid_out$Inemp_FT_notlimited_Dis +
                 Covid_out$Inemp_PT_limitedalot_Dis + Covid_out$Inemp_PT_limitedalittle_Dis + Covid_out$Inemp_PT_notlimited_Dis  +
                 Covid_out$Unemp_limitedalot_Dis + Covid_out$Unemp_limitedalittle_Dis + Covid_out$Unemp_notlimited_Dis +
                 Covid_out$verybadHlth_LTHP_Age0to15 + Covid_out$verybadHlth_LTHP_Age16to49 + Covid_out$GenHlthDis_LTHP_Age0to15 + Covid_out$GenHlthDis_LTHP_Age16to49 +
                 Covid_out$GenHlthDis_LTHP_Age50to64 + Covid_out$GenHlthDis_LTHP_Age65andover +
                 Covid_out$Single_Living + Covid_out$Partner_Living + Covid_out$Joint_Living)
summary(model3b)
sqrt(vif(model3b)) > 2
calc.relimp(model3b, type = c("lmg"), rela = TRUE)

ks.test(model3b$residuals, "pnorm", mean(model3b$residuals), sd(model3b$residuals))

# Model 4 - forward stepwise selection 
library(RcmdrMisc)
model4 <- stepwise(model3b, direction = "forward")
summary(model4)
hist(model4$residuals)
rug(model4$residuals)
plot(model4$residuals ~ model4$fitted.values, xlab = "fitted1 values", ylab = "residuals")
ks.test(model4$residuals, "pnorm", mean(model4$residuals), sd(model4$residuals))
sqrt(vif(model4)) > 2
calc.relimp(model4, type = c("lmg"), rela = TRUE)

ks.test(model4$residuals, "pnorm", mean(model4$residuals), sd(model4$residuals))

# test whether model2 and model4 are significantly different using F test
anova(model2, model4, test = "F")

# Model 5 - use a stepwise approach to search for a best model
model5 <- stepwise(model2, direction = "forward")
summary(model5)
hist(model5$residuals)
rug(model5$residuals)
plot(model5$residuals ~ model5$fitted.values, xlab = "fitted values", ylab = "residuals")
ks.test(model5$residuals, "pnorm", mean(model5$residuals), sd(model5$residuals))
sqrt(vif(model5)) > 2
calc.relimp(model5, type = c("lmg"), rela = TRUE)

# test whether model2 and model5 are significantky different using F test
anova(model2, model5, test = "F")

