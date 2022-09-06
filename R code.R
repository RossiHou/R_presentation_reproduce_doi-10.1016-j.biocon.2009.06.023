library(nlme)
library(readxl)
library(car)
library(multcomp)
library(gvlma)
yield1 <- read_xlsx("E:/Rtrain/presentation/002006-2007.xlsx")
yield2 <- read_xlsx("E:/Rtrain/presentation/002007-2008.xlsx")
overall <- read_xlsx("E:/Rtrain/presentation/overall.xlsx")
yield1$distance <- factor(yield1$distance,order=TRUE,levels=c("less than 1km","between 1km to 3km","greater than 3km"))
yield2$distance <- factor(yield2$distance,order=TRUE,levels=c("less than 1km","between 1km to 3km","greater than 3km"))
overall$distance <- factor(overall$distance,order=TRUE,levels=c("less than 1km","between 1km to 3km","greater than 3km"))
str(yield1)
str(yield2)
str(overall)
##      plot yield1
means1 <- aggregate(yield1$yield,by=list(yield1$hivetype,yield1$distance), FUN=mean)
means1s <- xtabs(means1$x~means1$Group.1+means1$Group.2)
means1s <- t(means1s)
barplot(means1s,col = c("red","yellow","blue"),legend=rownames(means1s),
        beside = TRUE)

##       plot  yield2
means2 <- aggregate(yield2$yield,by=list(yield2$hivetype,yield2$distance), FUN=mean)
means2s <- xtabs(means2$x~means1$Group.1+means1$Group.2)
means2s <- t(means2s)
barplot(means2s,col = c("red","yellow","blue"),legend=rownames(means2s),
        beside = TRUE)

####################################################
###          yield normality             

##When analysing large samples of data larger than 50 rows, 
##we tend to look at the normality test results obtained from the Kolmogorov-Smirnov Tests(K-S) test.
ks.test(yield1$yield,"pnorm")

ks.test(yield2$yield,"pnorm")

##When analysing small samples of data with less than 50 rows,
##we tend to look at the normality test results obtained from the Shapiro-Wilk test
shapiro.test(yield1$yield)
shapiro.test(yield2$yield)
###                           it is normally distribute

####################################################################
########                      aonva test                ############

##GLM
##The kernel of the aov() function uses the lm() algorithm

##          2006-2007  aonva Tukey’s Test 
fit1 <- aov(yield~hivetype + distance,data=yield1)
TukeyHSD(fit1)

##                      plot fit1 anova result
par(mar=c(5,4,6,2))
tuk11 <- glht(fit1, linfct=mcp(distance = "Tukey"))
plot(cld(tuk11,level=.05),col="lightgrey")

##  2007-2008 aonva Tukey’s Test 
fit2 <- aov(yield~hivetype + distance,data=yield2)
TukeyHSD(fit2)

###                  plot fit2 anova result

par(mar=c(5,4,6,2))
tuk21 <- glht(fit2, linfct=mcp(distance = "Tukey"))
plot(cld(tuk21,level=.05),col="lightgrey")
#######################################################

#############         regression analysis

###########              use  OLS

OLS <- lm(yield~hivetype + distance,data=overall)


##########################      regression diagnostics for OLS

##                          overall 

summary(OLS)
par(mfrow = c(2, 2))
plot(OLS,ask = FALSE)


####                     Normality

par(mfrow=c(1,1))
qqPlot(OLS,labels=row.names(overall),simulate=TRUE,main="Q-Q PLOT")

#####                  Independence of error

durbinWatsonTest(OLS)
##p>0.05, error term is independence
##p<0.05,it has autocorrelation, error term is not independence
###that's why paper said:"suggested the errors were not random, thus violating the assumption of independence of errors and leading to biased parameter estimates."

##############            homoscedasticity

ncvTest(OLS)
spreadLevelPlot(OLS)
#############  p>0.05---Satisfies the assumption of constant variance


####                    summary

gvmodel1 <- gvlma(OLS)
summary(gvmodel1)

##################

###############      use GLS

GLS <- gls(yield~hivetype+distance,data = overall)

##                          overall 
summary(GLS)
par(mfrow = c(2, 2))
plot(GLS)
residuals(GLS) 
par(mfrow = c(1, 1))
plot(predict(GLS,type="response"),residuals(GLS))
exp(coef(GLS))
