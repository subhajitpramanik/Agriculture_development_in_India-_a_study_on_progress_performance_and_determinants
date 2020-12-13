# multiple linear regression
# importing data
install.packages("readxl")
library(readxl)
minty <- read_excel("C:/Users/subha/Downloads/minty.xlsx")
View(minty)
# matrix of scatter plots
pairs(minty[,1:6])
# correlation coefficient matrix
round(cor(minty, method="pearson"), 2)
# or
install.packages("Hmisc")
library(Hmisc)
rcorr(as.matrix(minty))
# fitting multiple linear regression
mlrm<-lm(agr_val_ad~agr_ld+agr_crd+rain_fl+agr_emp, data=minty)
mlrm
summary(mlrm)
# confidence interval for the regression coefficients at 95% level of confidence
confint(mlrm, level=0.95)
# extract model residuals 
resid(mlrm)
#describing data
install.packages("psych")
library(psych)
describe(minty)
# drawing correlation plots
install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(cor(minty))
# creating scatter plots
install.packages("ggplot2")
library(ggplot2)
library(ggcorrplot)
tail(minty)
sp <- ggplot(data=minty , aes(x=agr_ld,y=agr_val_ad))
sp + geom_point()
sp <- sp + geom_point()
sp + ggtitle("first scatter plot")
sp + ggtitle("first scatter plot") + stat_smooth(method="lm",se=FALSE)
sp <- ggplot(data=minty , aes(x=agr_crd,y=agr_val_ad))
sp + geom_point()
sp <- sp + geom_point()
sp + ggtitle("second scatter plot")
sp + ggtitle("second scatter plot") + stat_smooth(method="lm",se=FALSE)
sp <- ggplot(data=minty , aes(x=rain_fl,y=agr_val_ad))
sp + geom_point()
sp <- sp + geom_point()
sp + ggtitle("third scatter plot")
sp + ggtitle("third scatter plot") + stat_smooth(method="lm",se=FALSE)
sp <- ggplot(data=minty , aes(x=agr_emp,y=agr_val_ad))
sp + geom_point()
sp <- sp + geom_point()
sp + ggtitle("fourth scatter plot")
sp + ggtitle("fourth scatter plot") + stat_smooth(method="lm",se=FALSE)
mlrm2 <- aov(agr_val_ad~agr_ld+agr_crd+rain_fl+agr_emp, data=minty)
