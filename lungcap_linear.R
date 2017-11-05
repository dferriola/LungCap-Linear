library(ggplot2)
library(rgl)
lungcap=read.csv(file.choose(),header=T)
summary(lungcap)
#visualizing 2 numerical data points
lm(lungcap$LungCap~lungcap$Age, lungcap)
##Call:
#  lm(formula = lungcap$LungCap ~ lungcap$Age, data = lungcap)

#Coefficients:
#  (Intercept)  lungcap$Age  
#   1.1469       0.5448  
model1=lm(lungcap$LungCap~lungcap$Age, lungcap)
summary(model1)
#Call:
#lm(formula = lungcap$LungCap ~ lungcap$Age, data = lungcap)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-4.7799 -1.0203 -0.0005  0.9789  4.2650 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.14686    0.18353   6.249 7.06e-10 ***
#  lungcap$Age  0.54485    0.01416  38.476  < 2e-16 ***
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 1.526 on 723 degrees of freedom
#Multiple R-squared:  0.6719,	Adjusted R-squared:  0.6714 
#F-statistic:  1480 on 1 and 723 DF,  p-value: < 2.2e-16
plot(lungcap)
cor(lungcap$LungCap, lungcap$Age)
#[1] 0.8196749
cor(lungcap$LungCap, lungcap$Height)
#[1] 0.9121873
model2=lm(lungcap$LungCap~lungcap$Smoke)
summary(model2)
#Call:
#lm(formula = lungcap$LungCap ~ lungcap$Smoke)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-7.2632 -1.7202  0.1048  1.9048  6.9048 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept)        7.7702     0.1041   74.64   <2e-16
#lungcap$Smokeyes   0.8753     0.3194    2.74   0.0063

#(Intercept)      ***
#  lungcap$Smokeyes ** 
#  ---
#  Signif. codes:  
#  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 2.65 on 723 degrees of freedom
#Multiple R-squared:  0.01028,	Adjusted R-squared:  0.008908 
#F-statistic: 7.507 on 1 and 723 DF,  p-value: 0.006297
plot(lungcap$LungCap, lungcap$Height)

ggplot(lungcap, aes(y=lungcap$LungCap, x=lungcap$Age, colour=lungcap$Smoke))+geom_point()
lungcap_height=ggplot(lungcap, aes(y=lungcap$LungCap, x=lungcap$Height))+
  geom_point(color="firebrick", alpha=.75)+
  scale_x_continuous("Age")+
  scale_y_continuous("Lung Capacity")+ggtitle('Age to Lung Capacity')
library(rgl)
plot3d(lungcap$LungCap, lungcap$Age, lungcap$Height, type='s', size=0.75, lit=F)
ggplot(lungcap, aes(y=lungcap$LungCap, x=lungcap$Height, colour=lungcap$Smoke))+
  geom_point(alpha=.75)+
  scale_x_continuous("Height")+
  scale_y_continuous("Lung Capacity")+ggtitle('Height to Lung Capacity')+
  scale_colour_brewer(palette="Dark2")+stat_smooth(method=lm, se=F, colour="grey30")