#R Project
library(ggplot2)

#Objective: to see if there are any differences in success rate of iOS
#face recognition if a person has a
#a) sunglasses cover
#b) mask cover
#c) no cover

#Blocks
#strong light, normal light, dim light

#RCBD design

sunglasses = c(0.9,0.8,1,1,0.9,0.9,0.8,1,0.9,0.9,1,1)
mask = c(0.4,0.3,0.4,0,0.3,0.1,0.1,0.4,0.3,0.2,0.3,0.2)
no_cover = c(1,1,0.9,0.8,0.9,0.9,1,0.8,0.9,0.9,1,0.9)
blocks = rep(c("strong_light","normal_light","dim_light"),times=12)

y = c(sunglasses,mask,no_cover)
treatment = as.factor(rep(c("sunglasses","mask","no_cover"),each=12))
blocks = as.factor(blocks)

data = data.frame(y,treatment,blocks)
data

g = lm(y~treatment+blocks)
anova(g)

#p-value (treatment) < 2e-16 
#p-value (treatment) < alpha = 0.05, reject the null hypothesis.

#p-value (blocks) = 0.3442
#p-value (blocks) > alpha = 0.05, do not reject the null hypothesis.

#Conclusion
#At the 5% significance level, there is sufficient evidence to conclude that
#there not all treatment means are different, ie. there are differences between
#the success rate of face recognition when a sunglasses cover, mask cover, and
#no cover are used. Also, at the 5% significance level, there is insufficient
#evidence to conclude that there are any block effects.



#Model Checking

#check normality of residuals
ggplot(g, aes(sample=.resid))+
  geom_qq() + geom_qq_line()+
  labs(title="QQPlot for Residuals", subtitle="iOS Face Recognition")

#The residuals are randomly scattered across the normality line - therefore,
#the assumption of normality of the residuals is satisfied.

#check randomness of residuals
ggplot(g, aes(.fitted,.resid)) +
  geom_point() + geom_hline(yintercept=0, linetype="dashed", color = "red")+
  labs(title="Residuals vs Fitted values", subtitle="iOS Face Recognition", y="Residuals", x="Fitted Values")

ggplot(g, aes(c(treatment),.resid))+
  geom_point() + geom_hline(yintercept=0, linetype="dashed", color = "red")+
  labs(title="Residuals vs Treatment", subtitle="iOS Face Recognition", y="Residuals", x="Treatment")

#There are no patterns in the residual plots - all of the
#points appear to be randomly scattered along y=0. Therefore,
#the assumption of randomness of the residuals is satisfied.


#Boxplots
#Create boxplots to give us an idea of the distributions between the
#different covers and between the different light intensities.

#Boxplot1: light intensity (block)
ggplot(data,aes(x=blocks,y=y))+
  geom_boxplot()+
  labs(title="Success Rate vs Light Intensity",subtitle="iOS Face Recognition", y="Success Rate", x="Light Intensity")
#This reaffirms that there are no block effects at the 5% significance level.


#Boxplot2: different covers
ggplot(data,aes(x=treatment,y=y))+
  geom_boxplot()+
  labs(title="Success Rate vs Type of Cover", subtitle="iOS Face Recognition", y="Success Rate", x="Type of Cover")
#Boxplot2 suggest that the success rate of
#both sunglasses and no_cover is far higher than with mask.
#We are going to confirm this with a formal Tukey multiple comparisons test.



#Multiple Comparisons

#Tukey
z = aov(y~treatment+blocks)
TukeyHSD(z,"treatment")

#At the 5% significance level, there is sufficient evidence to conclude that
#there are differences between mask&sunglasses and no_cover&mask, while there is
#insufficient evidence to conclude that there are any differences between
#no_cover&sunglasses, 
#ie. both sunglasses and no_cover are far away from mask while there are no differences
#between no_cover and sunglasses.


#Conclusion
#At the 5% significance level, there can be assumed no differences between
#the success rate of the iOS face recognition for no_cover and sunglasses while the 
#success rate for mask is different from both no_cover and sunglasses. Furthermore,
#the success rate of the iOS face recognition for both no_cover and sunglasses are
#very high, while the success rate for mask is far lower. 














