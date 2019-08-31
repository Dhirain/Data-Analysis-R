#----------------------------------------------------------
# Reset R's brain
#----------------------------------------------------------
rm(list=ls())

#----------------------------------------------------------
# Reset graphic device
# As long as there is any dev open (exept "null device") 
# close the active one!
# Caution: closes all open plots!!!!
#----------------------------------------------------------
while(!is.null(dev.list()))
{
  dev.off()
}
install.packages('gridExtra')
install.packages("ISLR")
install.packages("ggplot2")
require(ISLR)
library(ISLR)
library(ggplot2)
require(ggplot2)
library(gridExtra)
require(gridExtra)

attach(Wage)
assessment_dataframe <- Wage[sample(nrow(Wage), 3000), ]

#----------------------------------------------------------
# Extracting requried data
#----------------------------------------------------------
employment_data <- data.frame(age,education,year,wage)
head(employment_data)
str(employment_data)
summary(employment_data)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Step 0:Exploratory Data
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#for centre align of graph title
theme_update(plot.title = element_text(hjust = 0.5))

#histogram of wage
wage_plot <- ggplot(data = employment_data, aes(x=wage))+
  geom_histogram(fill = "sky blue" ,color = "black") +
  ggtitle("Wage distribution") 

#histogram of age
age_plot <- ggplot(data = employment_data, aes(x=age))+
  geom_histogram(fill = "orange2" ,color = "black", binwidth = 2) +
  ggtitle("Age distribution") 

#education distribution
education_plot <- ggplot(data = employment_data, aes(education, fill = education)) +
  geom_bar(color = 'black')+
  scale_x_discrete(labels = c ('1','2','3','4','5')) +
  ggtitle("Education distribution")

#Year distribution
year_plot <- ggplot(data = employment_data, aes(x = factor(year), fill = factor(year))) +
  geom_bar(color = 'black')+
  xlab('Calender year')+
  guides(fill=guide_legend(title="Year"))+
  ggtitle("Calender year distribution")

figure <- grid.arrange(  wage_plot , age_plot , education_plot , year_plot, ncol= 2, nrow = 2)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#step 1: Is there any relationship between the Predictors and the Response?
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#F-statistic test
fit <- lm(wage ~ . , data = employment_data)
summary(fit)
#Multiple R-squared:  0.2619,	Adjusted R-squared:  0.2604 
#F-statistic:   177 on 6 and 2993 DF,  p-value: < 2.2e-16

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Step 2: Find inidividual relationship
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
ggpairs(employment_data)

#convert education to numeric for getting correlation matrix
numeric_employment_data <- employment_data
numeric_employment_data$education <- as.numeric(numeric_employment_data$education)

#Find the correlation matrix
cor(numeric_employment_data,use="pairwise.complete.obs")
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#individual relationship
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#====================
#wage vs age
#====================
fitAge <- lm(wage ~ age)
summary(fitAge)
#Multiple R-squared:  0.03827,	Adjusted R-squared:  0.03795
#F-statistic: 119.3 on 1 and 2998 DF,  p-value: < 2.2e-16
ggplot(data = employment_data , aes (x = age , y = wage)) +
  geom_point() + geom_smooth(method = "lm") + ggtitle("Wage v age")

#====================
#education vs age
#====================
fitEducation <-lm(wage ~ education, data = employment_data)
summary(fitEducation)
#Multiple R-squared:  0.2348,	Adjusted R-squared:  0.2338
#F-statistic: 229.8 on 4 and 2995 DF,  p-value: < 2.2e-16
ggplot(data = employment_data,  aes(x = education, y = wage, color = education)) +
  geom_boxplot() 

#====================
#year vs age
#====================
fitYear <-lm(wage ~ year, data = employment_data)
summary(fitYear)
#Multiple R-squared:  0.005442,	Adjusted R-squared:  0.003448
#F-statistic: 12.94 on 1 and 2998 DF,  p-value: 0.0003277
scatter.smooth(x = employment_data$year , y =employment_data$wage, xlab = 'Age' , ylab = 'Wage', main = "Calender year vs Wage", col = "blue")


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Step 3: Given a certain age/education/calendar year, can we predict wage with a high level of accuracy?
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
predict(fitAge, data.frame(age = 25) , interval = "confidence")
predict(fitEducation, data.frame(education = '4. College Grad') , interval = "confidence")
predict(fitYear, data.frame(year = 2004) , interval = "confidence")

#prediction interval vs confidence interval 
pred<- predict(lm(wage~age,data=numeric_employment_data), interval="prediction")
combined_df <- cbind(numeric_employment_data, pred)
ggplot(combined_df, aes(age,wage))+
  geom_point() +
  geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y=upr), color = "red", linetype = "dashed")+
  geom_smooth(method=loess, se=TRUE)

pred<- predict(lm(wage~education,data=numeric_employment_data), interval="prediction")
combined_df <- cbind(numeric_employment_data, pred)
ggplot(combined_df, aes(education,wage))+
  geom_point() +
  geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y=upr), color = "red", linetype = "dashed")+
  geom_smooth(method=loess, se=TRUE)

pred<- predict(lm(wage~year,data=numeric_employment_data), interval="prediction")
combined_df <- cbind(numeric_employment_data, pred)
ggplot(combined_df, aes(year,wage))+
  geom_point() +
  geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y=upr), color = "red", linetype = "dashed")+
  geom_smooth(method=loess, se=TRUE)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#step 4: Which factors contribute to wage
#++++++++++++++++++++++++++++++++++++++++++++++++++++++
#P-value and R-square value for 3 independent variable
fit <- lm(wage ~ . , data = employment_data)
summary(fit)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#step 5: Is the relationship linear
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#====================================
#Age and Wage
#===================================
#degree 1
model1 <- lm(wage ~ age)
#degree 2
model2 <- lm(wage ~ poly(age,2))
predictAge2 <- predict(model2)
#degree 3
model3 <- lm(wage ~ poly(age,3))
predictAge3 <- predict(model3)
#plot on same graph
plot(x = employment_data$age , y =employment_data$wage, xlab = 'Age' , ylab = 'Wage', main = "Wage vs age polynomial comparision" )
abline(model1, lwd = 3 , col = 'red')
lines(smooth.spline(age,predictAge2),col = 'blue', lwd = 3)
lines(smooth.spline(age,predictAge3),col = 'green', lwd = 3)
legend(60,330,legend = c( "model1: linear", "model2: poly x^2","model3: poly x^3"), col = c("red","blue","green"),lwd = 3)
summary(model1)
summary(model2)
summary(model3)
anova(model1,model2,model3)

#====================================
#Education and Wage
#===================================
plot(x = employment_data$education , y =employment_data$wage, xlab = 'Education' , ylab = 'Wage', main = "Wage vs education" )
abline(fitEducation, lwd = 3 , col = 'red')
summary(fitEducation)

#=====================================
#Year and Wage
#====================================
model_year_0 <- lm(wage~ poly(age,2) +  education)
model_year_1 <- lm(wage~ poly(age,2) +  education + year)
model_year_2 <- lm(wage~ poly(age,2) +  education + poly(year,2))
anova(model_year_0,model_year_1,model_year_2)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#step 6 Interaction effect
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#=========================
#Interaction between Year and Age

intercation1 <-lm(data = employment_data , wage ~ year* age)
summary(intercation1)

#=========================
#Interaction between Year and education

intercation2 <-lm(data = employment_data , wage ~ year* education)
summary(intercation2)

#=========================
#Interaction between Year and Age

intercation3 <-lm(data = employment_data , wage ~ age * education)
summary(intercation3)

#==========================
#Intercation between age,year and education

intercation4 <-lm(data = employment_data , wage ~ age * education * year)
summary(intercation4)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Result
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

age_plot = ggplot(data = employment_data , aes (x = age , y = wage)) +
  geom_point() + geom_smooth(method ="loess")+
  ggtitle('Age v Wage')

 education_plot <- ggplot(data = employment_data, aes(x= education, y = wage,color = education)) +
  geom_boxplot()+
  scale_x_discrete(labels = c ('1','2','3','4','5')) +
  ggtitle("Education vs Wage")

year_plot = ggplot(data = employment_data , aes (x = year , y = wage)) +
  geom_point() + geom_smooth(method ="lm")+
  ggtitle('Year vs Wage')

figure <- grid.arrange(age_plot , education_plot , year_plot, ncol= 2, nrow = 2)

ggplot(data = employment_data , aes (x = age , y = wage,color=education)) +
  geom_point() + geom_smooth(method ="lm")+
  ggtitle('Age vs Wage with different education')
#End
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++