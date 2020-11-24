#library
library(moments)
library(psych)
install.packages("ggplot2")
library(ggplot2)
#user defined function
getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}

rangevalue <- function(x){max(x)-min(x)}
#dataset
slhk <- read.csv("D:\\EXcelr\\assignments\\liearmodel\\assignment\\salary\\Salary_Data.csv")
View(slhk)
attach(slhk)
#1st bussiness moment
mean(YearsExperience)#5.31
median(YearsExperience)#4.7
getmode(YearsExperience)#3.2

mean(Salary)#76003
median(Salary)#65237
getmode(Salary)#39343

#2nd bussiness mpment deccission
var(YearsExperience)#8.054
sd(YearsExperience)#2.83
rangevalue(YearsExperience)#9.4

var(Salary)#751550960
sd(Salary)#27414.43
rangevalue(Salary)#84660
#3rd bussiness moment deccission
skewness(YearsExperience)#0.36
skewness(Salary)#0.34
#4th bussiness moment deccission
kurtosis(YearsExperience)#1.95
kurtosis(Salary)#1.7
#describe
describe(slhk)

#qqplot
qqnorm(YearsExperience)
qqline(YearsExperience)

qqnorm(Salary)
qqline(Salary)
#grapical represntation
par(mfrow=c(2,2))
boxplot(YearsExperience,horizontal = TRUE,main = "Boxplot_yearsexperience",col = "blue")
boxplot(Salary,horizontal = TRUE,main = "Boxplot_salary",col = "red")

hist(YearsExperience,xlab = "yearsexperience",col = "blue")
hist(Salary,xlab = "salary",col = "red")
#standartisation
#slhk_std <- scale(slhk)
#slhk_frame <- as.data.frame(slhk_std)
#class(slhk_frame)
#View(slhk_frame)
#EDA

#mean(slhk_frame$YearsExperience)
#median(slhk_frame$YearsExperience)
#getmode(slhk_frame$YearsExperience)

#mean(slhk_frame$Salary)
#median(slhk_frame$Salary)
#getmode(slhk_frame$Salary)
#changeing columns name
#colnames(slhk_frame) <- c("expe","sal")
#attach(slhk_frame)
#linearmodel
par(mfrow=c(1,1))
plot(YearsExperience,Salary)
cor(YearsExperience,Salary)#0.98
model_1 <- lm(Salary~YearsExperience)
summary(model_1)#Rsqure=0.96
#resudial
sum(model_1$residuals)#-7.844e-12
sqrt(sum(model_1$residuals^2)/nrow(slhk))#5592.04
predict(model_1,level = 0.95)
confint(model_1, interval = "predict")      
#ggplot
ggplot(data = slhk,aes(x=YearsExperience,y=Salary))=geom_line(data=slhk,aes(x=YearsExperience,y=Salary),col ="blue")+geom_point(col ="red")


#model2
plot(Salary,log(YearsExperience),main = "Scator plot of salary hike")
cor(Salary,log(YearsExperience))
model2 <- lm(Salary~log(YearsExperience))
summary(model2)
sum(model2$residuals)
sqrt(sum(model2$residuals^2)/nrow(slhk))
predict(model2,level = 0.95)
confint(model2, interval = "predict")


#model_1 is relatively better model.




