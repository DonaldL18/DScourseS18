library(mice)
library(stargazer)
library(MixedDataImpute)
library(imputeR)





wages <- read.csv("~/Documents/GitHub/DScourseS18/ModelingOptimization/wages.csv")
wages2<- wages
mice_wages<- wages

#drop missing obs from hgc and tenure 



wages<- na.omit(wages, col= wages$hgc)
wages<- na.omit(wages, col= wages$tenure)

sum(is.na(wages$hgc))

# View with Stargazer

stargazer(wages,type = "latex")

sum(is.na(wages$logwage))
summary(wages)

#After looking through the dataset it seems to be MNAR. Some people may not feel comfortable in revealing their incomes
# between charateristics and missingness there are 560 missing obs out of 2246. 

#assume MAR 
complete.cases(wages$logwage)
sum(is.na(wages$logwage))

model1<- lm(logwage~. , data = wages)
stargazer(model1,type = "latex")
#mean imputation 
wages2$logwage<- guess(wages2$logwage, type = "mean")


model2<- lm(logwage~. , data = wages)
stargazer(model2,type = "latex")
#Mice impute with predicitive mean matching 

mice_wages<- mice(mice_wages, seed = 12345)


fit <- with(mice_wages, lm(logwage~ hgc+college+tenure+tenure^2+age+married))
round(summary(pool(fit)))
