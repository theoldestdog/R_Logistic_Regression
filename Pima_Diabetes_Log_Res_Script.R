#### load and verify data
pdd <- read.csv("C:\\Users\\alexd\\R_Files_Data\\Pima_Diabetes\\pdd.csv")
head(pdd)

### convert binaries to factors
is.factor("outcome")
outcome <- as.factor('outcome')
is.factor(outcome)

### histogram plot of age distribution
ggplot(aes(x=age), data = pdd)+
  geom_histogram(binwidth = 1, color='black', fill='#F79420')+
  scale_x_continuous(limits=c(20,90), breaks = seq(20,90,5))+
  xlab("Age")+
  ylab("No of Persons by Age")

### jitter plot of data elements
ggplot(aes(x=glucose, y=outcome), data=pdd)+
  geom_jitter(height=.05, alpha=.1)

### add column to categorize age into groupings
pdd$age_cat <- ifelse(pdd$age < 21, "<21",
                      ifelse((pdd$age >= 21) & (pdd$age <=25), "21-25",
                             ifelse((pdd$age > 25) & (pdd$age <=30), "25-30",
                                    ifelse((pdd$age > 30) & (pdd$age <=35), "30-35",
                                           ifelse((pdd$age > 35) & (pdd$age <=40), "35-40",
                                                  ifelse((pdd$age > 40) & (pdd$age <=50), "40-50",
                                                         ifelse((pdd$age > 50) & (pdd$age <=60), "50-60", ">60")))))))
head(pdd)                                    

#### table w/ numbers in each age category & bar/boxplot age_cat
table(pdd$age_cat)

ggplot(aes(x=age_cat), data=pdd)+
  geom_bar(fill='steelblue')

ggplot(aes(x=age_cat, y=bmi), data=pdd)+
  geom_boxplot()+
  coord_cartesian(ylim = c(0,70))

### first log reg using all variables
model_1 <- glm(outcome ~ pregnancies + glucose + b_p + skin_fold + insulin + bmi + age, data=pdd, family = binomial)
summary(model_1)

### plot result
ggplot(aes(x=pregnancies + glucose + b_p + skin_fold + insulin + bmi + age, y=outcome), data=pdd)+
  geom_jitter(height=.05, alpha=.1)+
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se=FALSE)

##### pchisq on deviances to test how good model is
1-pchisq(993.48, 767, lower.tail = TRUE, log.p = FALSE)

1-pchisq(733.78, 760, lower.tail = TRUE, log.p = FALSE)

diff <- pchisq(993.48-733.78, 767-760, lower.tail = FALSE)
diff

##### test for co-linearity of variable, < 5 is good, smaller the better
library(car)
car::vif(model_1)

### reduce the model by removing least effective variables and test result skin_fold, insulin and age
model_2 <- glm(outcome ~ pregnancies + glucose + b_p + bmi, data=pdd, family = binomial)
summary(model_2)

### pchisq on residual
1-pchisq(738.43, 763, lower.tail = TRUE, log.p = FALSE)

### plot
ggplot(aes(x=pregnancies + glucose + b_p + bmi, y=outcome), data=pdd)+
  geom_jitter(height=.05, alpha=.1)+
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se=FALSE)

### check co-linearity
car::vif(model_2)
