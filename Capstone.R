getwd()
setwd("/Users/jacobmunroe/Desktop/R")
library(read)
library(recipes)
library(caret)
library(tidyverse)
library(dplyr)

students <- read_csv("eMPowerME_3years_through_201718 (1).csv") %>% 
  janitor::clean_names()


#select only the columns that are useful

students_clean <-  students %>% 
  select(frl, school_name, grade, gender, race, el_status, sped, result) %>% 
  mutate(el_status = case_when(
    el_status == "EL" ~ 1,
    TRUE ~ 0
  ),
  sped = ifelse(sped == "Y", 1, 0),
  gender = ifelse(gender == "F", 1, 0),
  school_name = as.factor(school_name),
  race = as.factor(race)) 

students_train <- filter(students_clean, frl != "NULL")
students_test <- filter(students_clean, frl == "NULL")

students_recipe <- recipe(frl ~ ., data = students_train) %>% 
  step_dummy(all_nominal(), -frl)

trained_rec <- prep(students_recipe, training = students_train)

baked_train <- bake(trained_rec, students_train)
baked_test <- bake(trained_rec, students_test)

mod1 <- train(frl ~ .,
              data = baked_train,
              method = "rf",
              trControl = trainControl(
                method = "cv",
                number = 10,
                verboseIter = TRUE
              ))
mod1
mod2 <- train(frl ~ .,
              data = baked_train
              method = "rf"
              trControl = trainControl(verboseIter = TRUE))

test_pred <-  predict(mod1, baked_test)
bombined <- cbind(test_pred, baked_test) %>% 
  as_tibble()
bombined %>% group_by(test_pred) %>% summarise(count = n(),
                                               mean = mean(result))
View(bombined)
write.csv(bombined, file = "bombined.csv")
summary(students)

Buh <- students %>% group_by(frl) %>% summarise(count <- length(result),
                                                Test_Result <- mean(result))
Buh
View(Buh)
Race <- students %>% group_by(Race) %>% summarise(grade <- mean(grade, na.rm = TRUE),
                                                Test_Result <- mean(result))
View(Race)
RaceXgrade <- students %>% group_by(Race, grade) %>% summarise(grade <- mean(grade, na.rm = TRUE),
                                                  Test_Result <- mean(result))
View(RaceXgrade)
grade <- students %>% group_by (grade) %>% summarise(grade <- mean(grade, na.rm = TRUE),
                                                               Test_Result <- mean(result))
View(grade)
dist(students)

sped <- students %>% group_by (sped) %>% summarise(grade <- mean(grade, na.rm = TRUE),
                                                     Test_Result <- mean(result))
View(sped)
dist(sped)

spedxfrl <- students %>% group_by (frl, sped) %>% summarise(Test_Result <- mean(result, na.rm = TRUE),
                                                   Sum <- length(frl))
View(spedxfrl)
dist(sped)
?gather

anova <- aov(result ~ frl, data = students)
TukeyHSD(anova)

fatadrame <- students %>% group_by(gender) %>% summarise(avg <- mean(result, na.rm = TRUE),
                                                          sd <- sd(result, na.rm = TRUE),
                                                          count <- length(result))

fatadrame2 <- students %>% group_by(sped) %>% summarise(avg <- mean(result, na.rm = TRUE),
                                                         sd <- sd(result, na.rm = TRUE),
                                                         count <- length(result))
anova2 <- aov(result ~ school_name, data = students)

anova3 <- aov(result ~ test_date, data = buh1)

summary(anova3)
TukeyHSD(anova3)

anova4 <- aov(result ~ test_date, data = buh2)

summary(anova4)
TukeyHSD(anova4)

fatadrame
fatadrame2
summary(anova2)
TukeyHSD(anova2)

mean(students$result)

students2 <- students

Anova <- aov(result ~ frl, data = students2)

anova2 <- aov(sped ~ frl, data = students2)

fatadrame
fatadrame2
summary(anova2)
TukeyHSD(anova)

twenty16 <- subset(students, test_date == "5/1/2016 0:00:00")
twenty17 <- subset(students, test_date == "5/1/2017 0:00:00")
twenty18 <- subset(students, test_date == "5/1/2018 0:00:00")
unique(twenty16$test_date)


View(years)
anova_race_ela <- aov(result ~ test_date, data = students)

TukeyHSD(anova_race_ela)
mean(twenty16$result)
mean(twenty17$result)
mean(twenty18$result)

twenty16 %>%group_by(race) %>%summarise(mean = var(result))
twenty17 %>%group_by(race) %>%summarise(mean = var(result))
twenty18 %>%group_by(race) %>%summarise(mean = var(result))
twenty16 %>%group_by(frl) %>%summarise(mean = var(result))
twenty17 %>%group_by(frl) %>%summarise(mean = var(result))
twenty18 %>%group_by(frl) %>%summarise(mean = var(result))
unique(students$test_name)


buh1 <- subset(students, test_name == "MEA_eMPowerME_ELA")
unique(buh9$gender)
buh2 <- subset(students, test_name == "MEA_eMPowerME_Math")
bulbasaur.attack <- mean(buh2$result)
mi.attack <- mean(buh1$result)
siz.poke <- nrow(buh1)
sd.al.attack <- sqrt((1/siz.poke[1])*sum((buh1$result-mi.attack)^2))
Z.attack.bulbasaur <- (bulbasaur.attack - mi.attack)/(sd.al.attack)
pnorm(Z.attack.bulbasaur) 

var.test(result ~ gender, data = students)
var.test(result ~ el_status, data = students_clean)
var.test(result ~ sped, data = students_clean)
var.test(result ~ test_name, data = students)

buh9 <- subset(students, gender == "M")
unique(buh9$gender)
buh10 <- subset(students, gender == "F")
pikachu.attack <- mean(buh10$result)
mu.attack <- mean(buh9$result)
size.poke <- nrow(buh9)
sd.all.attack <- sqrt((1/size.poke[1])*sum((buh9$result-mu.attack)^2))
Z.attack.pikachu <- (pikachu.attack - mu.attack)/(sd.all.attack)
pnorm(Z.attack.pikachu) 


anova_school <- aov(result ~ school_name, data = students_clean)
summary(anova_school)
TukeyHSD(anova_school)
anova(anova_race)


anova_race <- aov(result ~ race, data = students_clean)
summary(anova_race)
TukeyHSD(anova_race)
anova(anova_race)

which(colnames(students2) == "gender")

which(colnames(students) == "result")

which(colnames(students) == "test_date")
unique(students$test_date)
chisq.test(table(students_clean$race, students_clean$frl))
chisq.test(table(students_clean$race, students_clean$school_name))
chisq.test(table(students_clean$el_status, students_clean$frl))
tulip <- c(1457, 1495, 1547)
chisq.test(tulip, p = c(1/3, 1/3, 1/3))
lip <- c(945, 938, 791)
res <- chisq.test(lip, p = c(1/3, 1/3, 1/3))
res$expected
x = table(students$test_date, students$result)
x
table(students$school_name, students$result)
x = t(t(ageByRace) / colSums(ageByRace))


var.test(result ~ test_date, students, 
         alternative = "two.sided")
plot(twenty16$result, twenty16$frl)
plot(twenty16$result, twenty16$race)

library(ggplot2)
View(student_regress)
student_regress <-students
student_regress$test_date[student_regress$test_date == "5/1/2016 0:00:00"] <- 1
student_regress$test_date[student_regress$test_date == "5/1/2017 0:00:00"] <- 2
student_regress$test_date[student_regress$test_date == "5/1/2018 0:00:00"] <- 3
plot(student_regress$test_date, student_regress$result)
plot(student_regress$result, student_regress$test_date)
bert <- lm(result ~ test_date, student_regress)
bert <- lm(test_date ~ result, student_regress)
abline(bert)
summary(bert)

sapply(students,function(x) sum(is.na(x)))
sapply(students, function(x) length(unique(x)))
write.csv(students_clean, file = "students_clean.csv")
train <- students_el[1:15183,]
nrow(students)
test <- students_el[15184:16871,]
model <- glm(el_status ~.,family=binomial(link='logit'),data=train)
plot(el_status ~.,family=binomial,data=train)
students_el$result <- as.numeric(students_el$result)
students_el <-  students %>% 
  select(frl, school_name, grade, gender, race, el_status, sped, result) %>% 
  mutate(el_status = case_when(
    el_status == "EL" ~ 1,
    TRUE ~ 0))
summary(model)
View(model)
plot(model)
anova(model, test="Chisq")
library(pscl)
pR2(model)
colnames(test)
fitted.results <- predict(model,newdata=subset(test,select=c(1,2,3,4,5,6,7,8)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$el_status)
print(paste('Accuracy',1-misClasificError))

ageByRace = table(students$race, students$frl)
ageByRace
ageByRace = t(t(ageByRace) / colSums(ageByRace))
ageByRace
# Sanity Check:
colSums(ageByRace.)


students_recipe2 <- recipe(result ~ ., data = train) %>% 
  step_dummy(all_nominal(), -result)

trained_rec2 <- prep(students_recipe2, training = train)

baked_train2 <- bake(trained_rec2, train)
baked_test2 <- bake(trained_rec2, test)

??train
mod2 <- train(result ~ .,
              data = baked_train2,
              method = "cforest",
              trControl = trainControl(
                method = "cv",
                number = 10,
                verboseIter = TRUE
              ))

test_pred2 <-  predict(mod1, baked_test)
bombined <- cbind(test_pred, baked_test) %>% 
  as_tibble()
bombined %>% group_by(test_pred) %>% summarise(count = n(),
                                               mean = mean(result))