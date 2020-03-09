library(tidyverse)
library(readstata13)
library(sampleSelection)
library(textclean)
library(ggplot2)
library(stargazer)

# Загружаем базу данных
data <- read.dta13("Working file 2015.dta")
# Выбираем колонны, с которыми будем работать
cols <- c('regionx', 'ixwagelm', 'ixhourlm', 'ixmarist', 'ixwrknow', 'ixnkids', 'ixnykids', 'ixilpjb8', 'NCAT1_X', 'ixdisabl', 'ixhiedul', 'ixpriind', 'ixgender', 'ixtgempy', 'ixbirthy')
data_cleaning <- data %>% select(cols)
# Обзор переменных
summary(data_cleaning)

# Корректируем пустые значения
for (col in cols) {
  data_cleaning[, col][data_cleaning[, col] %in% c(99999990:99999999)] <- NaN
}
# Удаляем выбросы и исправляем пустые значения
data_cleaning <- data_cleaning[-which(data_cleaning$ixwagelm %in% c(80000:450000)), ]
data_cleaning[, 'ixwagelm'][data_cleaning[, 'ixwagelm'] %in% c(997, 998)] <- NaN
# Дропнем 597 пустых строчек с пустым образованием
data_cleaning <- data_cleaning %>% drop_na('ixhiedul')

# Готовим переменные
data_converted <- data_cleaning %>% mutate(
  #возраст и его квадрат
  age = 2015 - ixbirthy,
  age2 = age*age,
  #опыт и его квадрат
  experience = ixtgempy,
  experience2 = ixtgempy*ixtgempy,
  #референтная группа - неполное среднее и пту без среднего
  #среднее полное
  educ1 = case_when(ixhiedul == 2 ~ 1, TRUE ~ 0),
  #пту/коллелд после среднего
  educ2 = case_when(ixhiedul %in% c(3, 5, 6) ~ 1, TRUE ~ 0),
  #бакалавриат
  educ3 = case_when(ixhiedul == 11 ~ 1, TRUE ~ 0),
  #магистратура или специалитет
  educ4 = case_when(ixhiedul %in% c(10, 12) ~ 1, TRUE ~ 0),
  #кандидат наук, ординатор, доктор
  educ5 = case_when(ixhiedul %in% c(8, 13, 14) ~ 1, TRUE ~ 0),
  #пол (женщина - 1, мужчина - 0)
  female = case_when(ixgender == 2 ~ 1, ixgender == 1 ~ 0),
  #женат(а) или не женат(а)
  married = case_when(ixmarist %in% c(2, 3, 6) ~ 1, ixmarist %in% c(1, 4, 5) ~ 0),
  #женатая женщина
  femarried = female * married,
  #проблемы со здоровьем
  disabl = case_when(ixdisabl %in% c(1, 5) ~ 1, ixdisabl %in% c(2, 6) ~ 0),
  #есть главная работа или нет
  work = case_when(ixwrknow == 1 ~ 1, ixwrknow %in% c(2:5) ~ 0),
  #зарплата, почасовая зп, логзарплата зп, логпочасовая зп для них
  wage = case_when(work == 1 & !is.na(ixwagelm) ~ ixwagelm),
  hourwage = case_when(ixhourlm > 0 ~ wage / ixhourlm),
  lnwage = case_when(wage > 1 ~ log(wage)),
  lnhourwage = case_when(hourwage > 1 ~ log(hourwage)),
  #число детей
  nkids = case_when(ixnkids < 100 ~ ixnkids),
  #наличие детей 0-6 лет
  kids06 = case_when(NCAT1_X > 0 ~ 1, TRUE ~ 0),
  profession = ixilpjb8 %/% 1000
  )

# Ограничение на возраст и небольшая чистка
data_converted <- data_converted[-which(data_converted$age %in% c(72:102)), ]
data_converted <- data_converted[-which(is.na(data_converted$ixhourlm) & !is.na(data_converted$wage)), ]
data_converted <- data_converted[-which(!is.na(data_converted$ixhourlm) & is.na(data_converted$wage)), ]
data_converted <- data_converted[-which(is.na(data_converted$ixwagelm) & data_converted$work == 1), ]

data_prepared <- data_converted %>% select(wage, lnwage, hourwage, lnhourwage, work, age, age2, experience, experience2, educ1, educ2, educ3, educ4, educ5, female, married, femarried, nkids, kids06, disabl)
# Обзор переменных
summary(data_prepared)

# Графики до
ggplot(data_prepared, aes(x=wage, fill=as.factor(female))) + geom_density(alpha=0.5, color='black')
ggplot(data_prepared, aes(x=wage, fill=as.factor(disabl))) + geom_density(alpha=0.5, color='black')
ggplot(data_prepared, aes(x=wage, fill=as.factor(female))) + geom_histogram(bins=20, colour="black") + facet_grid(female ~ .)
ggplot(data_prepared, aes(x=wage, fill=as.factor(disabl))) + geom_histogram(bins=20, colour="black") + facet_grid(female ~ .)
ggplot(data_prepared, aes(x=as.factor(female), y=wage)) + geom_boxplot()
ggplot(data_prepared, aes(x=as.factor(disabl), y=wage)) + geom_boxplot()

#Вводим доп ограничение
data_final <- data_prepared[-which(data_prepared$wage %in% c(50000:80000)), ]

#Графики после
ggplot(data_final, aes(x=wage, fill=as.factor(female))) + geom_density(alpha=0.5, color='black')
ggplot(data_final, aes(x=wage, fill=as.factor(disabl))) + geom_density(alpha=0.5, color='black')
ggplot(data_final, aes(x=wage, fill=as.factor(female))) + geom_histogram(bins=20, colour="black") + facet_grid(female ~ .)
ggplot(data_final, aes(x=wage, fill=as.factor(disabl))) + geom_histogram(bins=20, colour="black") + facet_grid(female ~ .)
ggplot(data_final, aes(x=as.factor(female), y=wage)) + geom_boxplot()
ggplot(data_final, aes(x=as.factor(disabl), y=wage)) + geom_boxplot()


special <- data_final %>% mutate(education = case_when(educ1==1 ~ 1, educ2==1 ~ 2, educ3==1 ~ 3, educ4==1 ~ 4, educ5==1 ~ 5, TRUE ~ 0))

# Полигон по образованию
ggplot(special, aes(x = factor(education))) + 
  geom_bar() + 
  theme_bw() + 
  labs(title = "Распределение работников по уровню образования", x = "Максимальный уровень образования", caption = "n = 10721")

# Показываю, почему мало бакалавров - выборка довольно возрастная, реформа, после бакалавриата идут в магистратуру
ggplot(data_final, aes(age)) + 
  geom_histogram() + 
  geom_vline(xintercept = 26, color = "red") + 
  theme_bw()

# Опыт
ggplot(data_final, aes(x=experience)) + 
  geom_histogram() + 
  theme_bw() + 
  labs(title = "Распределение работниковпо опыту работы", x = "Опыт работы", caption = "n = 10721")

# Модели
#опыт на зарплату, возраст на выбор
model1 <- selection(selection = work ~ age + age2 + educ1 + educ2 + educ3 + educ4 + educ5 + female + femarried + nkids + kids06,
                    outcome = lnwage ~ experience + experience2 + educ1 + educ2 + educ3 + educ4 + educ5 + female,
                    data = data_final, method='2step')
summary(model1)

#только опыт (без квадрата) на зарплату
model2 <- selection(selection = work ~ experience + experience2 + educ1 + educ2 + educ3 + educ4 + educ5 + female + femarried + nkids + kids06,
                    outcome = lnwage ~ experience + educ1 + educ2 + educ3 + educ4 + educ5 + female,
                    data = data_final, method='2step')
summary(model2)

#опыт на зарплату, возраст, возраст2 на решение
model3 <- selection(selection = work ~ age + age2 + educ1 + educ2 + educ3 + educ4 + educ5 + female + femarried + nkids + kids06,
                    outcome = lnwage ~ experience + educ1 + educ2 + educ3 + educ4 + educ5 + female + disabl,
                    data = data_final, method='2step')
summary(model3)

#везде возраст и возраст2
model4 <- selection(selection = work ~ age + age2 + educ1 + educ2 + educ3 + educ4 + educ5 + female + femarried + kids06,
                    outcome = lnwage ~ age + age2 + educ1 + educ2 + educ3 + educ4 + educ5 + female,
                    data = data_final, method='2step')
summary(model4)

#опыт опыт2 во всех
model5 <- selection(selection = work ~ experience + experience2 + educ1 + educ2 + educ3 + educ4 + educ5 + female + femarried + kids06 + disabl,
                    outcome = lnwage ~ experience + experience2 + educ1 + educ2 + educ3 + educ4 + educ5 + female + disabl,
                    data = data_final, method='2step')
summary(model5)

# Код для красивой визуализации итогов моделей
stargazer(model1, model2, model3, model4, model5, type="text",
          dep.var.labels=c("lnwage"), selection.equation = FALSE,
          out="outcome1.txt")

stargazer(model1, model2, model3, model4, model5, type="text",
          dep.var.labels=c("work"), selection.equation = TRUE,
          out="selection1.txt")

#Модели
#опыт на зарплату, возраст на выбор

#только опыт (без квадрата) на зарплату
model11 <- selection(selection = work ~ experience + educ1 + educ2 + educ3 + educ4 + educ5 + female + femarried+ married + nkids + kids06 + disabl,
                    outcome = lnwage ~ experience + educ1 + educ2 + educ3 + educ4 + educ5 + female+diasbl,
                    data = data_final, method='2step')
summary(model11)

#опыт на зарплату, возраст, возраст2 на решение
model12 <- selection(selection = work ~ age + educ1 + educ2 + educ3 + educ4 + educ5 + female + femarried + nkids + kids06+disabl+married,
                    outcome = lnwage ~ age + educ1 + educ2 + educ3 + educ4 + educ5 + female + disabl,
                    data = data_final, method='2step')
summary(model12)

#везде возраст и возраст2
model13 <- selection(selection = work ~ age + age2 + educ1 + educ2 + educ3 + educ4 + educ5 + female + femarried + kids06+disabl+married,
                    outcome = lnwage ~ age + age2 + educ1 + educ2 + educ3 + educ4 + educ5 + female + disabl,
                    data = data_final, method='2step')
summary(model13)

#опыт опыт2 во всех
model14 <- selection(selection = work ~ experience + experience2 + educ1 + educ2 + educ3 + educ4 + educ5 + female + femarried + kids06 + disabl+married,
                    outcome = lnwage ~ experience + experience2 + educ1 + educ2 + educ3 + educ4 + educ5 + female,
                    data = data_final, method='2step')
summary(model14)




#рубрика ээээксперименты
data_converted1 <- data_cleaning %>% mutate(
  #возраст и его квадрат
  age = 2015 - ixbirthy,
  age2 = age*age,
  #опыт и его квадрат
  experience = ixtgempy,
  experience2 = ixtgempy*ixtgempy,
  #референтная группа - неполное среднее,пту без среднего, профессиональные курсы
  #среднее полное
  educ1 = case_when(ixhiedul == 2 ~ 1, TRUE ~ 0),
  #пту со средним образованием, техническое училище
  educ2 = case_when(ixhiedul == 5 ~ 1, TRUE ~ 0),
  #колледж, техникум
  educ3 = case_when(ixhiedul == 6 ~ 1, TRUE ~ 0),
  #бакалавриат, магистратура,специалитет, кандидат наук, ординатор, доктор
  educ4 = case_when(ixhiedul %in% c(8, 10, 11, 12, 13, 14) ~ 1, TRUE ~ 0),
  #пол (женщина - 1, мужчина - 0)
  female = case_when(ixgender == 2 ~ 1, ixgender == 1 ~ 0),
  #женат(а) или не женат(а)
  married = case_when(ixmarist %in% c(2, 3, 6) ~ 1, ixmarist %in% c(1, 4, 5) ~ 0),
  #женатая женщина
  femarried = female * married,
  #проблемы со здоровьем
  disabl = case_when(ixdisabl %in% c(1, 5) ~ 1, ixdisabl %in% c(2, 6) ~ 0),
  #есть главная работа или нет
  work = case_when(ixwrknow == 1 ~ 1, ixwrknow %in% c(2:5) ~ 0),
  #зарплата, почасовая зп, логзарплата зп, логпочасовая зп для них
  wage = case_when(work == 1 & !is.na(ixwagelm) ~ ixwagelm),
  hourwage = case_when(ixhourlm > 0 ~ wage / ixhourlm),
  lnwage = case_when(wage > 1 ~ log(wage)),
  lnhourwage = case_when(hourwage > 1 ~ log(hourwage)),
  #число детей
  nkids = case_when(ixnkids < 100 ~ ixnkids),
  #наличие детей 0-6 лет
  kids06 = case_when(NCAT1_X > 0 ~ 1, TRUE ~ 0),
  profession = ixilpjb8 %/% 1000
)

#Ограничение на возраст и небольшая чистка
data_converted1 <- data_converted1[-which(data_converted1$age %in% c(72:102)), ]
data_converted1 <- data_converted1[-which(is.na(data_converted1$ixhourlm) & !is.na(data_converted1$wage)), ]
data_converted1 <- data_converted1[-which(!is.na(data_converted1$ixhourlm) & is.na(data_converted1$wage)), ]
data_converted1 <- data_converted1[-which(is.na(data_converted1$ixwagelm) & data_converted1$work == 1), ]

data_prepared1 <- data_converted1 %>% select(wage, lnwage, hourwage, lnhourwage, work, age, age2, experience, experience2, educ1, educ2, educ3, educ4, female, married, femarried, nkids, kids06, disabl)

#Вводим доп ограничение
data_final1 <- data_prepared1[-which(data_prepared1$wage %in% c(50000:80000)), ]

special1 <- data_final1 %>% mutate(education = case_when(educ1==1 ~ 1, educ2==1 ~ 2, educ3==1 ~ 3, educ4==1 ~ 4, TRUE ~ 0))
#Строю полигон по образованию
ggplot(special1, aes(x = factor(education))) + 
  geom_bar() + 
  theme_bw() + 
  labs(title = "Распределение работников по уровню образования", x = "Максимальный уровень образования")

#везде возраст и возраст2
model21 <- selection(selection = work ~ age + age2 + educ1 + educ2 + educ3 + educ4 + female + femarried + kids06+disabl+married,
                     outcome = lnwage ~ age + age2 + educ1 + educ2 + educ3 + educ4 + female + disabl,
                     data = data_final1, method='2step')
summary(model21)

#опыт опыт2 во всех
model22 <- selection(selection = work ~ experience + experience2 + educ1 + educ2 + educ3 + educ4 + female + femarried + kids06 + disabl+married,
                     outcome = lnwage ~ experience + experience2 + educ1 + educ2 + educ3 + educ4 + female,
                     data = data_final1, method='2step')
summary(model22)

stargazer(model21, model22, type="text",
          dep.var.labels=c("lnwage"), selection.equation = FALSE,
          out="outcome2.txt")

stargazer(model21, model22, type="text",
          dep.var.labels=c("work"), selection.equation = TRUE,
          out="selection2.txt")
