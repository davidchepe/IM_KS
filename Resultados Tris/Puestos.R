##########################################################################
# David SÃ¡nchez - LiveToTriathlon
##########################################################################

list.of.packages <- c("dplyr","ggplot2", "scales", "tibble", "foreach", "dbplyr", "broom", "DBI", "lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#devtools::install_github("edgararuiz/tidypredict")

library(tidypredict)
library(ggplot2)
library(scales)
library(tibble)
library(dplyr)
library(broom)
library(MASS)

ranks_table <- readr::read_csv('Combined_data.csv')
head(ranks_table)
str(ranks_table)
summary(ranks_table)

unique(ranks_table$race)
  
  
ranks_table %>% 
  select(overall_rank, s_overal_rank, b_overal_rank, r_overal_rank, run, date, race) %>%
  mutate(s_overal_rank = as.numeric(s_overal_rank)) %>% 
  mutate(b_overal_rank = as.numeric(b_overal_rank)) %>%
  mutate(date =  substr(date,1,4)) %>% 
  group_by(date, race) %>% 
  mutate(r_overal_rank = rank(run)) %>% 
  select(overall_rank, s_overal_rank, b_overal_rank, r_overal_rank, date, race) %>%
  ungroup(date, race) %>% 
  na.omit(.)-> ranks_table_columns

str(ranks_table_columns)
View(ranks_table_columns)
head(ranks_table_columns)
tail(ranks_table_columns)
summary(ranks_table_columns)

unique(ranks_table_columns$race)


ranks_table_columns %>%
  mutate(race = paste(race, substr(date,1,4), sep = "_", collapse = NULL)) %>%
  filter(race != 'western-australia_2017') %>%
  dplyr::select(overall_rank, s_overal_rank, b_overal_rank, r_overal_rank, race) -> ranks_model_full

summary(ranks_model_full)
unique(ranks_model_full$race)

barplot(table(ranks_model_full$race))

dup <- duplicated(ranks_model_full)
ranks_model_full[dup,]


result <- data.frame()

for(i in 1:length(unique(ranks_model_full$race))) {
  
  print(unique(ranks_model_full$race)[i])
  ranks_model_full %>% 
    filter(race==unique(ranks_model_full$race)[i]) %>%
    dplyr::select(overall_rank, s_overal_rank, b_overal_rank, r_overal_rank) -> rank_model_i 
  print("1")
  model <- lm(overall_rank ~ s_overal_rank + b_overal_rank + r_overal_rank -1, data = unique(rank_model_i), na.action = NULL) 
  result_aux <- data.frame("lm-1",unique(ranks_model_full$race)[i], t(model$coefficients))
  names(result_aux) <- (c("metodo","race", "s_overal_rank", "b_overal_rank", "r_overal_rank"))
  result <- rbind(result, result_aux)
  print("2")
  model2 <- rlm(overall_rank ~ s_overal_rank + b_overal_rank + r_overal_rank -1, data = unique(rank_model_i), na.action = NULL)
  result_aux <- data.frame("lmr-1",unique(ranks_model_full$race)[i], t(model$coefficients))
  names(result_aux) <- (c("metodo","race", "s_overal_rank", "b_overal_rank", "r_overal_rank"))
  result <- rbind(result, result_aux)
  print("3")
  model3 <- lm(overall_rank ~ s_overal_rank + b_overal_rank + r_overal_rank, data = unique(rank_model_i), na.action = NULL)
  result_aux <- data.frame("lm",unique(ranks_model_full$race)[i], t(model$coefficients))
  names(result_aux) <- (c("metodo","race", "s_overal_rank", "b_overal_rank", "r_overal_rank"))
  result <- rbind(result, result_aux)
  print("4")
  model4 <- rlm(overall_rank ~ s_overal_rank + b_overal_rank + r_overal_rank, data = unique(rank_model_i), na.action = NULL)
  result_aux <- data.frame("lmr",unique(ranks_model_full$race)[i], t(model$coefficients))
  names(result_aux) <- (c("metodo","race", "s_overal_rank", "b_overal_rank", "r_overal_rank"))
  result <- rbind(result, result_aux)
}

View(result)
hist(result$s_overal_rank)
hist(result$b_overal_rank)
hist(result$r_overal_rank)
plot(result$r_overal_rank~result$b_overal_rank)

result$race[result$r_overal_rank>0.6]

write.csv2(result, file = 'model.csv')



residuos <- model$residuals
residuos2 <- model2$residuals
residuos3 <- model3$residuals
residuos4 <- model4$residuals

plot(residuos)
hist(residuos)
smoothScatter(model$residuals)
qqnorm(model$residuals); qqline(model$residuals,col=2)

plot(residuos2)
hist(residuos2)
smoothScatter(model2$residuals)
qqnorm(model2$residuals); qqline(model2$residuals,col=2)

plot(residuos3)
hist(residuos3)
smoothScatter(model3$residuals)
qqnorm(model3$residuals); qqline(model3$residuals,col=2)

plot(residuos4)
hist(residuos4)
smoothScatter(model4$residuals)
qqnorm(model4$residuals); qqline(model4$residuals,col=2)

#Distribucion mas T que normal
confint(model,level=0.95)
shapiro.test(model$residual)

ggplot(result) +
  aes(x = b_overal_rank, y = r_overal_rank, label = race, solid = FALSE) + 
  geom_point()+geom_text(vjust=1) +geom_smooth(method = "auto")

unique(ranks_model_full$race)

ranks_model_full %>% 
  filter(race == 'Lanzarote_2017')%>%
  dplyr::select(overall_rank, s_overal_rank, b_overal_rank, r_overal_rank) -> test

lm(overall_rank ~ s_overal_rank + r_overal_rank+b_overal_rank, data=test)  -> model5

summary(model5)
model6 = step(model5, trace = 1, direction = "both")
summary(model6)
  
residuos6 <- model6$residuals  
  

plot(residuos6)
hist(residuos6)
smoothScatter(model6$residuals)
qqnorm(model6$residuals); qqline(model6$residuals,col=2)

#Distribucion mas T que normal
confint(model6,level=0.95)
shapiro.test(model6$residual)
#No es una distribución normal

model6$residual

ggplot(result) +
  aes(x = b_overal_rank, y = r_overal_rank, label = race, solid = FALSE) + 
  geom_point()+geom_text(vjust=1) +geom_smooth(method = "auto")
  

  
  
  
#PROBAMOS A METER EL GGEE
ranks_table %>% 
  dplyr::select(overall_rank, s_overal_rank, b_overal_rank, r_overal_rank, run, date, race, ggee) %>%
  mutate(s_overal_rank = as.numeric(s_overal_rank)) %>% 
  mutate(b_overal_rank = as.numeric(b_overal_rank)) %>%
  mutate(date =  substr(date,1,4)) %>%
  filter(race == 'Lanzarote')%>%
  filter(date == '2017')%>%
  group_by(date, race) %>% 
  mutate(r_overal_rank = rank(run)) %>% 
  ungroup(date, race) %>% 
  dplyr::select(overall_rank, s_overal_rank, b_overal_rank, r_overal_rank, ggee) %>%
  na.omit(.)-> ranks_table_columns_ggee

summary(ranks_table_columns_ggee)
table(ranks_table_columns_ggee$date)

#ANALIZAMOS CAMBIOS ESTRUCTURALES
lm(overall_rank ~ s_overal_rank + r_overal_rank+b_overal_rank, data=ranks_table_columns_ggee[ranks_table_columns_ggee$ggee=='25-29',])  -> model25
summary(model25)
lm(overall_rank ~ s_overal_rank + r_overal_rank+b_overal_rank, data=ranks_table_columns_ggee[ranks_table_columns_ggee$ggee=='30-34',])  -> model30
summary(model30)

residuos_GGEE <- model30$residuals  

plot(residuos_GGEE)
hist(residuos_GGEE)
smoothScatter(model30$residuals)
qqnorm(model30$residuals); qqline(model30$residuals,col=2)


###Vamos con Race

ranks_table %>% 
  dplyr::select(overall_rank, s_overal_rank, b_overal_rank, r_overal_rank, run, date, race) %>%
  mutate(s_overal_rank = as.numeric(s_overal_rank)) %>% 
  mutate(b_overal_rank = as.numeric(b_overal_rank)) %>%
  mutate(date =  substr(date,1,4)) %>%
  group_by(date, race) %>% 
  mutate(r_overal_rank = rank(run)) %>% 
  dplyr::select(overall_rank, s_overal_rank, b_overal_rank, r_overal_rank, date, race) %>%
  ungroup(date, race) %>%
  dplyr::select(overall_rank, s_overal_rank, b_overal_rank, r_overal_rank, race, date) %>%
  na.omit(.)-> ranks_table_r

ranks_table_r %>%
  mutate(race2 = paste(race, substr(date,1,4), sep = "_", collapse = NULL)) %>% 
  filter(race2 != 'western-australia_2017') %>%
  dplyr::select(overall_rank, s_overal_rank, b_overal_rank, r_overal_rank, race) -> ranks_model_full_r


result <- data.frame()

for(i in 1:length(unique(ranks_model_full_r$race))) {
  print(unique(ranks_model_full_r$race)[i])
  ranks_model_full_r %>% 
    filter(race==unique(ranks_model_full_r$race)[i]) %>%
    dplyr::select(overall_rank, s_overal_rank, b_overal_rank, r_overal_rank) -> rank_model_i 
  model_r <- lm(overall_rank ~ s_overal_rank + b_overal_rank + r_overal_rank -1, data = unique(rank_model_i), na.action = NULL)
  result_aux <- data.frame("lm",unique(ranks_model_full_r$race)[i], t(model_r$coefficients))
  names(result_aux) <- (c("metodo","race", "s_overal_rank", "b_overal_rank", "r_overal_rank"))
  result <- rbind(result, result_aux)
}

summary(model_r)
write.csv2(result, file = 'model_race.csv')

#*****************************************************************************************************





###Vamos con Race y Date

ranks_table %>% 
  dplyr::select(overall_rank, s_overal_rank, b_overal_rank, r_overal_rank, run, date, race) %>%
  mutate(s_overal_rank = as.numeric(s_overal_rank)) %>% 
  mutate(b_overal_rank = as.numeric(b_overal_rank)) %>%
  mutate(date =  substr(date,1,4)) %>% 
  group_by(date, race) %>% 
  mutate(r_overal_rank = rank(run)) %>% 
  dplyr::select(overall_rank, s_overal_rank, b_overal_rank, r_overal_rank, date, race) %>%
  ungroup(date, race) %>%
  dplyr::select(overall_rank, s_overal_rank, b_overal_rank, r_overal_rank, race, date) %>%
  na.omit(.)-> ranks_table_race

ranks_table_race %>%
  mutate(race = paste(race, substr(date,1,4), sep = "_", collapse = NULL)) %>% 
  filter(race != 'western-australia_2017') %>%
  dplyr::select(overall_rank, s_overal_rank, b_overal_rank, r_overal_rank, race) -> ranks_model_full_race


result <- data.frame()

for(i in 1:length(unique(ranks_model_full_race$race))) {
  print(unique(ranks_model_full_race$race)[i])
  ranks_model_full_race %>% 
    filter(race==unique(ranks_model_full_race$race)[i]) %>%
    dplyr::select(overall_rank, s_overal_rank, b_overal_rank, r_overal_rank) -> rank_model_i 
  model_race <- lm(overall_rank ~ s_overal_rank + b_overal_rank + r_overal_rank -1, data = unique(rank_model_i), na.action = NULL)
  result_aux <- data.frame("lm",unique(ranks_model_full_race$race)[i], t(model_race$coefficients))
  names(result_aux) <- (c("metodo","race", "s_overal_rank", "b_overal_rank", "r_overal_rank"))
  result <- rbind(result, result_aux)
}

summary(model_race)
write.csv2(result, file = 'model_race_date.csv')

#*****************************************************************************************************



###Vamos con GGEE y todo

ranks_table %>% 
  dplyr::select(overall_rank, s_overal_rank, b_overal_rank, r_overal_rank, run, date, race, ggee) %>%
  mutate(s_overal_rank = as.numeric(s_overal_rank)) %>% 
  mutate(b_overal_rank = as.numeric(b_overal_rank)) %>%
  mutate(date =  substr(date,1,4)) %>% 
  group_by(date, race) %>% 
  mutate(r_overal_rank = rank(run)) %>% 
  dplyr::select(overall_rank, s_overal_rank, b_overal_rank, r_overal_rank, date, race, ggee) %>%
  ungroup(date, race) %>% 
  na.omit(.)-> ranks_table_ggee



str(ranks_table_ggee)
View(ranks_table_ggee)
head(ranks_table_ggee)
tail(ranks_table_ggee)
summary(ranks_table_ggee)

unique(ranks_table_ggee$race)


ranks_table_ggee %>%
  mutate(race = paste(race, substr(date,1,4), sep = "_", collapse = NULL)) %>%
  mutate(race = paste(race, ggee, sep = "_", collapse = NULL)) %>% 
  filter(race != 'western-australia_2017') %>%
  dplyr::select(overall_rank, s_overal_rank, b_overal_rank, r_overal_rank, race) -> ranks_model_full_ggee

summary(ranks_model_full_ggee)
unique(ranks_model_full_ggee$race)

#comprobamos duplicados
barplot(table(ranks_model_full_ggee$race))
dup <- duplicated(ranks_model_full_ggee)
ranks_model_full_ggee[dup,]


result <- data.frame()

for(i in 1:length(unique(ranks_model_full_ggee$race))) {
  
  print(unique(ranks_model_full_ggee$race)[i])
  ranks_model_full_ggee %>% 
    filter(race==unique(ranks_model_full_ggee$race)[i]) %>%
    dplyr::select(overall_rank, s_overal_rank, b_overal_rank, r_overal_rank) -> rank_model_i 
  # print("1")
  # model <- lm(overall_rank ~ s_overal_rank + b_overal_rank + r_overal_rank -1, data = unique(rank_model_i), na.action = NULL) 
  # result_aux <- data.frame("lm-1",unique(ranks_model_full$race)[i], t(model$coefficients))
  # names(result_aux) <- (c("metodo","race", "s_overal_rank", "b_overal_rank", "r_overal_rank"))
  # result <- rbind(result, result_aux)
  # print("2")
  # model2 <- rlm(overall_rank ~ s_overal_rank + b_overal_rank + r_overal_rank -1, data = unique(rank_model_i), na.action = NULL)
  # result_aux <- data.frame("lmr-1",unique(ranks_model_full$race)[i], t(model$coefficients))
  # names(result_aux) <- (c("metodo","race", "s_overal_rank", "b_overal_rank", "r_overal_rank"))
  # result <- rbind(result, result_aux)
  # print("3")
  model_ggee <- lm(overall_rank ~ s_overal_rank + b_overal_rank + r_overal_rank -1, data = unique(rank_model_i), na.action = NULL)
  result_aux <- data.frame("lm",unique(ranks_model_full_ggee$race)[i], t(model_ggee$coefficients))
  names(result_aux) <- (c("metodo","race", "s_overal_rank", "b_overal_rank", "r_overal_rank"))
  result <- rbind(result, result_aux)
  # print("4")
  # model4 <- rlm(overall_rank ~ s_overal_rank + b_overal_rank + r_overal_rank, data = unique(rank_model_i), na.action = NULL)
  # result_aux <- data.frame("lmr",unique(ranks_model_full$race)[i], t(model$coefficients))
  # names(result_aux) <- (c("metodo","race", "s_overal_rank", "b_overal_rank", "r_overal_rank"))
  # result <- rbind(result, result_aux)
}

summary(model_ggee)
model_ggee$coefficients
anova(model_ggee)


View(result)
hist(result$s_overal_rank)
hist(result$b_overal_rank)
hist(result$r_overal_rank)
plot(result$r_overal_rank~result$b_overal_rank)


write.csv2(result, file = 'model.csv')


