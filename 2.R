library(tidyverse)
library(readr)
library(sf)
# загрузим табицу с данными
dafr = read_csv2("city_trees2.csv")
# сделаем ее массивом и выведем
a = data.frame(dafr)
a
#выводим имена колонок данных -- это наши переменные
colnames(a)
# 1. ¬се переменные имеют корректный тип данных
# ƒа
# 2. ”бррать повтор€ющиес€ переменные
a$HR <- NULL
a$dbh_mm <- NULL
a$X1 <- NULL
# 3. ”брать размерности из имен переменных
names(a)[names(a) == "dbh_m"] = "dbh"
# в остальных переменных размерность не указывалась
# 4. ¬сем переменным задать реальные размерности
library(units) #устанавливаем соответсвующий пакет
#устанавливаем нужную размерность 
units(a$Ht) = as_units("m")
units(a$dbh) = as_units("m")
units(a$Clearance) = as_units("m")
units(a$Crown_Depth) = as_units("m")
units(a$Total_NSEW_Radial_Crown_Spread) = as_units("m")
units(a$Average_Radial_Crown_spread) = as_units("m")
units(a$Crown_Diameter) = as_units("m")
units(a$Predicted_CD_comb_f) = as_units("m")
units(a$Predicted_CD) = as_units("m")
# 5. ≈сли кака€-то переменна€ €вл€етс€ ошибкой другой переменной, она должна быть убрана и добавлена в виде ошибки к основной переменной
# находим такие переменные и преобразуем их в ошибки
a = a %>% mutate(error = Predicted_CD_comb_f - Crown_Diameter)
a = a %>% mutate(error2 = Predicted_CD - Crown_Diameter)
# удал€ем старое ненужное
a$Diference <- NULL
a$Difference_comb_f <- NULL

# 6, 7, 8.   атегориальные переменные должны быть факторами,  атегории переменной из имени должны быть убраны,  оды категориальных переменных заменены их категори€ми
a$Data_Set[a$Data_Set == "0"] = "Peterborough"
a$Data_Set[a$Data_Set == "1"] = "Norwich"
a$Data_Set

# 10. Ќазвать виды на латыни
# maple - Acer platanoides, 
# Oak - Quercus robur,
# Silver birch - Betula pendula, 
# Sycamore - Platanus occidentalis
a$Species[a$Species == "Oak"] = "Quercus robur"
a$Species[a$Species == "Norway_maple"] = "Acer platanoides"
a$Species[a$Species == "Norway_Maple"] = "Acer platanoides"
a$Species[a$Species == "Silver_Birch"] = "Betula pendula"
a$Species[a$Species == "Sycamore"] = "Platanus occidentalis"

# 9. ƒолжны быть созданы переменные координат(lat,lon) в британской системе координат(с учетом кодов квадратов) и в WGS84
library(stringr)
a$Grid_Reference
coord1 = str_replace_all(a$Grid_Reference, ' ', '')
coord_north = str_trunc(coord1, 12, "right", ellipsis = "") %>% str_trunc(5, "left", ellipsis = "")
coord_north
coord_e = str_trunc(coord1, 7, "right", ellipsis = "") %>% str_trunc(5, "left", ellipsis = "")
quadr = str_trunc(coord1, 2, "right", ellipsis = "")
table = data.frame(as.integer(coord_e), as.integer(coord_north), quadr)
names(table) = c("E", "N", "Quadr")
table = na.exclude(table)
#----------
table = table %>% mutate("Easting_BC" = case_when(
  quadr == "TF" ~ E + 600000,
  quadr == "TG" ~ E + 700000,
  quadr == "TL" ~ E + 600000, 
)) %>% mutate("Northing_BC" = case_when(
  quadr == "TF" ~ N + 300000,
  quadr == "TG" ~ N + 300000,
  quadr == "TL" ~ N + 200000,
))
table = na.exclude(table)

#a$Age_Class[268] = "OM"
#это единственное значение такого класса
#припишем к другому классу, по одному значению построить ничего не получитс€
a$Age_Class[268] = "M" #допустим, к классу ћ. Ќа мой взгл€д, он наиболее близок по свойствам

Model_common = lm(data = a, log(as.double(dbh), base = exp(1)) ~ (as.double(Ht)))

ggplot(data = a, aes(x = as.double(Ht)^0.25, y=log(as.double(dbh), base = exp(1))))+
  geom_point(aes(color=Age_Class))+
  geom_smooth(method = "lm")+
  facet_wrap(~Species)+
  theme_bw()

ggplot(data = a, aes(x = as.double(Ht)^0.25, y=log(as.double(dbh), base = exp(1))))+
  geom_point(aes(color=Species))+
  geom_smooth(method = "lm")+
  facet_wrap(~Age_Class)+
  theme_bw()

ggplot(data = a, aes(x = as.double(Ht)^0.25, y=log(as.double(dbh), base = exp(1))))+
  geom_point(aes(color=Age_Class))+
  geom_smooth(method = "lm")+
  theme_bw()

# –ассмотрим качество модели
anova(Model_common)
# »тоговые коэффициенты
summary(Model_common)
# »тоговые коэффициенты в доступном табличном виде
broom::tidy((Model_common))

biom_mod = function(a){lm(as.double(dbh) ~ as.double(Ht), data = a)}

citymodels = a %>% group_by(Age_Class) %>% nest %>% filter(!is.na(Age_Class)) %>%
  mutate(model = data %>% map(biom_mod))  %>%
  mutate(resids = map2(data, model, modelr::add_residuals)) %>%
  mutate(glance = map(model, broom::glance))

results = citymodels %>% unnest(glance)
results %>% filter(r.squared < 0.999) %>% arrange(desc(r.squared)) %>% select(Age_Class, r.squared)