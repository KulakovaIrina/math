library(sp) #
library(rgdal) # чтение форматов геоданных
library(raster) # работа с растровыми данными
library(dynatopmodel) # морфометрический анализ
library(spatialEco) # обработка пространственных данных
library(gstat) # геостатистика
library(automap) # кригинг
library(tidyverse)
library(readr)
library(sf)

#points <- read.csv('xrf_points_demo.csv')
#------
points <- read.csv('xrf_points_with_nickel.csv')
#------
dem <- readGDAL('predictors/dsm_p_m_c_1.5.tif')

# разбиваем выборку на 2 части (~75% - тестовые данные, ~25% - данные для валидации)

subset <- subset(points, Set == "Test")
validation <- subset(points, Set == "Validation")

# данные для линейных регрессионных моделей должны быть распределены нормально
# в противном случае их нужно трансформировать

subset$Ni_exp_sqrt <- sqrt(subset$Ni_express)

# конвертируем таблицу в набор пространственных данных

coordinates(subset) <- ~x_utm + y_utm

# очень важно присвоить правильную проекцию
# https://www.earthdatascience.org/courses/earth-analytics/spatial-data-r/understand-epsg-wkt-and-other-crs-definition-file-types/

proj4string(subset) <-  CRS("+init=epsg:32636")
plot(subset)

# готовим растровые данные - независимые переменные для моделирования распределения концентрации никеля в почве

predictors <- raster(dem)
predictors$dem <- raster(dem)
predictors$slope <- terrain(raster(dem), opt = 'slope', unit = 'degrees')
predictors$aspect <- terrain(raster(dem), opt = 'aspect', unit = 'degrees')
predictors$flowdir <- terrain(raster(dem), opt = 'flowdir')
predictors$flowacc <- upslope.area(raster(dem), log=F, atb=F, deg=0.1, fill.sinks=F)
predictors$twi <- log(predictors$flowacc / tan(predictors$slope / 180))
predictors$curvature <- curvature(raster(dem), type = "total")

predictors$soils <- raster(readGDAL('predictors/mon_soils_raster_1.5m.tif'))

preds <- dropLayer(predictors, 1)
predictors <- preds
proj4string(predictors) <-  CRS("+init=epsg:32636")

plot(predictors)

# для регрессионного кригинга нам необходимо конвертировать наш набор растров в формат 'SpatialGridDataFrame'

pred <- as(predictors, 'SpatialGridDataFrame')
pred$soils <- as.factor(pred$soils)


# для каждой точки мы теперь можем "вытащить" значения каждого параметра в соответсвующей ячейке растра

sample <- raster::extract(predictors, subset, df=TRUE)
sample$soils <- as.factor(sample$soils)

#трассировка
write.csv2(subset, "test_subset.csv")
write.csv2(sample, "test_sample.csv")
#трассировка
#и тут мы видим, что в Sample затесались NA
#следующий код добавляет значения из Sample в Subset
#если туда попадут NA - работать не будет

subset$slope <- sample$slope
subset$aspect <- sample$aspect
subset$dem <- sample$dem
subset$curvature <- sample$curvature
subset$twi <- sample$twi
subset$flowdir <- sample$flowdir
subset$flowacc <- sample$flowacc
subset$soils <- sample$soils

#трассировка
write.csv2(subset, "test_subset2.csv")
write.csv2(sample, "test_sample2.csv")
#трассировка
#как видим, в Sample попали NA. Уберем строки с NA
a3 = data.frame(subset)
a4 = na.exclude(a3) #убрали
#теперь вернем переменной Subset ее первоначальные свойства
subset <- subset(a4)
coordinates(subset) <- ~x_utm + y_utm
proj4string(subset) <-  CRS("+init=epsg:32636")
#вернули. Продолжаем выполнение основного кода
head(subset)

# построим нашу первую регрессионную модель

l_fit <- lm(Ni_exp_sqrt ~ slope + aspect + dem + curvature + twi + flowdir + flowacc + soils, subset)

optimal_fit <- step(l_fit, direction = 'backward')

summary(optimal_fit)

# создадим вариограмму отстатков

vgmlm <- variogram(optimal_fit$residuals ~ 1, subset)

# также необходимо создать оптимальную модель вариограммы

vgm_lm <- vgm(nugget = 0.4, psill = 0.35, range = 60, model = "Exp")

plot(vgmlm, vgm_lm)

# регрессионный кригинг

rk <- krige(optimal_fit$call$formula, subset, pred, vgm_lm)
# это пока модель

# заодно сделаем обычный кригинг
krig <- autoKrige(Ni_exp_sqrt~1, subset, pred)

plot(krig)
plot(krig$krige_output$var1.pred)

# теперь построим карты концентрации никеля в почве на основе полученных моделей

pred$PredNiLmRK <- (rk$var1.pred)^2 # почвему мы возводим результат в квадрат?

pred$OrKrig <- (krig$krige_output$var1.pred)^2

# конвертируем обратно в набор растров

predicted <- stack(pred)
plot(predicted)

# подготовим данные для валидации

coordinates(validation) <- ~x_utm + y_utm
proj4string(validation) <-  CRS("+init=epsg:32636")

sampleVAL <- raster::extract(predicted, validation, df = T)

validation$Ni_predicted_lm_rk <- sampleVAL$PredNiLmRK
validation$Ni_predicted_or_kg <- sampleVAL$OrKrig

# проанализируем корреляцию предсказанных значений концентрации никеля и данных полевых измерений

plot(validation$Ni_express, validation$Ni_predicted_lm_rk)
plot(validation$Ni_express, validation$Ni_predicted_or_kg)

cor(validation$Ni_express, validation$Ni_predicted_lm_rk)
cor(validation$Ni_express, validation$Ni_predicted_or_kg)