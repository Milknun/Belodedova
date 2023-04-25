#Белодедова Мария ДА-132, вариант 1
#регион 32, Брянск
#рассчитайте урожайность пшеницы в 1999 году, взяв для рассчета средние суммы 
#активных температур за указанный год, с 13 ближайших метеостанций но рассчитав 
#колонку di самостоятельно, как долю месяца, когда среднедневные температуры были 
#выше 7 градусов, но учитывая, что посев не может начаться раньше середины апреля,
#а вегетация составляет 4 месяца
library(tidyverse)
library(dplyr)
library(lubridate)
library(rnoaa)
library(ggplot2)

rm(list=ls())
setwd("D:/BelodedovaME")
getwd()

#скачиваем станции (потом закомментировать)
station_data = ghcnd_stations()
#записываем в файл для последующей работы (потом тоже закомментировать)
write.csv(station_data, "stations.csv")
station_data = read.csv("stations.csv")

#После получения списка всех станций, получите список станций ближайших к столице вашего региона,
#создав таблицу с именем региона и координатами его столицы
bryansk=data.frame(id = "BRYANSK", latitude = 53.243561,  longitude = 34.363428)
bryansk_around = meteo_nearby_stations(lat_lon_df = bryansk, station_data = station_data,
                                       limit = 13, var = c("PRCP", "TAVG"),
                                       year_min = 1999, year_max = 1999)

#bryansk_around это список единственным элементом которого является таблица, содержащая идентификаторы 
#метеостанций отсортированных по их удалленности, очевидно что первым элементом таблицы будет 
#идентификатор метеостанции, его то мы и попытаемся получить
? meteo_nearby_stations
bryansk_id = bryansk_around[["BRYANSK"]][["id"]][1:13]

#получение всех данных с метеостанций
summary (bryansk_id)
str(bryansk_around)
all_bryansk_data = meteo_tidy_ghcnd(stationid = bryansk_id)

#чтобы получить таблицу всех метеостанций вокруг нужно выбрать целиком первый объект из списка
bryansk_table = bryansk_around[[1]]
summary(bryansk_table)

#нужно убедится, что этот список включает нужные по условию задачи метеостанции
bryansk_table = filter (bryansk_table)
bryansk_stations = bryansk_table
str(bryansk_stations)

#Таким образом, мы сформировали список необходимых станций, посмотрим, что он содержит
bryansk_stations$id

#Создание цикла, в который загружаются необходимые данные с метеостанций
#Промежуточный объект, куда скачиваются данные с конкретной метеостанции
all_bryansk_data = meteo_tidy_ghcnd(stationid = bryansk_id)

#посмотрим, что же скачивается
?meteo_tidy_ghcnd
summary(all_bryansk_data)

#Создадим промежуточный объект, куда будем скачивать данные с конкретной метеостанции
all_i = data.frame()

#Создадим объект, куда скачаем все данные всех метеостанций
all_bryansk_meteodata = data.frame()

#Цикл для всех метеостанций
for(i in 1:13)
  
{
  
  all_i = meteo_tidy_ghcnd(stationid = bryansk_around[["BRYANSK"]][["id"]][i])
  

  
  #выберем нужные свойства
  
  all_i = all_i[ ,c("id","date","tavg")]
  
  #с помощью команды rbind соединяем данные, полученные на предыдущих и данном 
  #этапах цикла
  
  print(all_i)
  
  all_bryansk_meteodata=rbind(all_bryansk_meteodata, all_i)
  
}

#Записываем полученные результаты
write.csv(all_bryansk_meteodata,"all_bryansk_meteodata.csv")

##################Разбивка даты на составляющие(год, месяц, день года)
#считываем данные из файла csv
all_bryansk_meteodata = read.csv("all_bryansk_meteodata.csv")

#посмотрим на данные
str(all_bryansk_meteodata)

#ищем библиотеку из tidyverse, которая может помочь с датой
library(lubridate)

#вытащить год
#проверим, что работает
y = year(all_bryansk_meteodata$date); y
all_bryansk_meteodata [,"year"]= year(all_bryansk_meteodata$date)

#добавим месяц
all_bryansk_meteodata [,"month"]= month(all_bryansk_meteodata$date)

#вытащить день от начала года
all_bryansk_meteodata [,"day_of_the_year"]= yday(all_bryansk_meteodata$date)

#проверим результат
str(all_bryansk_meteodata)

#отфильтруем данные
years_bryansk_meteodata = filter (all_bryansk_meteodata, year > 1998 & year < 2000 )

#проверим результат
str(years_bryansk_meteodata)
summary(years_bryansk_meteodata)

##################Средняя сумма активных температур - это сумму температур больше 10 град.
#по месячно, остальное в формуле- константы

#### 1. температурy нужно поделить на 10
years_bryansk_meteodata[,"tavg"]= years_bryansk_meteodata$tavg / 10
summary (years_bryansk_meteodata)

#### 2. Превратим в нули все NA и где tavg больше 10 градусов

years_bryansk_meteodata [is.na(years_bryansk_meteodata$tavg), "tavg"] = 0
years_bryansk_meteodata [years_bryansk_meteodata$tavg<10, "tavg"] = 0

#проверяем, что температура получилась в или 0 или больше 10 градусов
summary(years_bryansk_meteodata)
alldays= group_by(years_bryansk_meteodata,id,year,month)

#функция summarize применяет некоторые действия к отдельным группам, 
#полученным с помощью функции group_by
#просуммирую температуру по этим группам с помощью sum
sumT_alldays_bryansk = summarize(alldays, tsum = sum(tavg))

summary(sumT_alldays_bryansk)

#Сгруппирем данные по месяцам
groups_bryansk_months = group_by(sumT_alldays_bryansk,month)
groups_bryansk_months

#найду для всех метеостанций и ВСЕХ лет среднее по месяцам
sumT_months= summarize(groups_bryansk_months , St = mean(tsum))
sumT_months

##################Подготовка к расчету по формуле Урожая
###Ввод констант
#Расчет di как долю месяца, когда среднедневные температуры были выше 7 градусов, но учитывая, 
#что посев не может начаться раньше середины апреля, а вегетация составляет 4 месяца
tdi = all_bryansk_data %>% mutate(date=ymd(date),
                       year=year(date),
                       month=month(date)) %>%
  group_by(year,month) %>%
  mutate(dm = n(),
         gd = case_when(
           tavg >= 70 ~ T,
           tavg <70 ~ F
         )) %>%  summarise(di = sum(gd)/mean(dm))

tdi = tdi %>% filter(year>1998 & year < 2000) %>% ungroup() %>% group_by(month) %>%
  summarise(di = mean(di))
tdi$di[0:3]=0
tdi = tdi %>% mutate(tdis = cumsum(di))
tdi = tdi %>% mutate(di = case_when(tdis > 4 ~ di - (tdis-4),TRUE ~ di))
tdi = tdi %>% mutate(di = case_when(di < 0 ~ 0,TRUE ~ di))

di = tdi$di

#Рассчитываем среднюю сумму активных температур для 13 метеостанций, для Брянской области, за 1999 год
data_b = all_bryansk_data %>% 
  mutate(date=ymd(date),
         year=year(date),
         month=month(date)) %>%
  mutate(tavg=case_when( tavg<50 ~ 0, TRUE ~ tavg)/10) %>% 
  filter (year>1998 & year < 2000) %>% 
  group_by(id,year,month) %>% 
  summarize(tsum = sum(tavg)) %>% 
  group_by(month) %>% summarize(St = mean(tsum))

#Коэффициенты для расчета урожайности пшеницы
afi=c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000)

bfi=c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000)

#di=c(0.000,0.000,0.000,0.738,0.682,1.000,1.000,0.580,0.000,0.000,0.000,0.000)

y = 1.0 # - коэффициент для экспозиции склона - считаем, что все поля идеально ровные;

Kf = 300 # - коэффициент использования ФАР посевом;

Qj = 1600 # - калорийность урожая культуры;

Lj = 2.2 # - сумма частей основной и побочной продукции;

Ej = 25 # - стандартная влажность культуры;

#Рассчитаем Fi по месяцам
#Fi= afi+bfi∗y∗(St>10℃)

sumT_months = mutate(sumT_months, Fi = afi+bfi*y*St)

#Рассчитаем Yi

sumT_months = mutate(sumT_months, Yi = ((Fi*di)*Kf)/(Qj*Lj*(100 - Ej)))

## Расчитываем урожай как сумму по месяцам и думаем разумный ли он

Yield = sum(sumT_months$Yi)

Yield

# Ответ: 18.33 ц/га