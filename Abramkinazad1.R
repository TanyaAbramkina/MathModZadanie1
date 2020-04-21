#Абрамкина Татьяна – для региона 17 рассчитайте урожайность пшеницы в 2001 году,
#взяв для рассчета средние суммы активных температур за предыдущие 8 лет, с метеостанций
#на расстоянии от 60 до 150 км
# Проверка рабочей дирректории 
getwd()

#Устанавливаем пакеты
library(tidyverse)
library(rnoaa)
library(lubridate)
#Скачиванием список метеостанций 
station_data = ghcnd_stations()
# Сохраним в файл
write.csv(station_data, "station_data2020.csv")
# Сохраним данные в вектор
station_data = read.csv("station_data2020.csv") 


#Формируем список метеостанций ближайших к столице региона
#Создадим таблицу с именем региона и его координатами
tuva = data.frame(id = "tuva", latitude = 51.584332,longitude= 94.793085)
#прочитаем справку командой meteo_nearby_stations
?meteo_nearby_stations
# выбираем метеостанции в фиксированном радиусе от республики Тыва или конечное число станций,которые имеют необходимые данные в заданный временной период
tuva_around=meteo_nearby_stations(lat_lon_df = tuva, station_data = station_data,
                                      limit = 150,var = c("TAVG"),
                                      year_min = 1993, year_max = 2001)
# получаем индентификатор метеостанции Тыва
tuva_id=tuva_around[["tuva"]][["id"]][1]
summary(tuva_id)
tuva_table=tuva_around[[1]]
summary(tuva_table)
#Получаем таблицы ближайших метеостанций
tuva_table = data.frame(tuva_around)
summary(tuva_table)
# Отфильтруем все станции, на расстоянии от 60 до 150 км
tuva_stations= tuva_table[tuva_table$tuva.distance > 60 & tuva_table$tuva.distance < 150,]
str(tuva_stations)
tuva_stations$tuva.id
# Создание цикла, в котором скачиваются необходимые данные с метеостанций 
# Промежуточный объект, куда скачиваются данные с кокретной метеостанции
all_i = data.frame()
# Объект куда скачиваются все данные со всех метеостанций
all_tuva_meteodata = data.frame()
# Цикл для всех метеостанций
for(i in 1:2)
{
  tuva_id =  tuva_around[["tuva"]] [["id"]] [ i]
  data = meteo_tidy_ghcnd(stationid = tuva_id,
                          var = "TAVG",
                          date_min = "1993-01-01",
                          date_max = "2001-12-31")
  all_tuva_meteodata =  bind_rows(all_tuva_meteodata, data)
 
}
              
# Запись полученных данных в файл
write.csv(all_tuva_meteodata, "all_tuva_meteodata.csv")
all_tuva_meteodata
# считываем данные из файла all_tuva_meteodata.csv
all_tuva_meteodata = read.csv("all_tuva_meteodata.csv")
# Посмотрим на данные
str(all_tuva_meteodata)
# Добавим год, месяц,день
all_tuva_meteodata = mutate(all_tuva_meteodata, year = year(date), month = month(date), day = day(date))
str(all_tuva_meteodata)
# Отфильтруем данные за 1993 - 2001 годы
years_tuva_meteodata = filter(all_tuva_meteodata, year %in% c( 1993:2001))
# Проверим результат
str(years_tuva_meteodata)
summary(years_tuva_meteodata)
#Приводим средние суммы температур в подходящую форму, при помощи деления на 10
years_tuva_meteodata[,"tavg"]= years_tuva_meteodata$tavg / 10 
#Gревращаем все NA и tavg <5 в нули 
years_tuva_meteodata[is.na(years_tuva_meteodata$tavg),"tavg"] = 0
years_tuva_meteodata[years_tuva_meteodata$tavg<5, "tavg"] = 0
summary(years_tuva_meteodata)
#Группировка по метеостанциям, годам и месяцам при помощи функции group_by
alldays = group_by(years_tuva_meteodata, id, year, month)
#Просуммируем температуру по этим группам с помощью sum 
sumT_alldays_tuva = summarize(alldays, tsum = sum(tavg))
summary(sumT_alldays_tuva)
#Группируем данные по месяцам  
groups_tuva_months = group_by(sumT_alldays_tuva, month)
summary(groups_tuva_months)
#Найдем для всех метеостанций среднее по месяцам
sumT_months=summarize(groups_tuva_months,St=mean(tsum))
sumT_months

# Подготовка к расчету по формуле Урожая
#Ввод констант для расчета урожайности
afi = c(0.00, 0.00, 0.00, 32.11, 26.31, 25.64, 23.20, 18.73, 16.30, 13.83, 0.00, 0.00)
bfi = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03, 8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
di = c(0.00, 0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)
#Коэффициент для экспозиции склона - считаем, что все поля идеально ровные
y = 1.0
#Коэффициент использования ФАР посевом
Kf = 300
#Калорийность урожая культуры
Qj = 1600
#Коэффициент "сумма частей основной и побочной продукции"
Lj = 2.2
#Коэффициент "стандартная влажность культуры"
Ej = 25 
#Рассчет Fi по месяцам
sumT_months = mutate(sumT_months, Fi = afi + bfi * y * St)
#Рассчет Yi
sumT_months = mutate(sumT_months, Yi = ((Fi * di) * Kf) / (Qj * Lj * (100 - Ej)))
#Расчитываем урожай, как сумму по месяцам
Yield = sum(sumT_months$Yi)
Yield
# Ответ: 15,29 ц/га
# Для региона 17 урожайность пшеницы в  2001 году составила 15,29 ц/га
