#Мельниченко Екатерина Даниловна — для региона 27 рассчитайте 
#урожайность пшеницы в 2012 году, взяв для рассчета 
#средние суммы активных температур за предыдущие 9 лет, с 
#24 ближайших метеостанций
install.packages("rnoaa")
#Подключим нужные библиотеки
library(lubridate)
library(stringr)
library(ggplot2)
library(tidyverse)
library(rnoaa)
station_data = ghcnd_stations()
station_data = read.csv("station_data.csv")
khabarovsk = data.frame(id = "KHABAROVSK", latitude = 48.4761, longitude = 135.0668)
khabarovsk_around = meteo_nearby_stations(lat_lon_df = khabarovsk, station_data = station_data,
                                     limit = 24, var = c("PRCP", "TAVG"),
                                     year_min = 2003, year_max = 2012)
#khabarovsk_around это список единственным элементом которого является таблица, содержащая идентификаторы метеостанций отсортированных по их
# удалленности от Хабаровска, очевидно что первым элементом таблицы будет идентификатор метеостанции Хабаровска, его то мы и попытаемся получить
khabarovsk_id = khabarovsk_around[["KHABAROVSK"]][["id"]][1]
str(khabarovsk_around)
#Для получения всех данных с метеостанции, зная ее идентификатор, используйте след. команду
all_khabarovsk_data = meteo_tidy_ghcnd(stationid = khabarovsk_id)
khabarovsk_table=khabarovsk_around[[1]]
summary(khabarovsk_table)
all_i=data.frame()
all_khabarovsk=data.frame()
for(i in 1:24)
{all_i = meteo_tidy_ghcnd(stationid=khabarovsk_around[["KHABAROVSK"]][["id"]][i])
all_i=all_i[ ,c("id","date", "tavg")]
print(all_i)
all_khabarovsk=rbind(all_khabarovsk, all_i)}
#Средняя сумма активных температур
data_khabarovsk=all_khabarovsk %>%
  mutate(date=ymd(date),
         year=year(date),
         month=month(date)) %>%
  mutate(tavg=case_when(tavg<50 ~ 0, TRUE ~ tavg)/10) %>%
  filter (year>2003 & year<2012) %>%
  group_by(id,year,month) %>%
  summarize(tsum=sum(tavg, na.rm=T)) %>%
  group_by(month) %>% summarize(St = mean(tsum, na.rm=T))
afi=c(0.00,0.00,0.00,32.11,26.31,25.64,32.20,18.73,
      16.30,13.83,0.00,0.00)
bfi=c(0.00,0.00,0.00,11.30,9.26,9.03,8.16,6.59,5.73,
      4.87,0.00,0.00)
di=c(0.00,0.00,0.00,0.33,1.00,1.00,1.00,0.32,0.00,
     0.00,0.00,0.00)
y=1.0
Kf=300
Qj=1600
Lj=2.2
Ej=25
#Рассчитаем урожайность по месяцам
data_khabarovsk= data_khabarovsk %>%
  mutate(Fi=(afi)+(bfi)*y*(data_khabarovsk$St))
data_khabarovsk= data_khabarovsk %>% mutate(Yj=(((data_khabarovsk$Fi)*(di)*Kf)/(Qj*Lj*(100-Ej))))
#Расчитываем суммарную урожайность как сумму по месяцам
YIELD=sum(data_khabarovsk$Yj);YIELD
# Ответ:  15.93 ц/га