#Pimero que haremos en primera instancia es cargar las librerias a utilizar

library(tidyverse)
library(janitor)
library(lubridate)
library(stringr)
library(geosphere)
library(dplyr)


#Con este codigo redirecciono la carga de datos de mi pc
setwd("C:/Users/Shebi/Desktop/Google certificado/Curso 8/Base de datos/Base de datos RAW")
getwd()

#Ahora realizare la carga de las tablas en r studio, y le asignare a cada una su determinada variable

t1 <- read.csv("202004-divvy-tripdata.csv")
t2 <- read.csv("202005-divvy-tripdata.csv")
t3 <- read.csv("202006-divvy-tripdata.csv")
t4 <- read.csv("202007-divvy-tripdata.csv")
t5 <- read.csv("202008-divvy-tripdata.csv")
t6 <- read.csv("202009-divvy-tripdata.csv")
t7 <- read.csv("202010-divvy-tripdata.csv")
t8 <- read.csv("202011-divvy-tripdata.csv")
t9 <- read.csv("202012-divvy-tripdata.csv")
t10 <- read.csv("202101-divvy-tripdata.csv")
t11 <- read.csv("202102-divvy-tripdata.csv")
t12 <- read.csv("202103-divvy-tripdata.csv")

#Centralizar todas las tablas en 1 solo data frame, y eliminar todas las columnas y filas vacias

viaje_cycle <- rbind(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12)
viaje_cycle <- janitor::remove_empty(viaje_cycle,which = c("cols"))
viaje_cycle <- janitor::remove_empty(viaje_cycle,which = c("rows"))
#Tamaño de la base de datos

glimpse(viaje_cycle)

#Cambiar fortmato de fechas en la tabla, se puede observar que esta en formato completo

viaje_cycle$started_at <- lubridate::ymd_hms(viaje_cycle$started_at)
viaje_cycle$ended_at <- lubridate::ymd_hms(viaje_cycle$ended_at)
glimpse(viaje_cycle)

#Calculamos los minutos usados en bicicleta por los usuarios
viaje_cycle$datediff <- difftime(viaje_cycle$ended_at, viaje_cycle$started_at, units = "mins")
viaje_cycle$datediff <- round(viaje_cycle$datediff,2)


#Ahora crearemos una columna que calcule la cantidad de mt o km recorridos por persona

viaje_cycle$meters_recorred <- round(distHaversine(viaje_cycle[,c("start_lng", "start_lat")], viaje_cycle[,c("end_lng", "end_lat")]), 3)

#Despues de haber calculado la distancia podemos empezar a realizar algunos calculos agrupados por tipo de 

glimpse (viaje_cycle)


#Luego de haber calculado las columas como minutos de recorrido, y ademas los metros recorridos podremos calcular algunos analisis

#METROS
Q1_meters  <- quantile(viaje_cycle$meters_recorred,.25,na.rm = TRUE,dims = 1)
Q2_meters  <- quantile(viaje_cycle$meters_recorred,0.50,na.rm = TRUE, dims = 1)
Q3_meters  <- quantile(viaje_cycle$meters_recorred,0.75,na.rm = TRUE,dims = 1)
RIC_meters <- Q3_meters - Q1_meters
LS_meters <- Q3_meters + (RIC_meters * 1.5)
LI_meters <- Q1_meters - (RIC_meters * 1.5)
#MINUTOS
Q1_minutes  <- quantile(viaje_cycle$datediff,.25,na.rm = TRUE,dims = 1)
Q2_minutes  <- quantile(viaje_cycle$datediff,0.50,na.rm = TRUE, dims = 1)
Q3_minutes  <- quantile(viaje_cycle$datediff,0.75,na.rm = TRUE,dims = 1)
RIC_minutes <- Q3_minutes  - Q1_minutes 
LS_minutes <- Q3_minutes  + (RIC_minutes  * 1.5)
LI_minutes <- Q1_minutes  - (RIC_minutes  * 1.5)


Q1_meters
Q2_meters
Q3_meters
RIC_meters
LS_meters 
LI_meters # --> Limite inferior no puede ser menor a 0


Q1_minutes
Q2_minutes
Q3_minutes
RIC_minutes
LS_minutes
LI_minutes #--> MINUTOS NO PUEDEN SER NEGATIVOS

#ANALISIS FILTRADO DE LAS VARIABLES CREADAS RELACIONADAS A LOS MINUTOS Y DISTANCIAS RECORRIDAS

viaje_cycle %>% 
  drop_na() %>% #Dropeamos todas las filas que contengan datos vacios
  filter(between(meters_recorred,0,LS_meters),between(datediff,0,LS_minutes) ) %>% #Filtramos los minutos y metros recorridos datediff > 0,meters_recorred > 100
  arrange(desc(datediff),desc(meters_recorred)) %>% 
  group_by(member_casual) %>% #Agrupamos por tipo de miembro
  summarize(max(datediff),min(datediff),mean(datediff),sd(datediff),max(meters_recorred),min(meters_recorred),mean(meters_recorred),sd(meters_recorred) ) 
  

#Boxplot variable metros
boxplot_meters <- viaje_cycle %>% 
  ggplot(data = viaje_cycle, mapping = aes(x = member_casual, y = meters_recorred)) + geom_boxplot(outlier.shape = NA)   

#Boxplot variable minutos
boxplot_minutes <- viaje_cycle %>%
  ggplot(data = viaje_cycle, mapping = aes(x = member_casual, y = datediff)) + geom_boxplot(outlier.shape = NA ) + scale_y_continuous(name = "Minutos", limits = c(0,100))
boxplot_minutes

##viaje_cycle %>% 
 # drop_na() %>% 
  



#summarise(viaje_cycle)




#Cambiar el formato a numeros en la tabla

#viaje_cycle[c("date_start", 'hms_start')] <- str_split_fixed(viaje_cycle$started_at," ",2)
#viaje_cycle[c("date_end", 'hms_end')] <- str_split_fixed(viaje_cycle$ended_at," ",2)

#Seleccionar las columnas que quiero ocupar
#viaje_cycle <- viaje_cycle[c("ride_id" , "rideable_type","start_station_name","start_station_id","end_station_name" ,"end_station_id","start_lat","start_lng","end_lat","end_lng","member_casual","date_start","hms_start")]









