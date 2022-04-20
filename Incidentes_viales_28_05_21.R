library(tidyr)
library(tidyverse)
library(plotly)
library(varrank)
library(corrplot)
library(viridis)
library(rgdal)
library(gganimate)
setwd("G:/Mi unidad/Bicimixtles/Incidentes_Viales")
#setwd("C:/Users/Usuario/Google Drive/Bicimixtles/Incidentes_Viales")
rm(list=ls())

d <- read.csv("incidentes-viales-c5.csv")


#sapply(d, function(x) sum(is.na(x)))
d <- na.omit(d)


max_la<-max(d$latitud)
19.57632
min_la<- min(d$latitud)
19.09819
max_lo <- max(d$longitud)
-98.95278
min_lo <- min(d$longitud)
-99.34726

cuadricula <- 20

lat_seq <- seq(min_la - 0.01, max_la + 0.01, length=(cuadricula +1))
#lat_clas <-seq(1, 20)
lat_clas<- seq(min_la, max_la, length=cuadricula)


d <- d %>% mutate(cat_lat=cut(latitud, breaks=lat_seq, 
                              labels=lat_clas))

long_seq <- seq(min_lo - 0.01, max_lo + 0.01, length=cuadricula +1)
#long_clas <-seq(1, 20)
long_clas<- seq(min_lo, max_lo, length=cuadricula )
d <- d %>% mutate(cat_long=cut(longitud, breaks=long_seq, 
                               labels=long_clas))

d[,20] <- as.numeric(as.character(d[,20]))
d[,21] <- as.numeric(as.character(d[,21]))

colnames(d)[20] <- "lat"
colnames(d)[21] <- "long"


#accidente-ciclista
#unique(d$incidente_c4)
bici <- d %>% filter(incidente_c4=="accidente-ciclista")
####################################################

### Formato de fecha


bici$fecha <- as.Date(bici$fecha_creacion, "%d/%m/%Y")
names(bici)

######################################################
################### Generar los datos ################


bici_agrup <- bici %>% group_by(fecha,lat,long) %>% tally()

##### Acumular incidentes por fecha



########################

#write.csv(bici_acum,"bici_acum.csv")

#seq(as.Date("2014/1/1"), as.Date("2014/1/10"), "days")

min_f = min(bici_agrup$fecha)
max_f = max(bici_agrup$fecha)
#sample_date_1 <- seq(as.Date("2014/1/1"), as.Date("2014/1/10"), "days")
sample_date_r <- seq(min_f, max_f, "days")
sample_date <- as.data.frame(sample_date_r)
colnames(sample_date)[1] <- "fecha"
###############################

la <- unique(bici_agrup$lat)
lo <- unique(bici_agrup$long)


esp <- expand.grid(la, lo,sample_date_r)
colnames(esp)[1] <- "lat"
colnames(esp)[2] <- "long"
colnames(esp)[3] <- "fecha"


bici_t <- left_join(esp,bici_agrup)


bici_t$n[is.na(bici_t$n)] <- 0


############################

bici_acum <- bici_t %>% group_by(lat,long) %>% mutate(Incidentes = cumsum(n))

bici_f <- bici_acum %>% filter(Incidentes > 0)
######################################


incidentes1 <- bici_f %>% group_by(fecha) %>% tally()

incidentes1 <- incidentes1 %>% mutate(Incidentes_totales = cumsum(n))
incidentes <- subset(incidentes1,select=c(fecha,Incidentes_totales))
bici_total <- left_join(bici_f,incidentes)

###################################
#El mapa

mapa <- readOGR("df_municipio.shp", layer = "df_municipio")
vial <- readOGR("Vialidades_CDMX.shp", layer = "Vialidades_CDMX")

base<-ggplot() +  
  geom_polygon(data=mapa, aes(x=long, y=lat, group=group), 
               fill="black", color="gray",alpha = 0.1) 

calles <-ggplot() +  
  geom_polygon(data=vial, aes(x=long, y=lat, group=group), 
              fill="white", color="black",alpha = 0.1)
#calles 
vialidades <- geom_polygon(data=vial, aes(x=long, y=lat, group=group), 
                           fill="white", color="black",alpha = 0.1)
#######################################


map <- base + 
  vialidades +
  geom_point(aes(x =long, y = lat),
             data = bici)

################################
#################
mapa <- map +
  geom_point(aes(x = long, y = lat, size = n),
             data = bici_total, 
             colour = 'purple', alpha = .5) + labs(size = 'Incidentes')+
  geom_label(aes(label= "Periférico", x = -99.3, y = 19.48),data=bici_total) +
   scale_size_continuous(range = c(1, 8), 
                        breaks = c(250, 500, 750, 1000)) +
  labs(size = 'Incidentes') +
  labs(title = "Incidentes con bicis" ,subtitle = 'fecha: {frame_time}' )
mapa
################
#############################


mapa1 <- map +
  geom_point(aes(x = long, y = lat, size = Incidentes, color = Incidentes),
             data = bici_total, 
             #colour = 'purple',
             alpha = .5) +
            labs(size = 'Incidentes')    +
  geom_label(aes(label= "Incidentes_totales", x = -99.3, y = 19.6),data=bici_total)+
  geom_label(aes(label= Incidentes_totales, x = -99.2, y = 19.6),data=bici_total) +
           scale_size_continuous(range = c(1, 5), 
                        breaks = c(50, 100, 150, 200,250,300,350,400)) +
  scale_fill_viridis() +
  geom_text(aes(label= " A Querétaro", x = -99.28, y = 19.48),data=bici_total) +
  geom_text(aes(label= "A Toluca", x = -99.4, y = 19.30),data=bici_total) +
  geom_text(aes(label= "A Pachuca", x = -99.05, y = 19.54),data=bici_total) +
  geom_text(aes(label= "A Puebla", x = -98.93, y = 19.28),data=bici_total) +
  geom_text(aes(label= "A Cuernavaca", x = -99.25, y = 19.05),data=bici_total) +
  labs(size = 'Incidentes') +
  # Here comes the gganimate specific bits
  labs(title = "Incidentes con bicis" ,subtitle = 'fecha: {frame_time}' ) +
  transition_time(fecha) 
t1 <- Sys.time()
mapa1
t2 <- Sys.time()
t2 - t1
an <- animate(mapa1,start_pause=10, end_pause=20,nframes=200)
an
anim_save("Bici_Viales_3.gif", an)

t2 <- Sys.time()
t2 - t1
an 

##################################
an <- animate(mapa1, fps=10)
an

animate(mapa1, 200, fps = 10) 

######################################

  