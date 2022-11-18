#bike share hw

library(tidyverse)
library(sf)
library(lubridate)
library(tigris)
library(gganimate)
library(riem)
library(gridExtra)
library(knitr)
library(kableExtra)
library(cowplot)
library(geojsonR)

options(tigris_class = "sf")
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

palette5 <- c("#eff3ff","#bdd7e7","#6baed6","#3182bd","#08519c")
palette4 <- c("#D2FBD4","#92BCAB","#527D82","#123F5A")
palette2 <- c("#6baed6","#08519c")

##collect indego rides

rides <- read.csv("C:/Users/Charlie/Documents/GIS/5thSq/IndegoMap/RideData/indego-trips-2016-q1.csv")

cd <- "C:/Users/Charlie/Documents/GIS/5thSq/IndegoMap/RideData/indego-trips"
years <- c("2022")
qs <- c("q2")

indegofull <- rides[0,]

for (year in years){
  for (q in qs){
    df <- read.csv(paste(paste(cd,paste(year,q, sep = "-"),sep = "-"),".csv", sep = ""))
    if("bike_type" %in% colnames(df)== F){
      df$bike_type <- rep(NA, nrow(df))
    }
    
    df$start_time2 <- df$start_time
    df <- separate(df, start_time2, into = c('date','time'), sep = " ")
    
    if(substr(df$date[1], 1, 2) == '20'){
      df$date <- df$date %>% as.Date(format = "%Y-%m-%d")
    }else{
      df$date <- df$date %>% as.Date(format = "%m/%d/%Y")
    }
    
    
    indegofull <- rbind(indegofull, df)
    print(paste(year,q,sep="-"))
  }
}




#bring in stations

statlatlong <- data.frame(df$start_station, df$start_lat, df$start_lon)

statlatlong <- statlatlong[!duplicated(statlatlong),]
statlatlong <- statlatlong %>% rename('Station_ID' = 'df.start_station',
                                      'lat'='df.start_lat',
                                      'lon'='df.start_lon')
stations <- read.csv("C:/Users/Charlie/Documents/GIS/5thSq/IndegoMap/indego-stations-2021-07-01.csv")


#statlatlong$Station_ID <- statlatlong$Station_ID %>% as.numeric()
stats <- left_join(x = stations, y = statlatlong)

stats <- stats %>% rename('dateopen' = "Day.of.Go_live_date") 


stats <- stats %>% filter(!is.na(lat)) %>% st_as_sf(coords = c('lon','lat'),crs = (4326))


###DOCK STATION SHIT

stats_geojson<- FROM_GeoJson(url_file_string = 'https://kiosks.bicycletransit.workers.dev/phl')

statdocks <- data.frame(totaldocks = rep(x=NA, 184), station_name = rep(x = NA,184))

stats_geojson$features[[3]]$properties$name
stats_geojson$features[[3]]$properties$totalDocks

for(i in 1:184){
  statdocks[i,] <- c(stats_geojson$features[[i]]$properties$totalDocks,
                     stats_geojson$features[[i]]$properties$name)
}


stats <- left_join(stats, statdocks, by = c('Station_Name' = 'station_name'))


#get time frame
indegofull <- indegofull %>% filter(start_station != 3000 & end_station != 3000)

#lubridate

indegofull$year <- indegofull$date %>% year()
indegofull$week <- indegofull$date %>% week()

indegofull$start_time_60 <- indegofull$start_time %>% mdy_hm() %>% floor_date(unit = '1 hour')
indegofull$end_time_60 <- indegofull$end_time %>% mdy_hm() %>% floor_date(unit = '1 hour') 

indegofull$endweek <- indegofull$end_time_60 %>% week()

#station names

indegofull <- left_join(indegofull, stats, by = c('start_station' = 'Station_ID'))
indegofull <- rename(indegofull, start_station_name = 'Station_Name')

indegofull <- left_join(indegofull, stats %>%
                          st_drop_geometry() %>% 
                          select('Station_ID', 'Station_Name'),
                        by = c('end_station' = 'Station_ID'))

indegofull <- rename(indegofull, end_station_name = 'Station_Name')



indego5week <- indegofull %>% filter(year == 2022 & week %in% seq(15,19) & endweek  %in% seq(15,19))



# 
# indegofull$start_time_60 <- indegofull$start_time %>% mdy_hm() %>% floor_date(unit = '1 hour') %>% as_datetime()
# 
# 
length(unique(indego5week$start_station_name))
length(unique(indego5week$end_station_name))

length(unique(indego5week$start_time_60))
length(unique(indego5week$end_time_60))



#create panel
length(unique(indego5week$start_station_name)) * length(unique(indego5week$start_time_60)) 


study.panel <- 
  expand.grid(hour_unit = unique(indego5week$start_time_60 ), 
              station_name = unique(indego5week$end_station_name))

nrow(study.panel)      

#fill panel
bike.panel.starts <- 
  indego5week %>%
  mutate(Start_Counter = 1) %>%
  right_join(study.panel, by = c('start_time_60' = 'hour_unit', 'start_station_name' = 'station_name')) %>% 
  group_by(start_time_60, start_station_name) %>%
  summarize(start_count = sum(Start_Counter, na.rm=T)) %>% 
  rename('hour_unit' = start_time_60,
         'station_name' = start_station_name)

bike.panel.ends <- 
  indego5week %>%
  mutate(End_Counter = 1) %>%
  right_join(study.panel, by = c('end_time_60' = 'hour_unit', 'end_station_name' = 'station_name')) %>% 
  group_by(end_time_60, end_station_name) %>%
  summarize(end_count = sum(End_Counter, na.rm=T))  %>% 
  rename('hour_unit' = end_time_60,
         'station_name' = end_station_name)


bike.panel <- left_join(bike.panel.starts,bike.panel.ends) %>% filter(!station_name %>% is.na())

bike.panel <- left_join(bike.panel, statdocks) %>% 
  mutate(totaldocks = as.numeric(totaldocks))

  
bike.panel$totaldocks <- ifelse(is.na(bike.panel$totaldocks), median(bike.panel$totaldocks, na.rm = T), bike.panel$totaldocks)


bike.panel$net <- bike.panel$start_count - bike.panel$end_count
bike.panel$total <- bike.panel$start_count + bike.panel$end_count

bike.panel <- 
  bike.panel %>% 
  arrange(station_name, hour_unit)

bike.panel$net_over_time <- seq(0,1,nrow(bike.panel))
for(i in 2:nrow(bike.panel)){
  if(bike.panel$station_name[i] != bike.panel$station_name[i-1]){
    bike.panel$net_over_time[i] <- bike.panel$net[i]
  } else {
    bike.panel$net_over_time[i] <- bike.panel$net_over_time[i-1] + bike.panel$net[i]
  }
}

bike.panel$total_over_time <- seq(0,1,nrow(bike.panel))
for(i in 2:nrow(bike.panel)){
  if(bike.panel$station_name[i] != bike.panel$station_name[i-1]){
    bike.panel$total_over_time[i] <- bike.panel$total[i]
  } else {
    bike.panel$total_over_time[i] <- bike.panel$total_over_time[i-1] + bike.panel$total[i]
  }
}


bike.panel$bikes_at_stat <- rep(NA,1,nrow(bike.panel))
bike.panel$bikes_at_stat[1] <- 10 
bike.panel$van_deployed <- rep('none',1,nrow(bike.panel))
for(i in 2:nrow(bike.panel)){
  if(bike.panel$station_name[i] != bike.panel$station_name[i-1]){
    bike.panel$bikes_at_stat[i] <- ceiling(bike.panel$totaldocks[i] / 2) 
  } else if(bike.panel$bikes_at_stat[i-1] > bike.panel$totaldocks[i]) { #if station is full
    bike.panel$bikes_at_stat[i] <- ceiling(bike.panel$totaldocks[i]/4)*3
    bike.panel$van_deployed[i] <- "take"
  } else if(bike.panel$bikes_at_stat[i-1] < 2) { #if station is empty
    bike.panel$bikes_at_stat[i] <- ceiling(bike.panel$totaldocks[i]/4)
    bike.panel$van_deployed[i] <- "leave"  
  } else {
    bike.panel$bikes_at_stat[i] <- bike.panel$bikes_at_stat[i-1] + bike.panel$total[i]
    bike.panel$van_deployed[i] <- 'none'
  }
}


f<-bike.panel %>%
  filter(station_name %in% sample(bike.panel$station_name %>% unique(), 5)) %>% 
  group_by(hour_unit, station_name) %>% 
  summarize(bikes_at_stat = bikes_at_stat, statname = station_name, van_deployed = van_deployed) %>% 
  ungroup() 

#f$statname <- factor(f$statname, levels=h %>% filter(stats5 == 'y') %>% pull(station_name))

f <- bike.panel %>%
  filter(station_name %in% sample(bike.panel$station_name %>% unique(), 5)) %>% droplevels()
f$station_name <- factor(f$station_name)

ggplot(f,aes(hour_unit,bikes_at_stat)) + geom_line() +
  scale_colour_manual(values = c('red', 'green','blue')) + 
  #geom_vline(data = mondays, aes(xintercept = monday)) +
  facet_wrap(~as.character(station_name), ncol =1, scales = 'free')+
  labs(#title="Rideshare trips by week: November-December",
    #subtitle="Dotted lines for Thanksgiving & Christmas", 
    x="Starts per Hour", y="") +
  plotTheme() + theme(panel.grid.major = element_blank())    




 # add weather features

##weather
weather.Data <- 
  riem_measures(station = "PHL", date_start = "2021-01-01", date_end = "2021-12-12")

weather.Panel <-  
  weather.Data %>%
  mutate_if(is.character, list(~replace(as.character(.), is.na(.), "0"))) %>% 
  replace(is.na(.), 0) %>%
  mutate(interval60 = ymd_h(substr(valid, 1, 13))) %>%
  mutate(week = week(interval60),
         dotw = wday(interval60, label=TRUE)) %>%
  group_by(interval60) %>%
  summarize(Temperature = max(tmpf),
            Percipitation = sum(p01i),
            Wind_Speed = max(sknt)) %>%
  mutate(Temperature = ifelse(Temperature == 0, 42, Temperature))

grid.arrange(top = "Weather Data - Chicago - November & December, 2018",
             ggplot(weather.Panel, aes(interval60,Percipitation)) + geom_line() + 
               labs(title="Percipitation", x="Hour", y="Percipitation") + plotTheme(),
             ggplot(weather.Panel, aes(interval60,Wind_Speed)) + geom_line() + 
               labs(title="Wind Speed", x="Hour", y="Wind Speed") + plotTheme(),
             ggplot(weather.Panel, aes(interval60,Temperature)) + geom_line() + 
               labs(title="Temperature", x="Hour", y="Temperature") + plotTheme())



bike.panel <- 
  bike.panel %>% 
  #left_join(weather.Panel, by = c('hour_unit' = 'interval60')) %>%
  left_join(stats %>% select(geometry, Station_Name), by = c('station_name' = 'Station_Name')) %>%
  mutate(week = week(hour_unit),
         dotw = wday(hour_unit, label = TRUE)) %>%
  st_sf()


#time features
bike.panel <- 
  bike.panel %>% 
  arrange(station_name, hour_unit) %>% 
  group_by(station_name) %>% 
  mutate(sclagHour = dplyr::lag(start_count,1),
         sclag2Hours = dplyr::lag(start_count,2),
         sclag3Hours = dplyr::lag(start_count,3),
         sclag12Hours = dplyr::lag(start_count,12),
         sclag1day = dplyr::lag(start_count,24)) %>% 
  ungroup()


#split train/test

bike.Train <- filter(bike.panel, week < 18)
bike.Test <- filter(bike.panel, week >= 18)

mondays <- 
  mutate(bike.panel,
         monday = ifelse(dotw == "Fri" & hour(hour_unit) == 1,
                         hour_unit, 0)) %>%
  filter(monday != 0) 






h <- bike.panel %>% 
  group_by(station_name) %>% 
  summarize(total = sum(total),
            net = sum(net)) %>% .[order(-.$total),] %>%
  mutate(stats5 = ifelse(total %in% quantile(.$total), 'y','n')) %>% 
  left_join(statdocks)

h1<-ggplot(h)+ 
  geom_bar(aes(y=total,x=reorder(station_name, total),
               fill = stats5), stat = 'identity')+
  theme_minimal()+
  theme(legend.position = 'none',
        plot.title = element_text(hjust = .85, vjust=0))+
  coord_flip()+
  scale_fill_manual(values = c('#0085ca','#97d700'))+
  labs(title='Ride Starts from Station between May 28st and July 1st', x = '', y = 'Total ride starts in date range')

h2<-ggplot(h)+ 
  geom_bar(aes(y=net,x=reorder(station_name, net),
               fill = stats5), stat = 'identity')+
  theme_minimal()+
  theme(legend.position = 'none',
        plot.title = element_text(hjust = .85, vjust=0))+
  coord_flip()+
  scale_fill_manual(values = c('#0085ca','#97d700'))+
  labs(title='Ride Starts from Station between May 28st and July 1st', x = '', y = 'Total ride starts in date range')


f<-st_drop_geometry(rbind(
  mutate(bike.Train, Legend = "Training"), 
  mutate(bike.Test, Legend = "Testing"))) %>%
  filter(station_name %in% c(h %>% filter(stats5 == 'y') %>% pull(station_name))) %>% 
  group_by(hour_unit, station_name) %>% 
  summarize(bikes_at_stat = bikes_at_stat, Legend = Legend, statname = station_name) %>% 
  ungroup() 

f$statname <- factor(f$statname, levels=h %>% filter(stats5 == 'y') %>% pull(station_name))


ggplot(f, aes(hour_unit,bikes_at_stat, colour = Legend)) + geom_line() +
  scale_colour_manual(values = palette2) + 
  geom_vline(data = mondays, aes(xintercept = monday)) +
  facet_wrap(~statname, ncol =1, scales = 'free')+
  labs(#title="Rideshare trips by week: November-December",
    #subtitle="Dotted lines for Thanksgiving & Christmas", 
    x="Starts per Hour", y="") +
  plotTheme() + theme(panel.grid.major = element_blank())    

plot_grid(h1,f1)

ggdraw() +
  draw_plot(h2,height = 2)




ggplot(h)+ 
  geom_bar(aes(y=totalDocks,x=reorder(station_name, total),
               fill = stats5), stat = 'identity')+
  theme_minimal()+
  theme(legend.position = 'none',
        plot.title = element_text(hjust = .85, vjust=0))+
  coord_flip()+
  scale_fill_manual(values = c('#0085ca','#97d700'))+
  labs(title='Ride Starts from Station between May 28st and July 1st', x = '', y = 'Total ride starts in date range')


##map

river <- area_water('PA', county = 'Philadelphia') %>%
  st_as_sf()%>%
  st_transform(crs=4326) %>% filter(AWATER > 10000) %>% 
  st_crop(y = st_bbox(stats))

ggplot()+
  geom_sf(data=river%>%  st_crop(y = bounds),color= NA, fill = 'blue')+
  geom_sf(data=h %>%  st_crop(y = bounds), aes(color = net), size =3)+
  scale_color_gradient2(mid = 'grey')+
  mapTheme()


anim.data <- bike.panel %>%  filter(week == 23)

rideshare_animation <-
  ggplot() +
  geom_sf(data=river,color= NA, fill = 'blue')+
  geom_sf(data = anim.data, aes(color = net_over_time, size = log(net_over_time) )) +
  scale_color_gradient2(mid = 'grey')+
  labs(title = "",
       subtitle = "60 minute intervals: {current_frame}") +
  transition_manual(hour_unit) +
  mapTheme()


bounds <- c(ymax = 39.98, ymin = 39.924, xmin =-75.211, xmax = -75.13)


rideshare_animation_b <-
  ggplot() +
  geom_sf(data=river%>%  st_crop(y = bounds),color= NA, fill = 'blue')+
  geom_sf(data = anim.data %>%  st_crop(y = bounds), aes(color = net_over_time, size = total_over_time)) +
  scale_color_gradient2(mid = 'grey')+
  labs(title = "",
       subtitle = "60 minute intervals: {current_frame}") +
  transition_manual(hour_unit) +
  mapTheme()

#dlist<-unique(anim.data$hour_unit)
#N<-length(dlist)


# g.progress<- function(i=10,maxi=N){
#   ggplot(data=data.frame(x="progress",y=i/maxi),
#          aes(x=x,y=y))+geom_bar(stat="identity",color=NA,fill="#0033a0",alpha=0.82)+
#     geom_bar(stat="identity", data=data.frame(x="progress",y=1),
#              color="black",fill=NA)+
#     theme_void()+scale_y_continuous(limits=c(0,1))+
#     theme(plot.title=element_text(size=8,hjust=0.1))+
#     labs(title="Animation progress")+
#     coord_flip()
#   }
# 
# i=1
# cowplot::plot_grid(anim.data,g.progress(i))

animate(rideshare_animation_b, duration=10, renderer = gifski_renderer())

### modeling and purrr 

bike.panel$hour <- bike.panel$hour_unit %>% hour()
bike.panel$day <- bike.panel$hour_unit %>% day()

bike.Train <- filter(bike.panel, week < 25)
bike.Test <- filter(bike.panel, week >= 25)

### net regressoins
reg1 <- lm(net ~  hour + dotw + Temperature, data=bike.Train)
reg2 <- lm(net ~  station_name + dotw + Temperature, data=bike.Train)
reg3 <- lm(net ~  station_name + hour + dotw + Temperature, data=bike.Train)
reg4 <- lm(net ~  station_name + hour + dotw + Temperature + 
             lagHour + lag2Hours + lag3Hours + lag12Hours + lag1day, data=bike.Train)

bike.Test.weekNest <- 
  as.data.frame(bike.Test) %>%
  nest(-week) 


model_pred <- function(dat, fit){
  pred <- predict(fit, newdata = dat)}

week_predictions <- 
  bike.Test.weekNest %>% 
  mutate(A_Time_FE = map(.x = data, fit = reg1, .f = model_pred),
         B_Space_FE = map(.x = data, fit = reg2, .f = model_pred),
         C_Space_Time_FE = map(.x = data, fit = reg3, .f = model_pred),
         D_Space_Time_Lags = map(.x = data, fit = reg4, .f = model_pred))



week_predictions <- week_predictions %>%  
  gather(Regression, Prediction, -data, -week) %>% 
  mutate(Observed = map(data, pull, net),
         Absolute_Error = map2(Observed, Prediction,  ~ abs(.x - .y)),
         MAE = map_dbl(Absolute_Error, mean),
         sd_AE = map_dbl(Absolute_Error, sd)) 


week_predictions %>%
  dplyr::select(week, Regression, MAE) %>%
  gather(Variable, MAE, -Regression, -week) %>%
  ggplot(aes(week, MAE)) + 
  geom_bar(aes(fill = Regression), position = "dodge", stat="identity") +
  scale_fill_manual(values = palette5) +
  labs(title = "Mean Absolute Errors by model specification and week") +
  plotTheme()


week_predictions %>% 
  mutate(hour_unit = map(data, pull, hour_unit),
         station_name = map(data, pull, station_name)) %>%
  dplyr::select(hour_unit, station_name, Observed, Prediction, Regression) %>%
  unnest() %>%
  gather(Variable, Value, -Regression, -hour_unit, -station_name) %>%
  group_by(Regression, Variable, hour_unit) %>%
  summarize(Value = mean(Value)) %>%
  ggplot(aes(hour_unit, Value, colour=Variable)) + geom_line(size = 1.1) + 
  facet_wrap(~Regression, ncol=1) +
  scale_colour_manual(values = palette2) +
  labs(title = "Mean Predicted/Observed ride share by hourly interval", 
       x = "Hour", y= "Rideshare Trips") +
  plotTheme()


## start count regressions 
reg1 <- lm(start_count ~  hour + dotw + Temperature, data=bike.Train)
reg2 <- lm(start_count ~  station_name + dotw + Temperature, data=bike.Train)
reg3 <- lm(start_count ~  station_name + hour + dotw + Temperature, data=bike.Train)
reg4 <- lm(start_count ~  station_name + hour + dotw + Temperature + 
             sclagHour + sclag2Hours + sclag3Hours + sclag12Hours + sclag1day, data=bike.Train)

bike.Test.weekNest <- 
  as.data.frame(bike.Test) %>%
  nest(-week) 


week_predictions <- 
  bike.Test.weekNest %>% 
  mutate(A_Time_FE = map(.x = data, fit = reg1, .f = model_pred),
         B_Space_FE = map(.x = data, fit = reg2, .f = model_pred),
         C_Space_Time_FE = map(.x = data, fit = reg3, .f = model_pred),
         D_Space_Time_Lags = map(.x = data, fit = reg4, .f = model_pred))



week_predictions <- week_predictions %>%  
  gather(Regression, Prediction, -data, -week) %>% 
  mutate(Observed = map(data, pull, start_count),
         Absolute_Error = map2(Observed, Prediction,  ~ abs(.x - .y)),
         MAE = map_dbl(Absolute_Error, mean),
         sd_AE = map_dbl(Absolute_Error, sd)) 


week_predictions %>%
  dplyr::select(week, Regression, MAE) %>%
  gather(Variable, MAE, -Regression, -week) %>%
  ggplot(aes(week, MAE)) + 
  geom_bar(aes(fill = Regression), position = "dodge", stat="identity") +
  scale_fill_manual(values = palette5) +
  labs(title = "Mean Absolute Errors by model specification and week") +
  plotTheme()


week_predictions %>% 
  mutate(hour_unit = map(data, pull, hour_unit),
         station_name = map(data, pull, station_name)) %>%
  dplyr::select(hour_unit, station_name, Observed, Prediction, Regression) %>%
  unnest(cols = c(hour_unit, station_name, Observed, Prediction)) %>%
  gather(Variable, Value, -Regression, -hour_unit, -station_name) %>%
  group_by(Regression, Variable, hour_unit) %>%
  summarize(Value = mean(Value)) %>%
  ggplot(aes(hour_unit, Value, colour=Variable)) + geom_line(size = 1.1) + 
  facet_wrap(~Regression, ncol=1) +
  scale_colour_manual(values = palette2) +
  labs(title = "Mean Predicted/Observed ride share by hourly interval", 
       x = "Hour", y= "Rideshare Trips") +
  plotTheme()

## cross validation

reg1 <- lm(start_count ~  hour(hour_unit) + dotw + Temperature, data=bike.Train)



#Neigborhood cross validation

spatial.cv.vars <- bike.panel %>% st_drop_geometry() %>% select(hour, dotw, Temperature,
                                                                sclagHour, sclag2Hours, sclag3Hours, sclag12Hours, sclag1day) %>% colnames()


spatial.cv <- crossValidate(
  dataset = bike.panel,
  id = "station_name",
  dependentVariable = "start_count",
  indVariables = spatial.cv.vars) %>%
  dplyr::select(cvID = station_name, start_count, Prediction, geometry)

temporal.cv.vars <- bike.panel %>% st_drop_geometry() %>% select(station_name, hour, dotw, Temperature,
                                                                 sclagHour, sclag2Hours, sclag3Hours, sclag12Hours, sclag1day) %>% colnames()


temporal.cv <- crossValidate(
  dataset = bike.panel,
  id = "day",
  dependentVariable = "start_count",
  indVariables = temporal.cv.vars) %>%
  dplyr::select(cvID = day, start_count, Prediction, geometry)

reg.summary <- 
  rbind(
    mutate(spatial.cv,           Error = Prediction - start_count,
           Regression = "Spatial CV"),
    
    mutate(temporal.cv,        Error = Prediction - start_count,
           Regression = "Temporal CV")) %>%
  st_sf() 

error_by_reg_and_fold <- 
  reg.summary %>%
  group_by(Regression, cvID) %>% 
  summarize(Mean_Error = mean(Prediction - start_count, na.rm = T),
            MAE = mean(abs(Mean_Error), na.rm = T),
            SD_MAE = mean(abs(Mean_Error), na.rm = T)) %>%
  ungroup()

error_by_reg_and_fold %>%
  ggplot(aes(MAE)) + 
  geom_histogram(bins = 30, colour="black", fill = "#FDE725FF") +
  facet_wrap(~Regression) +  
  geom_vline(xintercept = 0) + scale_x_continuous(breaks = seq(0, 8, by = 1)) + 
  labs(title="Distribution of MAE",
       x="Mean Absolute Error", y="Count") +
  plotTheme()


