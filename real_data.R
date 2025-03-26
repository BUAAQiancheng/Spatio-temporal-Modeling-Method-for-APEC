###### 一、total_data ######
source("beta&summary.R") 
source("lamda.R")
source("profile.R")
source("lasso.R")

lon_lat_data <- read.csv('station_order.csv')
total_data <- read.csv('total_data.csv')

library(xts)
time <- as.POSIXct(paste(total_data$year, total_data$month, total_data$day, 
                         total_data$hour, sep = "-"), format = "%Y-%m-%d-%H")
total_data$time <- time

library(dplyr)  
integrated_data <- total_data %>%
  left_join(lon_lat_data, by = c("name" = "station_name")) %>%
  mutate(HEAT_B = ifelse((province == "Beijing" & 
                            time >= as.POSIXct("2014-11-15 00:00:00")),
                         1, 0),
         HEAT_H = ifelse((province == "Hebei" & 
                            time >= as.POSIXct("2014-11-15 00:00:00")),
                         1, 0),
         HEAT_T = ifelse((province == "Tianjin" & 
                            time >= as.POSIXct("2014-11-01 00:00:00")),
                         1, 0),
         B1 = ifelse(province == "Beijing" & 
                       time >= as.POSIXct("2014-11-03 00:00:00") & 
                       time <= as.POSIXct("2014-11-03 23:00:00"), 
                     1, 0),
         B2 = ifelse(province == "Beijing" & 
                       time >= as.POSIXct("2014-11-04 00:00:00") & 
                       time <= as.POSIXct("2014-11-04 23:00:00"), 
                     1, 0),
         B3 = ifelse(province == "Beijing" & 
                       time >= as.POSIXct("2014-11-05 00:00:00") & 
                       time <= as.POSIXct("2014-11-05 23:00:00"), 
                     1, 0),
         B4 = ifelse(province == "Beijing" & 
                       time >= as.POSIXct("2014-11-06 00:00:00") & 
                       time <= as.POSIXct("2014-11-06 23:00:00"), 
                     1, 0),
         B5 = ifelse(province == "Beijing" & 
                       time >= as.POSIXct("2014-11-07 00:00:00") & 
                       time <= as.POSIXct("2014-11-07 23:00:00"), 
                     1, 0),
         B6 = ifelse(province == "Beijing" & 
                       time >= as.POSIXct("2014-11-08 00:00:00") & 
                       time <= as.POSIXct("2014-11-08 23:00:00"), 
                     1, 0),
         B7 = ifelse(province == "Beijing" & 
                       time >= as.POSIXct("2014-11-09 00:00:00") & 
                       time <= as.POSIXct("2014-11-09 23:00:00"), 
                     1, 0),
         B8 = ifelse(province == "Beijing" & 
                       time >= as.POSIXct("2014-11-10 00:00:00") & 
                       time <= as.POSIXct("2014-11-10 23:00:00"), 
                     1, 0),
         B9 = ifelse(province == "Beijing" & 
                       time >= as.POSIXct("2014-11-11 00:00:00") & 
                       time <= as.POSIXct("2014-11-11 23:00:00"), 
                     1, 0),
         B10 = ifelse(province == "Beijing" & 
                        time >= as.POSIXct("2014-11-12 00:00:00") & 
                        time <= as.POSIXct("2014-11-12 23:00:00"), 
                      1, 0),
         B11 = ifelse(province == "Beijing" & 
                        time >= as.POSIXct("2014-11-13 00:00:00") & 
                        time <= as.POSIXct("2014-11-15 23:00:00"), 
                      1, 0),
         H1 = ifelse(province == "Hebei" & 
                       time >= as.POSIXct("2014-11-03 00:00:00") & 
                       time <= as.POSIXct("2014-11-03 23:00:00"), 
                     1, 0),
         H2 = ifelse(province == "Hebei" & 
                       time >= as.POSIXct("2014-11-04 00:00:00") & 
                       time <= as.POSIXct("2014-11-04 23:00:00"), 
                     1, 0),
         H3 = ifelse(province == "Hebei" & 
                       time >= as.POSIXct("2014-11-05 00:00:00") & 
                       time <= as.POSIXct("2014-11-05 23:00:00"), 
                     1, 0),
         H4 = ifelse(province == "Hebei" & 
                       time >= as.POSIXct("2014-11-06 00:00:00") & 
                       time <= as.POSIXct("2014-11-06 23:00:00"), 
                     1, 0),
         H5 = ifelse(province == "Hebei" & 
                       time >= as.POSIXct("2014-11-07 00:00:00") & 
                       time <= as.POSIXct("2014-11-07 23:00:00"), 
                     1, 0),
         H6 = ifelse(province == "Hebei" & 
                       time >= as.POSIXct("2014-11-08 00:00:00") & 
                       time <= as.POSIXct("2014-11-08 23:00:00"), 
                     1, 0),
         H7 = ifelse(province == "Hebei" & 
                       time >= as.POSIXct("2014-11-09 00:00:00") & 
                       time <= as.POSIXct("2014-11-09 23:00:00"), 
                     1, 0),
         H8 = ifelse(province == "Hebei" & 
                       time >= as.POSIXct("2014-11-10 00:00:00") & 
                       time <= as.POSIXct("2014-11-10 23:00:00"), 
                     1, 0),
         H9 = ifelse(province == "Hebei" & 
                       time >= as.POSIXct("2014-11-11 00:00:00") & 
                       time <= as.POSIXct("2014-11-11 23:00:00"), 
                     1, 0),
         H10 = ifelse(province == "Hebei" & 
                        time >= as.POSIXct("2014-11-12 00:00:00") & 
                        time <= as.POSIXct("2014-11-12 23:00:00"), 
                      1, 0),
         H11 = ifelse(province == "Hebei" & 
                        time >= as.POSIXct("2014-11-13 00:00:00") & 
                        time <= as.POSIXct("2014-11-15 23:00:00"), 
                      1, 0),
         
         T1 = ifelse(province == "Tianjin" & 
                       time >= as.POSIXct("2014-11-03 00:00:00") & 
                       time <= as.POSIXct("2014-11-03 23:00:00"), 
                     1, 0),
         T2 = ifelse(province == "Tianjin" & 
                       time >= as.POSIXct("2014-11-04 00:00:00") & 
                       time <= as.POSIXct("2014-11-04 23:00:00"), 
                     1, 0),
         T3 = ifelse(province == "Tianjin" & 
                       time >= as.POSIXct("2014-11-05 00:00:00") & 
                       time <= as.POSIXct("2014-11-05 23:00:00"), 
                     1, 0),
         T4 = ifelse(province == "Tianjin" & 
                       time >= as.POSIXct("2014-11-06 00:00:00") & 
                       time <= as.POSIXct("2014-11-06 23:00:00"), 
                     1, 0),
         T5 = ifelse(province == "Tianjin" & 
                       time >= as.POSIXct("2014-11-07 00:00:00") & 
                       time <= as.POSIXct("2014-11-07 23:00:00"), 
                     1, 0),
         T6 = ifelse(province == "Tianjin" & 
                       time >= as.POSIXct("2014-11-08 00:00:00") & 
                       time <= as.POSIXct("2014-11-08 23:00:00"), 
                     1, 0),
         T7 = ifelse(province == "Tianjin" & 
                       time >= as.POSIXct("2014-11-09 00:00:00") & 
                       time <= as.POSIXct("2014-11-09 23:00:00"), 
                     1, 0),
         T8 = ifelse(province == "Tianjin" & 
                       time >= as.POSIXct("2014-11-10 00:00:00") & 
                       time <= as.POSIXct("2014-11-10 23:00:00"), 
                     1, 0),
         T9 = ifelse(province == "Tianjin" & 
                       time >= as.POSIXct("2014-11-11 00:00:00") & 
                       time <= as.POSIXct("2014-11-11 23:00:00"), 
                     1, 0),
         T10 = ifelse(province == "Tianjin" & 
                        time >= as.POSIXct("2014-11-12 00:00:00") & 
                        time <= as.POSIXct("2014-11-12 23:00:00"), 
                      1, 0),
         T11 = ifelse(province == "Tianjin" & 
                        time >= as.POSIXct("2014-11-13 00:00:00") & 
                        time <= as.POSIXct("2014-11-15 23:00:00"), 
                      1, 0))

data.total <- data.frame(station=integrated_data$name,
                         province=integrated_data$province,
                         id=integrated_data$ID,
                         time=integrated_data$time,
                         PM2.5=integrated_data$PM2.5,
                         TEMP=integrated_data$TEMP,
                         HUMI=integrated_data$HUMI,
                         HEAT_B=integrated_data$HEAT_B,
                         HEAT_H=integrated_data$HEAT_H,
                         HEAT_T=integrated_data$HEAT_T,
                         
                         B1=integrated_data$B1,
                         B2=integrated_data$B2,
                         B3=integrated_data$B3,
                         B4=integrated_data$B4,
                         B5=integrated_data$B5,
                         B6=integrated_data$B6,
                         B7=integrated_data$B7,
                         B8=integrated_data$B8,
                         B9=integrated_data$B9,
                         B10=integrated_data$B10,
                         B11=integrated_data$B11,
                         H1=integrated_data$H1,
                         H2=integrated_data$H2,
                         H3=integrated_data$H3,
                         H4=integrated_data$H4,
                         H5=integrated_data$H5,
                         H6=integrated_data$H6,
                         H7=integrated_data$H7,
                         H8=integrated_data$H8,
                         H9=integrated_data$H9,
                         H10=integrated_data$H10,
                         H11=integrated_data$H11,
                         T1=integrated_data$T1,
                         T2=integrated_data$T2,
                         T3=integrated_data$T3,
                         T4=integrated_data$T4,
                         T5=integrated_data$T5,
                         T6=integrated_data$T6,
                         T7=integrated_data$T7,
                         T8=integrated_data$T8,
                         T9=integrated_data$T9,
                         T10=integrated_data$T10,
                         T11=integrated_data$T11,
                         
                         nearfar1=integrated_data$NearFar2.x,
                         nearfar2=integrated_data$NearFar3.x,
                         nearfar3=integrated_data$NearFar4.x,
                         nearfar4=integrated_data$NearFar5.x,
                         nearfar5=integrated_data$NearFar6.x,
                         nearfar6=integrated_data$NearFar7.x,
                         nearfar7=integrated_data$NearFar8.x,
                         nearfar8=integrated_data$NearFar9.x,
                         nearfar9=integrated_data$NearFar10.x,
                         nearfar10=integrated_data$NearFar11.x,
                         nearfar11=integrated_data$NearFar12.x,
                         nearfar12=integrated_data$NearFar13.x,
                         nearfar13=integrated_data$NearFar14.x,
                         nearfar14=integrated_data$NearFar15.x
) 


###### 二、数据预处理（缺失值插补+季节性差分） ######
#---- (1)PM25数据 ----
# set.seed(123)
library(tidyverse)
PM25 <- data.total %>% 
  select(station, time, PM2.5, id, nearfar1, nearfar2, nearfar3, nearfar4, nearfar5,
         nearfar6,nearfar7,nearfar8,nearfar9,nearfar10,nearfar11,nearfar12,nearfar13,nearfar14) %>%
  group_by(time) %>% 
  mutate() %>%  
  spread(key = time, value = PM2.5) %>%
  ungroup() 
PM25 <- as.data.frame(PM25)

#---- (2)缺失率 ----
# a.缺失率>80%的index
ind <- NULL

result_table <- data.frame(
  Station = character(nrow(PM25)),
  Missing_Rate = numeric(nrow(PM25))
)# 所有站点的缺失率

for (i in 1:nrow(PM25)) {
  missrate <- sum(is.na(PM25[i, 17:ncol(PM25)])) / 8760  
  if (missrate >= 0.8){ind <- c(ind, i)}
  result_table$Station[i] <- PM25$station[i]
  result_table$Missing_Rate[i] <- missrate
}

PM25.ind <- PM25[-ind,]

# b.插补
for (i in 1:nrow(PM25.ind)) {
  # 处理单个时间点缺失的情况
  for (j in 18:ncol(PM25.ind)-1) {
    if (is.na(PM25.ind[i, j])) {
      
      # 如果前后相邻时间点数据不缺失，则取平均值填补
      if (!is.na(PM25.ind[i, j - 1]) && !is.na(PM25.ind[i, j + 1])) {
        PM25.ind[i, j] <- (PM25.ind[i, j - 1] + PM25.ind[i, j + 1]) / 2
      }
    }
  }
  print(i)
}


# 对于多个连续缺失的情况&最后一列缺失&第一列缺失
for (i in 1:nrow(PM25.ind)) {
  for (j in 17:ncol(PM25.ind)) {
    # 检查缺失数据
    if (is.na(PM25.ind[i, j])) {
      time_point <- colnames(PM25.ind)[j]
      
      # nearfar1 ～ nearfar10的平均值插补
      near_values <- c()
      for (t in 1:14) {
        near_station <- data.total[data.total$id == PM25.ind[[paste0("nearfar", t)]][i] & 
                                     data.total$time == time_point, "PM2.5"]
        near_values <- c(near_values, near_station)
      }
      PM25.ind[i, j] <- mean(near_values, na.rm = TRUE)
    }
  }
  print(i)
}

#---- (3)季节性差分 ----
# 季节性检验
library(tseries)
kpss.test(as.numeric(PM25.ind[1,17:8776]))# p<0.01 非平稳

# 将数据转换为长格式
library(tidyverse)
data_long <- PM25.ind[,-(3:16)] %>%
  pivot_longer(cols =  -c(station, id), names_to = "time", values_to = "PM2.5")

# 添加季节
time <- seq.POSIXt(from = as.POSIXct("2014-01-01 00:00:00"), by = "hour", length.out = 8760)
data_long <- data_long %>%
  mutate(season = case_when(
    (time >= as.POSIXct("2014-03-01 00:00:00") & time <= as.POSIXct("2014-05-31 23:00:00")) ~ 1,
    (time >= as.POSIXct("2014-06-01 00:00:00") & time <= as.POSIXct("2014-08-31 23:00:00")) ~ 2,
    (time >= as.POSIXct("2014-09-01 00:00:00") & time <= as.POSIXct("2014-11-30 23:00:00")) ~ 3,
    (time >= as.POSIXct("2014-01-01 00:00:00") & time <= as.POSIXct("2014-02-28 23:00:00")) ~ 4,
    (time >= as.POSIXct("2014-12-01 00:00:00") & time <= as.POSIXct("2014-12-31 23:00:00")) ~ 5
  ))

# 季节均值调整
data_long <- data_long %>%
  group_by(station, season) %>%
  mutate(mean_PM2.5 = mean(PM2.5, na.rm = TRUE)) %>%
  ungroup()

data_long <- data_long %>%
  mutate(newPM2.5 = PM2.5 - mean_PM2.5)

PM25_mean <- data_long %>%
  select(station, time, newPM2.5, id) %>%
  spread(key = time, value = newPM2.5) %>%
  arrange(id) %>% 
  ungroup() 
PM25_mean[44,6954]=(PM25_mean[44,6953]+PM25_mean[44,6955])/2
PM25_mean[46,6954]=(PM25_mean[46,6953]+PM25_mean[46,6955])/2

temp = data_long$PM2.5  # 原始PM2.5
data_long$PM2.5 = temp  # 恢复原始数据


# log+季节调整
data_long$PM2.5<-log(data_long$PM2.5) # 取对数
data_long <- data_long %>%
  group_by(station, season) %>%
  mutate(mean_PM2.5 = mean(PM2.5, na.rm = TRUE)) %>%
  ungroup()

data_long <- data_long %>%
  mutate(newPM2.5 = PM2.5 - mean_PM2.5)

PM25_mean <- data_long %>%
  select(station, time, newPM2.5, id) %>%
  spread(key = time, value = newPM2.5) %>%
  arrange(id) %>% 
  ungroup()



# 季节性检验
kpss.test(as.numeric(PM25_mean[46,3:8762])) # p>0.1 平稳

#---- (4)yy,pp,nn ----
### 现在共有72个站点
### 即pp = 72,nn = 8760
dayind = c(6553:8760)
yy <- as.matrix(PM25.ind[,-(1:16)])

yy <- as.matrix(PM25_mean[,-(1:2)])

yy[44,6952] = mean(yy[44,6951]+yy[44,6953])
yy[46,6952] = mean(yy[46,6951]+yy[46,6953])
# yy = log(yy)
pp <- nrow(yy) # 72
nn <- ncol(yy) # 8760

## 绘制时序图
dates = colnames(PM25.ind[,-(1:16)])
index = c(which(dates=='2014-11-03'),which(dates=='2014-11-16'))
date = seq(as.Date("2014-11-03"), as.Date("2014-11-15"), by = "1 day")
date = format(date, "%m-%d")

# beijing
beijing_pm = apply(yy[1:11,],2,mean)
data = beijing_pm[index[1]:(index[2]-1)]
days =(index[2]-index[1])/24 
plot(1:(24*days),data,'l',xaxt='n') 
axis(1, at = (0:(days-1))*24+1, labels = date)
abline()

# heibei
hebei_pm = apply(yy[12:61,],2,mean)
data = hebei_pm[index[1]:(index[2]-1)]
days =(index[2]-index[1])/24 
plot(1:(24*days),data,'l',xaxt='n') 
axis(1, at = (0:(days-1))*24+1, labels = date)

# tianjin
tianjin_pm = apply(yy[62:72,],2,mean)
data = tianjin_pm[index[1]:(index[2]-1)]
days =(index[2]-index[1])/24 
plot(1:(24*days),data,'l',xaxt='n') 
axis(1, at = (0:(days-1))*24+1, labels = date)



###### 三、生成WW ######
#---- (1)读取72个站点的经纬度 ----
lon_lat <- matrix(0, nrow = 72, ncol = 2)

for (s in 1:72) {
  for (t in 1:93){
    if (PM25_mean$station[s] == lon_lat_data$station_name[t]){
      lon_lat[s,] <- c(lon_lat_data$pollution_station_lon[t], lon_lat_data$pollution_station_lat[t])
      print(paste(PM25_mean$station[s], '=', lon_lat_data$station_name[t]))
    }
  }
}

rownames(lon_lat) <- PM25_mean$station
colnames(lon_lat) <- c('lon','lat')

#---- (2)逆距离设置W ----
# 计算逆距离
library(geosphere)
WW <- matrix(0, nrow = pp, ncol = pp)
rownames(WW) <- PM25_mean$station
colnames(WW) <- PM25_mean$station

for (ss in 1:(nrow(lon_lat)-1)) {
  for(tt in (ss+1):nrow(lon_lat)){
    distance <- distGeo(lon_lat[ss,], lon_lat[tt,])
    if(distance==0){
      WW[ss,tt] <- 0; WW[tt,ss] <- 0}
    else{
      WW[ss,tt] <- 1/distance;WW[tt,ss] <- 1/distance}
  }
}

for (i in 1:pp) {
  WW[i, ] <- WW[i, ] / sum(WW[i, ])
}

##### 四、生成zz #####
#  TEMP & HUMI & Z1 - Z9
library(zoo)
library(dplyr)

zz_data <- data.total %>% 
  select(time,station,id,TEMP,HUMI,HEAT_B,HEAT_H,HEAT_T,B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,
         H1,H2,H3,H4,H5,H6,H7,H8,H9,H10,H11,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11) %>%
  group_by(station) %>%
  mutate(TEMP = na.approx(TEMP), # 缺失值线性插补
         HUMI = na.approx(HUMI)) %>%
  arrange(time,id) %>% # 排列
  filter(station %in% PM25.ind$station)
# 输出
#write.csv(zz_data, file = 'zz_data_with_Z9.csv') # zz_data

zz_data <- as.matrix(zz_data[,-(1:3)])

# 3维矩阵
dd <- 38 # temp humi heat3 + 11*3
zz <- array(0, dim = c(pp, dd, nn))
for (ii in 1:nn) {
  z_t <- zz_data[((ii-1) * pp + 1):((ii) * pp),] #从‘2014-01-01 00:00:00’开始
  zz[, , ii] <- z_t
}

##### 五、lamda的估计 #####
#---- (1)serie & param ----
serie <- list(yy=yy, nn=nn, zz=zz)
param <- list(pp=pp, dd=dd, WW=WW)

#---- (2)估计lamda ---- 
Yab <- profile(serie = serie, param = param)

# lamda.hat
lamda.hat <- lamda.estimation(Yab = Yab, nn=nn, param = param) 
#write.csv(lamda.hat, file = "./数据2/lamda_hat_new.csv", row.names = F)

##### 六、beta的估计 #####
np0 <- nn*pp
yy.star <- matrix(0,nrow = np0, ncol = 1)
for (t in 1:(nn-1)) {  
  for (i in 1:pp) {
    yy.star[pp*(t-1)+i,1] <- yy[i,t+1]-lamda.hat[i,1]*WW[i,]%*%yy[,t+1]-lamda.hat[i,2]*yy[i,t]-lamda.hat[i,3]*WW[i,]%*%yy[,t]
  }
}
yy1 = yy.star
zz1 = zz_data
set.seed(123)
result1 = lasso(zz1,yy1,c(3,5,13,16),T)
result2 = lasso(zz1,yy1,c(3,5,13,16))
