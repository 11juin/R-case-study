library("readxl")
data=read_excel('C:/Users/Jiaying/Downloads/Copy of PED-Weather-Data-2019_old.xlsx')
## get average values every 6 days for numerical features(except for wind)
blue_col_name = c("TMSTAMP","StnID","AvgWS","AvgWD","AvgAir_T","AvgRH","Pluvio_Rain")
blue_data = data[,blue_col_name]
i1 = apply(blue_data, 1, function(x) all(is.na(x)))
split_blue = split(blue_data[!i1,], cumsum(i1)[!i1])
mean_func <- function(data) {
  data = data.frame(data)
  data = data[-1,]
  n=6
  group = list(gl(ceiling(nrow(data)/n), n)[1:nrow(data)])
  green_col_name = c("AvgWS","AvgAir_T","AvgRH","Pluvio_Rain")
  green_data = aggregate(data[,green_col_name],group,function(x) c(mean = round(mean(x), 1)))[-1]
  return(green_data)
}
avg_real =  lapply(split_blue, mean_func)

## get average values every 6 days for string features(wind)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
wind_data = data[-1,13]
mode_wind_func <- function(wind_data) {
  data = data.frame(wind_data)
  n=6
  group = list(gl(ceiling(nrow(data)/n), n)[1:nrow(data)])
  green_col_name = c("AvgWS","AvgAir_T","AvgRH","Pluvio_Rain")
  green_data = aggregate(data,group,function(x) getmode(x))[-1]
  colnames(green_data) = "wind_avg"
  return(green_data)
}
i2 = apply(wind_data, 1, function(x) all(is.na(x)))
split_wind = split(wind_data[!i2,], cumsum(i2)[!i2])
avg_wind =  lapply(split_wind, mode_wind_func)

## export green data
green_data_temp = do.call(rbind, Map(data.frame, A=avg_real, B=avg_wind))
green_data_temp = green_data_temp[,c(1,5,2,3,4)]
green_data_final <- rbind(green_data_temp[1:nrow(avg_wind[[1]]), ],rep(NA,ncol(green_data_temp)), green_data_temp[(nrow((avg_wind[[1]]))+1):nrow(green_data_temp), ])
write.table(green_data_final,"jiaying_green_export.csv", sep = ',',row.names=FALSE)






