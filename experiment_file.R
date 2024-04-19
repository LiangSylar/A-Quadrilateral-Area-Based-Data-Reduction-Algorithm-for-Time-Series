#### this file is used to do experiments on ts to produce Seg_TS
#### Requires the user to run the according functions in the file called
####    "all the functions and algorithms together" first 


# loading file and data --------------------------------------------------------------------------------------------------------------------------------------------------
setwd("E:/CSC Courses/CSC4008/Data") # set location
d1 = read.csv("pecan_1_min_part1.csv") # open file d1
date_time = as.character(d1$localminute)
date_c = substr(date_time,0,10)
date = as.Date(date_c,"%Y-%m-%d") #small y not work; because Y=2016 is y=16
time_c = substr(date_time,12,19)
d1$time = time_c
d1$date = date
out <- split(d1, f = d1$dataid)

k = 2  # k = 2, 3, 4, ..., 15
userk = out[[k]] 
ts = userk$use[userk$date==as.Date("2016-01-15","%Y-%m-%d")]
plot.ts(ts) # see what ts looks like

# The following codes compares the result of bottom-up algo and area algo
# the data object used is one day's electricity consumption of user k
k = 14  # k = 2, 3, 4, ..., 15
userk = out[[k]] 
ts = userk$use[userk$date==as.Date("2016-01-15","%Y-%m-%d")]
par(mfrow=c(1,1))
plot.ts(ts) # see what ts looks like
num_seg_lst = seq(800,400,-20)
len = length(num_seg_lst)

par(mfrow=c(1,1))
h = ts[1000:1050]
a <- bottom_up3(10,h)
plot_segments(a,h)
b <- area_algo3(10,h)
plot_segments(b,h)


### explore whether the length of ts correlates with score
end_lst <- seq(10, 1440, 50)
results <- array(0, length(end_lst))
i = 1
for(end in end_lst){
  seg <- ts[1:end]
  results_arry <- compute_scores(seg)
  results[i] <- sum(results_arry[,1] > results_arry[,2])/dim(results_arry)[1]
  i = i+1
}

plot(results, type="l")

# plot(num_seg_lst, results[,1], type="l") # bottom-up algo
# points(num_seg_lst, results[,2], type="l",col="red") # area algo

table <- rbind(end_lst, results)
write.csv(table,"length of ts.csv",row.names=F)




#### 
plot.ts(ts[1:200])
plot.ts(ts[400:600])
fts(ts[1:200])

### 
err_lst = compare_err(100, ts)
err_lst[1]/err_lst[2]


