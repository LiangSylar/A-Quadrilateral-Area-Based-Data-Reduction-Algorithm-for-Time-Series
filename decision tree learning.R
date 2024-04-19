# study on features of d1  ~ d11, electricity consumption

setwd("E:/CSC Courses/CSC4008/Data") # set location
d1 = read.csv("pecan_1_min_part1.csv") # open file d1
date_time = as.character(d1$localminute)
date_c = substr(date_time,0,10)
date = as.Date(date_c,"%Y-%m-%d") #small y not work; because Y=2016 is y=16
time_c = substr(date_time,12,19)
d1$time = time_c
d1$date = date
out <- split(d1, f = d1$dataid)


# algorithm -----------------------------------------------
k = 13
userk = out[[k]] 
ts = userk$use[userk$date==unique(userk$date)[1]]
ts = ts[1000:1050]
par(mfrow=c(1,1))
plot.ts(ts) # see what ts looks like
min_pts <- 100
a <- bottom_up3(50,ts)
plot_segments(a,ts)


finished_seg <- decision_tree(4, 100) # k = 4, min_point = 100



# start while loop -------------------------------------------------------------------


# input: k = 1:10

for(k in 1:10){
  finished_seg <- decision_tree(k, 100)
  
}



ts <- ts[20:40]
plot(ts)


a <- area_algo3(20,ts)
plot_segments(a,ts)






