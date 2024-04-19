# this file create features for the users from file d1 to d11
# need to run the function fts first

#### open file  ---------------------------------------------------------
setwd("E:/CSC Courses/CSC4008/Data") 
d1 = read.csv("pecan_1_min_part1.csv")
d2 = read.csv("pecan_1_min_part2.csv")
d3 = read.csv("pecan_1_min_part3.csv")
d4 = read.csv("pecan_1_min_part4.csv")
d5 = read.csv("pecan_1_min_part5.csv")
d6 = read.csv("pecan_1_min_part6.csv")
d7 = read.csv("pecan_1_min_part7.csv")
d8 = read.csv("pecan_1_min_part8.csv")
d9 = read.csv("pecan_1_min_part9.csv")
d10 = read.csv("pecan_1_min_part10.csv")
d11 = read.csv("pecan_1_min_part11.csv")

df = read.csv("target variable.csv")
df$idx <- seq(1,dim(df)[1])

# in file d2, k = 1 and k = 23 daily userk ts has length < 1000, so is dispensed. 
# in file d4, k = 15 is dispensed. 
# in file d6, k = 1 is dispensed.
# in file d7, k = 12 is dispensed. 
# in file d8, k = 19 is dispensed.
# in file d11,k = 48, k = 52, k = 53, k = 72

# features.obj = list()
# num_users.obj = c()
# compute features for each file -----------------------------------------------------
features_filed <- function(d, file_order){
  #### d = d9
  #### file_order = as.numeric()
  
  date_time = as.character(d$localminute)
  date_c = substr(date_time,0,10)
  date = as.Date(date_c,"%Y-%m-%d") #small y not work; because Y=2016 is y=16
  time_c = substr(date_time,12,19)
  d$time = time_c
  d$date = date
  out <- split(d, f = d$dataid) 
  num_users = length(unique(d$dataid)) #### num_users = 14
  
  # 
  h = df[,file_order]
  (users = df$idx[!is.na(h)])
  features <- matrix(NA, nrow = num_users, ncol=8)
  features[,1] = rep(file_order, num_users)
  features[users,2] = users
  
  for(k in users){
    userk = out[[k]]
    ts = userk$use[userk$date==userk$date[1]]
    #plot.ts(ts)
    features[k,-c(1,2)] = fts(ts)
  }
  return (features)
}

####

#### combine the features and target variable df -------------------------------------------
# features11 <- features_filed(d11,11)
y <- df$d11
data11 <- cbind(features11,y)

# features10 <- features_filed(d10,10)
y <- df$d10
data10 <- cbind(features10,y[1:dim(features10)[1]])

# features9 <- features_filed(d9,9)
y <- df$d9
data9 <- cbind(features9,y[1:dim(features9)[1]])

features8 <- features_filed(d8,8)
y <- df$d8
data8 <- cbind(features8,y[1:dim(features8)[1]])

features7 <- features_filed(d7,7)
y <- df$d7
data7 <- cbind(features7,y[1:dim(features7)[1]])

features6 <- features_filed(d6,6)
y <- df$d6
data6 <- cbind(features6,y[1:dim(features6)[1]])

features5 <- features_filed(d5,5)
y <- df$d5
data5 <- cbind(features5,y[1:dim(features5)[1]])


features4 <- features_filed(d4,4)
y <- df$d4
data4 <- cbind(features4,y[1:dim(features4)[1]])

features3 <- features_filed(d3,3)
y <- df$d3
data3 <- cbind(features3,y[1:dim(features3)[1]])

features2 <- features_filed(d2,2)
y <- df$d2
data2 <- cbind(features2,y[1:dim(features2)[1]])

features1 <- features_filed(d1,1)
y <- df$d1
data1 <- cbind(features1,y[1:dim(features1)[1]])


#### combind data1 to data11 ---------------------------------------
a <- rbind(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10,data11)
dim(a) #346 9
data_all <- as.data.frame(a)
# var,range,max/min/ave freq,noisiness
names(data_all) <- c("dx","userk","var","range","max freq","min freq","ave freq","noisiness","scores")

data_all2 <- data_all[!is.na(data_all$userk),] # clean the NA column
# write.csv(data_all2, "fitting data.csv", row.names=F)












