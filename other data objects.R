setwd("E:/CSC Courses/CSC4008/Data/ucl_ts")

# first data obj in d1
d1 <- read.csv("CinCECGTorso.csv",header = F)
r1 <- d1[1,-1]
ts <-as.numeric(r1)
plot.ts(ts)

lst <- 2^seq(2,7)
num_seg_lst <- length(r1)/lst
len = length(num_seg_lst)
results = matrix(0, nrow = len, ncol=2)
i = 1
for(max_seg in num_seg_lst){
  results[i,] = compare_err(max_seg, ts)
  i = i+1
}
print(results)
plot(num_seg_lst,results[,1],type="l")
points(num_seg_lst,results[,2],type="l",col="red")



# first data obj in d2
d2 <- read.csv("HandOutlines.csv",header = F)
r2 <- d2[1,-1]
ts <-as.numeric(r2)
plot.ts(ts)

lst <- 2^seq(2,7)
num_seg_lst <- length(r1)/lst
len = length(num_seg_lst)
results = matrix(0, nrow = len, ncol=2)
i = 1
for(max_seg in num_seg_lst){
  results[i,] = compare_err(max_seg, ts)
  i = i+1
}
print(results)
plot(num_seg_lst,results[,1],type="l")
points(num_seg_lst,results[,2],type="l",col="red")


# first data obj in d3
d3 <- read.csv("Haptics.csv",header=F)
ts <- as.numeric(d3[1,-1])
plot.ts(ts)

lst <- 2^seq(2,7)
num_seg_lst <- length(r1)/lst
len = length(num_seg_lst)
results = matrix(0, nrow = len, ncol=2)
i = 1
for(max_seg in num_seg_lst){
  results[i,] = compare_err(max_seg, ts)
  i = i+1
}
print(results)
plot(num_seg_lst,results[,1],type="l")
points(num_seg_lst,results[,2],type="l",col="red")


# first data obj in d4
d4 <- read.csv("HouseTwenty.csv",header=F)
ts <- as.numeric(d4[1,-1])
plot.ts(ts)

lst <- 2^seq(2,7)
num_seg_lst <- length(r1)/lst
len = length(num_seg_lst)
results = matrix(0, nrow = len, ncol=2)
i = 1
for(max_seg in num_seg_lst){
  results[i,] = compare_err(max_seg, ts)
  i = i+1
}
print(results)
plot(num_seg_lst,results[,1],type="l")
points(num_seg_lst,results[,2],type="l",col="red")

# first data obj in d5
d5 <- read.csv("Mallat.csv",header=F)
ts <- as.numeric(d5[1,-1])
plot.ts(ts)

lst <- 2^seq(2,7)
num_seg_lst <- length(r1)/lst
len = length(num_seg_lst)
results = matrix(0, nrow = len, ncol=2)
i = 1
for(max_seg in num_seg_lst){
  results[i,] = compare_err(max_seg, ts)
  i = i+1
}
print(results)
plot(num_seg_lst,results[,1],type="l")
points(num_seg_lst,results[,2],type="l",col="red")

# first data obj in d6
d6 <- read.csv("MixedShapesSmallTrain.csv",header=F)
ts <- as.numeric(d6[1,-1])
plot.ts(ts)

lst <- 2^seq(2,7)
num_seg_lst <- length(r1)/lst
len = length(num_seg_lst)
results = matrix(0, nrow = len, ncol=2)
i = 1
for(max_seg in num_seg_lst){
  results[i,] = compare_err(max_seg, ts)
  i = i+1
}
print(results)
plot(num_seg_lst,results[,1],type="l")
points(num_seg_lst,results[,2],type="l",col="red")


# first data obj in d7 ---------------------------------------------------
d7 <- read.csv("PigCVP.csv",header=F)
ts <- as.numeric(d7[1,-1])
plot.ts(ts)

lst <- 2^seq(2,7)
num_seg_lst <- length(r1)/lst
len = length(num_seg_lst)
results = matrix(0, nrow = len, ncol=2)
i = 1
for(max_seg in num_seg_lst){
  results[i,] = compare_err(max_seg, ts)
  i = i+1
}
print(results)
plot(num_seg_lst,results[,1],type="l")
points(num_seg_lst,results[,2],type="l",col="red")



# first data obj in d8 ---------------------------------------------------
d8 <- read.csv("SemgHandSubjectCh2.csv",header=F)
ts <- as.numeric(d8[1,-1])
plot.ts(ts)

lst <- 2^seq(2,7)
num_seg_lst <- length(r1)/lst
len = length(num_seg_lst)
results = matrix(0, nrow = len, ncol=2)
i = 1
for(max_seg in num_seg_lst){
  results[i,] = compare_err(max_seg, ts)
  i = i+1
}
print(results)
plot(num_seg_lst,results[,1],type="l")
points(num_seg_lst,results[,2],type="l",col="red")
#### result not good; why? 

# first data obj in d9 ---------------------------------------------------
d9 <- read.csv("UWaveGestureLibraryAll.csv",header=F)
ts <- as.numeric(d9[1,-1])
plot.ts(ts)

lst <- 2^seq(2,7)
num_seg_lst <- length(r1)/lst
len = length(num_seg_lst)
results = matrix(0, nrow = len, ncol=2)
i = 1
for(max_seg in num_seg_lst){
  results[i,] = compare_err(max_seg, ts)
  i = i+1
}
print(results)
plot(num_seg_lst,results[,1],type="l")
points(num_seg_lst,results[,2],type="l",col="red")


# first data obj in d10 ---------------------------------------------------
d10 <- read.csv("Worms.csv",header=F)
ts <- as.numeric(d10[1,-1])
plot.ts(ts)

lst <- 2^seq(2,7)
num_seg_lst <- length(r1)/lst
len = length(num_seg_lst)
results = matrix(0, nrow = len, ncol=2)
i = 1
for(max_seg in num_seg_lst){
  results[i,] = compare_err(max_seg, ts)
  i = i+1
}
print(results)
plot(num_seg_lst,results[,1],type="l")
points(num_seg_lst,results[,2],type="l",col="red")





