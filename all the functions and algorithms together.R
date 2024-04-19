##### this file stores all functions and algorithm

# A function to plot the segments -----------------------------------------------------------------------------------------------------------------------------------
# input: segmented data object (Seg_TS); original data object (ts)
# output: a plot of the Seg_TS w.r.t to the original data ts
plot_segments <- function(Seg_TS, ts){
  len_s = dim(Seg_TS)[1]
  plot(ts,type="l")
  for(i in 1:len_s){
    start = Seg_TS[i,1]
    end = Seg_TS[i,2]
    y = ts[start:end]
    len_m = length(y)
    x = seq(start,end)
    lmobj = lm(y~x)
    points(x,lmobj$fitted.values,type = "l",col="red")
  }
}




# A function to compute the overall MSE for Seg_TS ------------------------------------------------------------------------------------------------------
# input: Seg_TS 
# output: the overall MSE loss of the Seg_TS w.r.t original data set ts
MSE_loss <- function(Seg_TS,ts){
  total_MSE = array(0,dim=dim(Seg_TS)[1])
  for(i in 1:dim(Seg_TS)[1]){
    start = Seg_TS[i,1]
    end = Seg_TS[i,2]
    y = ts[start:end]
    len_y = length(y)
    x = seq(1:len_y)
    lmobj = lm(y~x)
    total_MSE[i] = sum((lmobj$residuals)^2)
  }
  return (sum(total_MSE)/(length(ts)))
}




# bottom-up method ------------------------------------------------------------------------------------------------------------------------------------------
# input: max_error, original data object(ts)
# output: Seg_TS
bottom_up <- function(max_error, ts){
  
  # create Seg_TS
  len = length(ts)
  num_seg = ceiling(len/2)
  if (num_seg%%2 == 1){
    num_seg+1;
    ts = c(ts,0)
  }
  len = length(ts) # update length of ts
  Seg_TS = matrix(0, nrow = num_seg, ncol = 2)
  dim(Seg_TS)  # 61 2
  # create initial fine approximation 
  for (i in 1:num_seg){
    Seg_TS[i,1] = (i-1)*2+1
    Seg_TS[i,2] = i*2
  }
  
  # find cost of merging each pair of segments 
  merge_cost = array(0,dim(Seg_TS)[1]-1)
  for(i in 1:(num_seg-1)){
    start = Seg_TS[i,1]
    end = Seg_TS[i+1,2]
    y = ts[start:end]
    len_m = length(y)
    x = seq(1:len_m)
    lmobj = lm(y~x)
    merge_cost[i] = sum((lmobj$residuals)^2)/len_m
  }
  # merge_cost
  
  # while loop: find minimum merge cost, merge and create new seg
  while (min(merge_cost) < max_error){
    min_Mcost = min(merge_cost)
    index = which(min_Mcost == merge_cost)[[1]]
    # print(index)
    if(length(merge_cost)==1){
      print("hahaha")
      break
    }
    if (index == dim(Seg_TS)[1]) { 
      # This if statement for debugging only;
      # usually cannot have index == dim(Seg_TS)[1]!!!
      print(index)
      print(min_Mcost)
      print(max_error)
      break
    }
    Seg_TS[index,2] = Seg_TS[index+1,2] # merge Seg_TS[index] and Seg_TS[index+1]
    Seg_TS = Seg_TS[-(index+1),] # delete Seg_TS[index+1]
    
    # compute merge cost for Seg_TS(index) and Seg_TS(index+1)
    #   and update the cost to merge_cost[index]
    #   note: make sure that index < num_Seg-1
    if (index < dim(Seg_TS)[1]){
      start = Seg_TS[index,1]
      end = Seg_TS[index+1,2] 
      y = ts[start:end]
      #print(y)
      len_m = length(y)
      x = seq(1:len_m)
      lmobj = lm(y~x)
      merge_cost[index] = sum((lmobj$residuals)^2)/len_m
    }
    
    
    # compute merge cost for seg_TS(index-1) and Seg_TS(index)
    #   and update the cost to merge_cost[index-1]
    #   note: make sure that index > 1, otherwise out of index. 
    if (index > 1){
      start = Seg_TS[index-1,1] 
      #print(start)
      end = Seg_TS[index,2]
      #print(end)
      y = ts[start:end]
      #print(y) # issue here!!!
      len_m = length(y)
      x = seq(1:len_m)
      lmobj = lm(y~x)
      merge_cost[index] = sum((lmobj$residuals)^2)/len_m 
    }
    
    # delete merge_cost[index+1]
    if (index < (length(merge_cost))){
      merge_cost = merge_cost[-(index+1)]
    }
    else{
      merge_cost = merge_cost[-(index-1)]
    }
    # print(dim(Seg_TS)[1])
    
  }
  return(Seg_TS)
}



# bottom-up method ------------------------------------------------------------------------------------------------------------------------------------------
# this algorithm uses max_num_seg as criteria 
# input: max_num_seg, original data object(ts)
# output: Seg_TS
bottom_up3 <- function(max_num_seg, ts){
  if (length(ts)%%2 == 1){
    ts = ts[-1]
  }
  len = length(ts) # update length of ts
  num_seg = len/2
  
  Seg_TS = matrix(0, nrow = num_seg, ncol = 2)
  # print(Seg_TS)
  # print(dim(Seg_TS)  # 61 2
  # print(class(Seg_TS))
  # create initial fine approximation 
  for (i in 1:num_seg){
    Seg_TS[i,1] = (i-1)*2+1
    Seg_TS[i,2] = i*2
  }
 
 
  Seg_TS = matrix(0, nrow = num_seg, ncol = 2)
  dim(Seg_TS)  # 61 2
  # create initial fine approximation 
  for (i in 1:num_seg){
    Seg_TS[i,1] = (i-1)*2+1
    Seg_TS[i,2] = i*2
  }
  
  # find cost of merging each pair of segments 
  merge_cost = array(0,dim(Seg_TS)[1]-1)
  for(i in 1:(num_seg-1)){
    start = Seg_TS[i,1]
    end = Seg_TS[i+1,2]
    y = ts[start:end]
    len_m = length(y)
    x = seq(1:len_m)
    lmobj = lm(y~x)
    merge_cost[i] = sum((lmobj$residuals)^2)/len_m
  }
  # merge_cost
  # print(num_seg)
  
  # while loop: find minimum merge cost, merge and create new seg
  while (dim(Seg_TS)[1] >= max_num_seg){
    min_Mcost = min(merge_cost)
    index = which(min_Mcost == merge_cost)[[1]]
    # print(index)
    if(length(merge_cost)==1){
      print("hahaha")
      break
    }
    if (index == dim(Seg_TS)[1]) { 
      # This if statement for debugging only;
      # usually cannot have index == dim(Seg_TS)[1]!!!
      print(index)
      print(min_Mcost)
      print(max_error)
      break
    }
    Seg_TS[index,2] = Seg_TS[index+1,2] # merge Seg_TS[index] and Seg_TS[index+1]
    Seg_TS = Seg_TS[-(index+1),] # delete Seg_TS[index+1]
    
    # compute merge cost for Seg_TS(index) and Seg_TS(index+1)
    #   and update the cost to merge_cost[index]
    #   note: make sure that index < num_Seg-1
    if (index < dim(Seg_TS)[1]){
      start = Seg_TS[index,1]
      end = Seg_TS[index+1,2] 
      y = ts[start:end]
      #print(y)
      len_m = length(y)
      x = seq(1:len_m)
      lmobj = lm(y~x)
      merge_cost[index] = sum((lmobj$residuals)^2)/len_m
    }
    
    
    # compute merge cost for seg_TS(index-1) and Seg_TS(index)
    #   and update the cost to merge_cost[index-1]
    #   note: make sure that index > 1, otherwise out of index. 
    if (index > 1){
      start = Seg_TS[index-1,1] 
      #print(start)
      end = Seg_TS[index,2]
      #print(end)
      y = ts[start:end]
      #print(y) # issue here!!!
      len_m = length(y)
      x = seq(1:len_m)
      lmobj = lm(y~x)
      merge_cost[index] = sum((lmobj$residuals)^2)/len_m 
    }
    
    # delete merge_cost[index+1]
    if (index < (length(merge_cost))){
      merge_cost = merge_cost[-(index+1)]
    }
    else{
      merge_cost = merge_cost[-(index-1)]
    }
    # print(dim(Seg_TS)[1])
    
  }
  return(Seg_TS)
}





# area algorithm  -------------------------------------------------------------- --------------------------------------------------------------
# input: max_error, original data object(ts)
# output: Seg_TS
area_algo <- function(max_error, ts){
  if(length(ts)%%2 == 1){
    ts = ts[-1]
  }
  len = length(ts) # update length of ts
  num_seg = len/2
  Seg_TS = matrix(0, nrow = num_seg, ncol = 2)
  # print(Seg_TS)
  # create initial fine approximation 
  for (i in 1:num_seg){
    Seg_TS[i,1] = (i-1)*2+1
    Seg_TS[i,2] = i*2
  }
  #Seg_TS # check results
  
  # find cost of merging each pair of segments 
  merge_cost = array(0,dim(Seg_TS)[1]-1) # merge_cost records our designed loss
  # merge_cost2 = array(0, dim(Seg_TS)[1]-1) # merge cost records the MSE cost
  
  for(i in 1:(num_seg-1)){
    start = Seg_TS[i,1]
    end = Seg_TS[i+1,2]
    y = ts[start:end]
    len_m = length(y)
    x = seq(1:len_m)
    lmobj = lm(y~x)
    
    merge_cost[i] = area(i, Seg_TS, ts)
    # merge_cost2[i] = sum((lmobj$residuals)^2)/len_m
  }
  
  # merge_cost1 = merge_cost
  # merge_cost = scale(merge_cost1)
  # par(mfrow=c(1,1))
  
  
  # merge_cost = merge_cost1/(max(merge_cost2 - min(merge_cost2)))
  # merge_cost
  
  
  # while loop: find minimum merge cost, merge and create new seg
  i = 0
  while (min(merge_cost) < max_error){
    i = i+1
    # if (i == 490) break
    # print(min(merge_cost))#####
    min_Mcost = min(merge_cost)
    index = which(min_Mcost == merge_cost)[[1]]
    # print(dim(Seg_TS)[1]-length(merge_cost))
    Seg_TS[index,2] = Seg_TS[index+1,2] # merge Seg_TS[index] and Seg_TS[index+1]
    Seg_TS = Seg_TS[-(index+1),] # delete Seg_TS[index+1]
    
    if(length(merge_cost)==1){
      # print("hahaha")
      break
    }
    
    # compute merge cost for Seg_TS(index) and Seg_TS(index+1)
    #   and update the cost to merge_cost[index]
    #   note: make sure that index < num_Seg-1
    if (index < dim(Seg_TS)[1]){
      # print(2)
      start = Seg_TS[index,1]
      end = Seg_TS[index+1,2] 
      y = ts[start:end]
      #print(y)
      len_m = length(y)
      x = seq(1:len_m)
      lmobj = lm(y~x)
      # merge_cost[index] = sum((lmobj$residuals)^2)/len_m
      
      merge_cost [index] = area(index, Seg_TS, ts)
    }
    
    
    # compute merge cost for seg_TS(index-1) and Seg_TS(index)
    #   and update the cost to merge_cost[index-1]
    #   note: make sure that index > 1, otherwise out of index. 
    if (index > 1){
      # print(3)
      start = Seg_TS[index-1,1] 
      #print(start)
      end = Seg_TS[index,2]
      #print(end)
      y = ts[start:end]
      #print(y) # issue here!!!
      len_m = length(y)
      x = seq(1:len_m)
      lmobj = lm(y~x)
      # merge_cost[index] = sum((lmobj$residuals)^2)/len_m 
      # print(index - )
      merge_cost[index] = area(index-1, Seg_TS, ts)#####
    }
    
    # delete merge_cost[index+1]
    if (index < (length(merge_cost))){
      merge_cost = merge_cost[-(index+1)]
    } else {
      merge_cost = merge_cost[-(index-1)]
    }
    
    # print(dim(Seg_TS)[1]-length(merge_cost))
    if (length(merge_cost) == dim(Seg_TS)[1]) {
      # This if statement for debugging only;
      # usually cannot have index == dim(Seg_TS)[1]!!!
      print(index)
      print(length(merge_cost))
      # print(min_Mcost)
      print(max_error)
      break
    }
    
  }
  return (Seg_TS)
}




# area algorithm with max_num_seg as criteria  --------------------------------------------------------------
# this algorithm uses max_num_seg as criteria 
# input: max_num_seg, original data object(ts)
# output: Seg_TS
area_algo3 <- function(max_num_seg, tsd){
  if (length(tsd)%%2 == 1){
    tsd = tsd[-1]
  }
  len = length(tsd) # update length of tsd
  num_seg = len/2
  Seg_TSD = matrix(0, nrow = num_seg, ncol = 2)
  # print(Seg_TSD)
  # print(dim(Seg_TSD)  # 61 2
  # print(class(Seg_TSD))
  # create initial fine approximation 
  for (i in 1:num_seg){
    Seg_TSD[i,1] = (i-1)*2+1
    Seg_TSD[i,2] = i*2
  }
  #Seg_TSD # check resultsd
  
  # find cost of merging each pair of segmentsd 
  merge_cost = array(0,dim(Seg_TSD)[1]-1) # merge_cost records our designed loss
  
  
  for(i in 1:(num_seg-1)){
    start = Seg_TSD[i,1]
    end = Seg_TSD[i+1,2]
    y = tsd[start:end]
    len_m = length(y)
    x = seq(1:len_m)
    lmobj = lm(y~x)
    
    merge_cost[i] = area(i, Seg_TSD, tsd)
    # merge_cost2[i] = sum((lmobj$residuals)^2)/len_m
  }
  
  merge_cost1 = merge_cost
  # merge_cost = scale(merge_cost1)
  # merge_cost = merge_cost1/(max(merge_cost2 - min(merge_cost2)))
  
  
  # while loop: find minimum merge cost, merge and create new seg
  i = 0
  while (dim(Seg_TSD)[1] >= max_num_seg){
    i = i+1
    # if (i == 490) break
    # print(min(merge_cost))#####
    min_Mcost = min(merge_cost)
    index = which(min_Mcost == merge_cost)[[1]]
    # print(dim(Seg_TSD)[1]-length(merge_cost))
    Seg_TSD[index,2] = Seg_TSD[index+1,2] # merge Seg_TSD[index] and Seg_TSD[index+1]
    Seg_TSD = Seg_TSD[-(index+1),] # delete Seg_TSD[index+1]
    
    if(length(merge_cost)==1){
      # print("hahaha")
      break
    }
    
    # compute merge cost for Seg_TSD(index) and Seg_TSD(index+1)
    #   and update the cost to merge_cost[index]
    #   note: make sure that index < num_Seg-1
    if (index < dim(Seg_TSD)[1]){
      # print(2)
      start = Seg_TSD[index,1]
      end = Seg_TSD[index+1,2] 
      y = tsd[start:end]
      #print(y)
      len_m = length(y)
      x = seq(1:len_m)
      lmobj = lm(y~x)
      # merge_cost[index] = sum((lmobj$residuals)^2)/len_m
      
      merge_cost [index] = area(index, Seg_TSD, tsd)
    }
    
    
    # compute merge cost for seg_TSD(index-1) and Seg_TSD(index)
    #   and update the cost to merge_cost[index-1]
    #   note: make sure that index > 1, otherwise out of index. 
    if (index > 1){
      # print(3)
      start = Seg_TSD[index-1,1] 
      #print(start)
      end = Seg_TSD[index,2]
      #print(end)
      y = tsd[start:end]
      #print(y) # issue here!!!
      len_m = length(y)
      x = seq(1:len_m)
      lmobj = lm(y~x)
      # merge_cost[index] = sum((lmobj$residuals)^2)/len_m 
      # print(index - )
      merge_cost[index] = area(index-1, Seg_TSD, tsd)#####
    }
    
    # delete merge_cost[index+1]
    if (index < (length(merge_cost))){
      merge_cost = merge_cost[-(index+1)]
    } else {
      merge_cost = merge_cost[-(index-1)]
    }
    
    # print(dim(Seg_TSD)[1]-length(merge_cost))
    if (length(merge_cost) == dim(Seg_TSD)[1]) {
      # This if statement for debugging only;
      # usually cannot have index == dim(Seg_TSD)[1]!!!
      print(index)
      print(length(merge_cost))
      # print(min_Mcost)
      print(max_error)
      break
    }
    
  }
  return (Seg_TSD)
}



#### A function that 
#### input: number of segment(max_seg) and original data object(ts)
#### output: a vector with two entries: given the maximum number of segments
####  first entry is the total MSE error of Bottom-up algo,
####  second entry is the total MSE error of Area algo. 

compare_err <- function(max_seg, ts){
  errs = array(0,2)   # create an empty vector
  # errs[1] = MSE_loss(bottom_up3(max_seg, ts),ts)
  # errs[2] = MSE_loss(area_algo3(max_seg, ts),ts)
  errs[1] = MSE_loss(bottom_up3(max_seg, ts),ts)
  errs[2] = MSE_loss(area_algo3(max_seg, ts),ts)
  return(errs)
}
# compare_err(120,ts) # try out the function



#### new area function   -------------------------------------------------------------------------------------------------------------------
# version 3
area <- function(index, Seg_TS, ts){
  # print(Seg_TS)
  # print(dim(Seg_TS))
  x1 = Seg_TS[index,1]
  # print(is.na(x1))
  x2 = Seg_TS[index,2]
  
  x3 = Seg_TS[index+1,1]
  x4 = Seg_TS[index+1,2]
  
  y1 = ts[x1]
  y2 = ts[x2]
  y3 = ts[x3]
  y4 = ts[x4]
  a = sqrt((x1-x2)^2+(y1-y2)^2)
  b = sqrt((x1-x3)^2+(y1-y3)^2)
  c = sqrt((x2-x3)^2+(y2-y3)^2)
  d = sqrt((x3-x4)^2+(y3-y4)^2)
  e = sqrt((x4-x2)^2+(y4-y2)^2)
  f = sqrt((x1-x4)^2+(y1-y4)^2)
  #S1 = sqrt((a+b+c)*(a*b*c))
  S2 = sqrt((a+e+f)*(a*e*f))
  S3 = sqrt((d+b+f)*(d*b*f))
  #S4 = sqrt((d+c+e)*(d*c*e))

  p=(a+b+c)/2
  #print(p)
  p=p*(p-a)*(p-b)*(p-c)
  if(p<0){
    p=0
  }
  S1 = sqrt(p)
  p=(c+d+e)/2
  p=p*(p-c)*(p-d)*(p-e)
  if(p<0){
    p=0
  }
  S4 = sqrt(p)
  S = (S1+S4)^2*(x4-x1)*(max(y1,y2,y3,y4)-min(y1,y2,y3,y4))
  #  print(a)
  #  print(b)
  #  print(c)
  #  print(d)
  #  print(e)
  #  print(S1)
  #  print(S2)
  # print(dim(Seg_TS))
  return(S)
}



#### functions for creating features ---------------------------------------------------------------------------------------------------------
# var,range,max/min/ave freq,noisiness
fts<-function(tms){
  turn=c()
  freq=c() # frequency
  nsy=abs(tms[2]-tms[1])
  for(i in 3:length(tms)){
    if(tms[i-2]<tms[i-1] & tms[i]<tms[i-1]){
      turn=c(turn, i-1)
    }
    nsy=nsy+abs(tms[i]-tms[i-1])
  }
  for(i in 2:length(turn)){
    freq=c(freq, turn[i]-turn[i-1])
  }
  res=c(var(tms)/mean(tms)^2,(max(tms)-min(tms))/mean(tms),
        max(freq),min(freq),mean(freq),(nsy/length(tms))/mean(ts))
  return(res)
}



#### function for decision_tree learning 
# input: user order k, min_Pts
# output: finished_seg which contains the set of segments 
# 
# process: decompose the userk data into a series of smaller segments
# the argument min_Pts determine how long each segment is around
# notice that the min_pts does not determine the length of the segment
# and the length of each small segment (sometimes we call it seg) are different
# but the length basically is around 60~80 
decision_tree <- function(k, min_pts){
  userk = out[[k]] 
  ts = userk$use[userk$date==unique(userk$date)[1]]
  
  # intialization
  Seg_TS <- data.frame(start = 1, end = length(ts))
  finished_seg <- data.frame(row.names=c("start","end"))
  
  
  h = 0
  while(dim(Seg_TS)[1] > 0){
    h = h+1
    if (h == 100){
      break
    }
    start <- Seg_TS[1,1]
    end <- Seg_TS[1,2]
    seg <- ts[start:end]
    plot.ts(seg)
    n_candids <- ceiling(length(seg)/min_pts)
    if (n_candids == 1){
      print("n_candids = 1, break")
      finished_seg = rbind(finished_seg,Seg_TS[1,]) #plug the seg_ts into the finished list
      Seg_TS = Seg_TS[-1,]
      next
    }
    candids <- sample((start):(end), n_candids)
    diff <- array(NA, n_candids)
    results0 <- compute_scores(seg)
    s0 <- sum(results0[,1]>results0[,2])/dim(results0)[1]
    d0 <- abs(s0-0.5)
    
    for(i in 1:n_candids){
      candid <- candids[i]
      t1 = ts[start:candid]
      t2 = ts[(candid+1):end]
      if (length(t1)<=min_pts || length(t2)<=min_pts){
        print(paste("candidate dispensed:",candid))
        next;
      }
      # compute diff
      results1 = compute_scores(t1)
      results2 = compute_scores(t2)
      s1 <- sum(results1[,1]>results1[,2])/dim(results1)[1]
      s2 <- sum(results2[,1]>results2[,2])/dim(results2)[1]
      d12 <- abs(s1-0.5) + abs(s2-0.5)
      diff[i] = (d12-d0)
      
    }
    # if all candidates are dispensed, use the middle point
    if (sum(!is.na(diff)) ==0 ){
      
      best_candid = ceiling((end+start)/2)
      
      # next
    } else{
      # diff_s <- diff
      diff[is.na(diff)] = min(diff[!is.na(diff)])
      best_candid_idx <- which(max(diff) == diff)[[1]]
      best_candid = candids[best_candid_idx]
    }
    best_candid  
    # print(is.na(best_candid_idx))

    
    # split by the best_candid
    Seg_TS
    (Seg_TS <- rbind(Seg_TS,c(start, best_candid)))
    (Seg_TS <- rbind(Seg_TS,c(best_candid+1, end)))
    (Seg_TS <- Seg_TS[-1,])
    
  }
  return (finished_seg)
}




#### function for computing the score
# input: a data series object
# output: a two-col matrix (first col is error of bottom-up, second col for area algo) 
compute_scores <- function(seg){
  num_seg_lst = ceiling(length(seg)/2^seq(2,7))
  num_seg_lst = num_seg_lst[num_seg_lst>2]
  len = length(num_seg_lst)
  results = matrix(0, nrow = len, ncol=2)
  i = 1
  for(max_seg in num_seg_lst){
    results[i,] = compare_err(max_seg, seg)
    i = i+1
  }
  return(results)
}



#### functions for plotting segments in finsihed_seg ---------------------------------------------
# now we plot some of the segments
plot_finished_Seg <- function(ts, finished_seg,r){
  #### r = 10
  start <- finished_seg[r,1]
  end <- finished_seg[r,2]
  seg <- ts[start:end]
  plot.ts(seg)
  
}





#### seg_decompose --------------------------------------------------------------------------------------
# input: order k means the kth user in d1
# input: min_pts determins how many smaller data ojects you want to get from userk data
# output: a matrix which contains features and scores spawn by the finished_segments 
# 
# process: decompose the data object userk into a series finer data objects
# each data object is in the form of 6 variables and a score variable
# this function prepares a data object for classification task 

seg_decompose <- function(k,min_pts){
  set.seed(123)
  userk = out[[k]] 
  ts = userk$use[userk$date==unique(userk$date)[1]]
  
  finished_seg <- decision_tree(k, min_pts)
  k_X <- matrix(NA, nrow=dim(finished_seg)[1], ncol = 9)
  k_X[,1] = rep(k, dim(finished_seg)[1])
  k_X[,2] = seq(1,dim(finished_seg)[1])
  
  for(r in 1:dim(finished_seg)[1]){
    seg <- ts[finished_seg[r,1]:finished_seg[r,2]]
    k_X[r,-c(1,2,9)] <- fts(seg)
    results <- compute_scores(seg)
    k_X[r,9] <- sum(results[,1]>results[,2])/dim(results)[1]
    
    
    score <- k_X[r,9]
    if (score > 0.5){
      setwd("E:/CSC Courses/CSC4008/Data/decision tree learning/A better")
    } else if (score < 0.5){
      setwd("E:/CSC Courses/CSC4008/Data/decision tree learning/B better")
    } else{
      setwd("E:/CSC Courses/CSC4008/Data/decision tree learning/AB even")
    }
    
    png(file=paste("user ",as.character(k),"_seg ",as.character(r),".png",sep=""))
    plot.ts(seg, main=as.character(r))
    dev.off()
    getwd()
    
  }
  return (k_X)
  
}


#### sliding window 
# input: ts
# input: d
# input: max_err
# output: a set of two-column

sliding_window2 <- function(ts, d, max_err){
  
  seg_result <- c()
  start = 1
  end = d
  plot.ts(ts)
  while(1){
    seg <- ts[start:end]
    
    tmp <- area_algo(max_err,seg)
    if (length(tmp)!=2){
      tmp <- tmp[1,]
    }
    # points(ts[tmp[1]:tmp[2]],col="red",type="l")
    # print(tmp)
    tmp <- tmp + rep(start-1,2)
    seg_result <-rbind(seg_result, tmp)
    start = tmp[2]+1
    end = start + d - 1
    print(start)
    print(end)
    if (end > length(ts)){
      if (start > length(ts)){
        return(seg_result)
      }
      print("?")
      tmp <- area_algo(max_err, ts[start:length(ts)])
      tmp <- tmp + start - 1
      seg_result <- rbind(seg_result,tmp)
      return (seg_result)
    }
  
  }
}



sliding_window <- function(ts, d, max_err){
  
  seg_result <- c()
  start = 1
  end = d
  plot.ts(ts)
  while(1){
    seg <- ts[start:end]
    tmp <- bottom_up(max_err,seg)[1,]
    # points(ts[tmp[1]:tmp[2]],col="red",type="l")
    # print(tmp)
    tmp <- tmp + rep(start-1,2)
    seg_result <-rbind(seg_result, tmp)
    start = tmp[2]+1
    end = start + d - 1
    print(start)
    print(end)
    if (end > length(ts)){
      if (start > length(ts)){
        return(seg_result)
      }
      tmp <- area_algo(max_err, ts[start:length(ts)])
      tmp <- tmp + start - 1
      seg_result <- rbind(seg_result,tmp)
      return (seg_result)
    }
    
  }
  
}

a <- sliding_window(ts,30,0.08)
length(a)
plot_segments(a, ts)
MSE_loss(a,ts)

b <- sliding_window2(ts,50,10)
length(b)
plot_segments(b, ts)
MSE_loss(b,ts)





























