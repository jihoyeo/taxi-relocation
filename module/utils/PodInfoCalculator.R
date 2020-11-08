# Derive pod information by zones


PodInfoCalculator <- function (P_n,PodCoord,time){
  
  num_pod_per_grid<-data.frame(grid_id=grid$dprt_grid,num_pod=P_n[[(time-1)/30+1]]) %>% arrange(grid_id)
  
  PodInfo_tmp <- data.frame(grid_id=rep(num_pod_per_grid$grid_id,num_pod_per_grid$num_pod))
  
  pod_xy<-do.call(rbind,lapply(num_pod_per_grid$num_pod, function(k) PodCoord[[k]]))
  
  PodInfo_tmp <- cbind(PodInfo_tmp,pod_xy)
  
  PodInfo_tmp$x <- PodInfo_tmp$x + sapply(PodInfo_tmp$grid_id, 
                                          function(k) grid %>% filter(dprt_grid==k) %>% pull(X_lower_left))
  
  PodInfo_tmp$y <- PodInfo_tmp$y + sapply(PodInfo_tmp$grid_id, 
                                          function(k) grid %>% filter(dprt_grid==k) %>% pull(Y_lower_left))
  
  PodInfo_tmp$num_veh <-0
  PodInfo_tmp$pod_id<-1:nrow(PodInfo_tmp)
  
  PodInfo[[time]] <<- PodInfo_tmp
  for (i in (time+1):(time+29)) PodInfo[[i]] <<- PodInfo[[time]]
  
}





