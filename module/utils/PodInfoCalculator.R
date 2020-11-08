# Derive pod information by zones


PodInfoCalculator <- function (P_n,PodCoord,i){
  
  num_pod_per_grid<-data.frame(grid_id=grid$dprt_grid,num_pod=P_n[[trunc((i-1)/30+1)]])
  
  P_n[[i]]
  pod_info_tmp<-PodCoord[[i]]
  destination$x <-destination$x+ grid %>% filter(dprt_grid==id_tmp) %>% pull(X_lower_left)
  destination$y <-destination$y+ grid %>% filter(dprt_grid==id_tmp) %>% pull(Y_lower_left)
  
  
  PodInfo[[i]] <<- 
}





