# Pod에 있는 차량들의 대수를 계속 업데이트 

PodInfoUpdater <- function(time){
  if ((time-1)%%30==0) {
    return (paste("Update the pod information at time", time))
  } else {
    idle_tmp <- IdleVehicle[[time]]
    pod_info_tmp <- PodInfo[[time]]
    
    num_veh <- NULL
    for (a in 1:nrow(pod_info_tmp)){
      num_veh_update <- idle_tmp %>% 
        filter(X==pod_info_tmp$x[a],
               Y==pod_info_tmp$y[a])
      num_veh_update<- nrow(num_veh_update)
      
      num_veh<-c(num_veh,num_veh_update)
    }
    
    PodInfo[[time]]$num_veh<<-num_veh
    return (paste("Update the pod information at time", time))
  }
}
