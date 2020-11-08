# input으로 Pooled vehicles이 들어왔을 때 각 차량들의 좌표값을 계산 
# 이 좌표값에 따라 차량들이 움직일 수 있도록
# X_intra_relo; Y_intra_relo

IntraZonalPositionCalculator <- function(PoolVehicle,type="intra_s1"){
  if (type=="intra_s1"){
    num_pod_per_grid<-data.frame(table(PoolVehicle$grid_id))
    colnames(num_pod_per_grid)<-c("grid_id","num_pod")
    num_pod_per_grid$grid_id<-as.numeric(as.character(num_pod_per_grid$grid_id))
    
    PoolVehicle_output<-NULL
    
    for (k in 1:nrow(num_pod_per_grid)){
      id_tmp<-num_pod_per_grid$grid_id[k]
      PoolVehicle_tmp<- PoolVehicle %>% filter(grid_id==id_tmp)
      
      destination<-PodCoord[[nrow(PoolVehicle_tmp)]]
      destination$x <-destination$x+ grid %>% filter(dprt_grid==id_tmp) %>% pull(X_lower_left)
      destination$y <-destination$y+ grid %>% filter(dprt_grid==id_tmp) %>% pull(Y_lower_left)
      
      X_intra_relo<-NULL
      Y_intra_relo<-NULL
      for (m in 1:nrow(PoolVehicle_tmp)){
        vehicle_position_match <- which.min(abs(destination$x-PoolVehicle_tmp[m,]$X)+
                                              abs(destination$y-PoolVehicle_tmp[m,]$Y))
        
        X_intra_tmp <- destination$x[vehicle_position_match]
        Y_intra_tmp <- destination$y[vehicle_position_match]
        
        destination <- destination[-vehicle_position_match,]
        X_intra_relo<-c(X_intra_relo,X_intra_tmp)
        Y_intra_relo<-c(Y_intra_relo,Y_intra_tmp)
      }
      PoolVehicle_tmp$X_intra_relo<-X_intra_relo
      PoolVehicle_tmp$Y_intra_relo<-Y_intra_relo
      
      PoolVehicle_output <- rbind(PoolVehicle_output,PoolVehicle_tmp)} 
    
    return(PoolVehicle_output)} else {
    # Pod id를 만들고 pod의 차대수를 모니터링 해야함
    # Pod사이의 차량수가 균형에 맞도록
    print("Not Yet!!!")
  }
}
