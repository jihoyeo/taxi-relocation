# Idle & InterRelo & IntraRelo vehicle들로 IntraRelo Vehicle들의 목적지를 업데이트 (InterRelo & Idle >> IntraRelo)

IntraZonalRepositioner<-function(i=1){
  # Idle Vehicle
  idle_vehicle_tmp<-IdleVehicle[[i]]
  
  # InterRelo vehicle
  inter_relo_vehicle_tmp<-InterReloVehicle[[i]]
  if(!is.null(inter_relo_vehicle_tmp)) {
    if (nrow(inter_relo_vehicle_tmp)!=0){
      inter_relo_vehicle_tmp$grid_id <- ZoneIdentifier(inter_relo_vehicle_tmp)
      inter_relo_vehicle_tmp <- inter_relo_vehicle_tmp %>% filter(grid_id==grid_inter_relo) # destined zone에 도착한 차량만 추출
    }
  }

  # IntraRelo vehicle
  intra_relo_vehicle_tmp<-IntraReloVehicle[[i]]
  # if (nrow(intra_relo_vehicle_tmp)==0) intra_relo_vehicle_tmp<-NULL
  
  # Pool (Pooled vehicles >> Intra-relo vehicles)
  PoolVehicle <- idle_vehicle_tmp
  if (!is.null(inter_relo_vehicle_tmp)) PoolVehicle <- rbind(PoolVehicle,inter_relo_vehicle_tmp %>% select(colnames(PoolVehicle)))
  if (!is.null(intra_relo_vehicle_tmp)) PoolVehicle <- rbind(PoolVehicle,intra_relo_vehicle_tmp %>% select(colnames(PoolVehicle)))
  
  # Idle & InterRelo >> IntraRelo
  if(nrow(PoolVehicle)!=0) {
    PoolVehicle_update <- IntraZonalPositionCalculator(PoolVehicle, type="intra_s1")
    PoolVehicle_update$state <- "intra-relocate"} else {
      PoolVehicle_update<-PoolVehicle
      PoolVehicle_update$X_intra_relo<-NULL
      PoolVehicle_update$Y_intra_relo<-NULL
      PoolVehicle_update$state<-NULL
    }
  
  
  # Update IntraReloVehicle
  IntraReloVehicle[[i]] <<- PoolVehicle_update

  # Update IdleVehicle
  IdleVehicle[[i]] <<- idle_vehicle_tmp %>% filter(!VehicleID %in% PoolVehicle_update$VehicleID)
  
  # Update InterReloVehicle
  InterReloVehicle[[i]] <<- InterReloVehicle[[i]] %>% filter(!VehicleID %in% PoolVehicle_update$VehicleID)
  # InterReloVehicle[[i+1]] <<- InterReloVehicle[[i]]
  
  
  # 이전에 t+1으로 넘겨줬던 Idle중에 Relocation으로 빠진 차량들 제거
  IdleVehicle[[i+1]] <<- IdleVehicle[[i+1]] %>% filter(!VehicleID %in% PoolVehicle_update$VehicleID)
}


