# InterRelo_to_Idle
# Relocating vehicles의 상태를 매시간마다 업데이트
# Relocation 목적지에 도달하면 Idle로 상태를 변경

InterRelo_to_Idle <- function (InterReloVehicle, time) {
  tmp <- InterReloVehicle[[time]]
  if (is.null(tmp)) {
    tmp <- InterReloVehicle[[1]][0,]
    InterReloVehicle[[time]] <<- InterReloVehicle[[1]][0,]}
  # Exit if there is no relocated vehicles
  if (nrow(tmp)==0) {
    InterReloVehicle[[time+1]] <<- InterReloVehicle[[time]][0,]
  } else {
    # Passenger OD-pair at time t
    InterRelo_tmp<-InterReloVehicle[[time]]
    
    position_update<-mapply(VehiclePositionUpdater,
                            InterRelo_tmp$X,InterRelo_tmp$Y,
                            InterRelo_tmp$X_inter_relo,
                            InterRelo_tmp$Y_inter_relo,
                            VehicleSpeed)
    InterRelo_tmp$X<-position_update[1,]
    InterRelo_tmp$Y<-position_update[2,]
    
    InterRelo_tmp$state<-ifelse(InterRelo_tmp$X==InterRelo_tmp$X_inter_relo &
                                  InterRelo_tmp$Y == InterRelo_tmp$Y_inter_relo,
                                "idle",
                                "inter-relocate")
    
    if (nrow(InterRelo_tmp %>% filter(state=="inter-relocate"))!=0) 
    {InterReloVehicle[[time+1]]<<- InterRelo_tmp %>% filter(state=="inter-relocate")}
    
    Idle_update <- InterRelo_tmp %>% filter(state=="idle")
    Idle_update$grid_id<-Idle_update$grid_inter_relo
    IdleVehicle[[time+1]]<<- rbind(IdleVehicle[[time+1]], Idle_update %>% select(colnames(IdleVehicle[[time]])))
  }
}
