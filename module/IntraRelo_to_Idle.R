# IntraRelo_to_Idle
# Relocating vehicles의 상태를 매시간마다 업데이트
# Relocation 목적지에 도달하면 Idle로 상태를 변경

IntraRelo_to_Idle <- function (IntraReloVehicle, time) {
  # Exit if there is no relocated vehicles
  if (is.null(IntraReloVehicle[[time]])) 
    return(paste("There is no relocated vehicles at time",time))
  
  # Passenger OD-pair at time t
  IntraRelo_tmp<-IntraReloVehicle[[time]]
  
  position_update<-mapply(VehiclePositionUpdater,
                          IntraRelo_tmp$X,IntraRelo_tmp$Y,
                          IntraRelo_tmp$X_intra_relo,
                          IntraRelo_tmp$Y_intra_relo,
                          VehicleSpeed)
  IntraRelo_tmp$X<-position_update[1,]
  IntraRelo_tmp$Y<-position_update[2,]
  
  IntraRelo_tmp$state<-ifelse(IntraRelo_tmp$X==IntraRelo_tmp$X_intra_relo &
                                IntraRelo_tmp$Y == IntraRelo_tmp$Y_intra_relo,
                              "idle",
                              "intra-relocate")
  
  if (nrow(IntraRelo_tmp %>% filter(state=="intra-relocate"))!=0) 
  {IntraReloVehicle[[time+1]]<<- IntraRelo_tmp %>% filter(state=="intra-relocate")}
  
  Idle_update <- IntraRelo_tmp %>% filter(state=="idle")
  IdleVehicle[[time+1]]<<- rbind(IdleVehicle[[time+1]], Idle_update %>% select(colnames(IdleVehicle[[time]])))
}
