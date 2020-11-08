#### Update Vehicle state in assigning(Assign >> Operation)
# Assign 된 차량들의 위치를 timestamp마다 계속 업데이트
# 승객의 위치까지 도달하면 vehicle state를 assign 에서 operation으로 변경

Assign_to_Oprt <- function (AssignVehicle, time) {
  # Passenger OD-pair at time t
  Assign_tmp<-AssignVehicle[[time]]
  
  if (nrow(Assign_tmp)==0) {
    return (paste("There is no assigned vehicles at time", time))
    AssignVehicle[[time+1]]<<- Assign_tmp %>% filter(state=="assign")
    OprtVehicle[[time+1]]<<- Assign_tmp %>% filter(state=="operate")
  }
  
  position_update<-mapply(VehiclePositionUpdater,Assign_tmp$X,Assign_tmp$Y,Assign_tmp$X_start,Assign_tmp$Y_start,VehicleSpeed)
  Assign_tmp$X<-position_update[1,]
  Assign_tmp$Y<-position_update[2,]
  
  Assign_tmp$state<-ifelse(Assign_tmp$X==Assign_tmp$X_start & Assign_tmp$Y == Assign_tmp$Y_start,"operate","assign")
  
  AssignVehicle[[time+1]]<<- Assign_tmp %>% filter(state=="assign")
  OprtVehicle[[time+1]]<<- Assign_tmp %>% filter(state=="operate")
}