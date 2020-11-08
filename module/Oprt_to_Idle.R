# Update Vehicle state in operation (Operation >> Idle)
# In-operation 중인 차량들의 위치를 time stamp마다 계속 업데이트
# 승객의 목적지까지 도달하면 vehicle state를 operation에서 idle로 변경

Oprt_to_Idle <- function (OprtVehicle, time) {
  if (time==1) print("There is no vehicles in operation") else {
    Oprt_tmp<-OprtVehicle[[time]]
    if (nrow(Oprt_tmp)!=0){
      position_update<-mapply(VehiclePositionUpdater,Oprt_tmp$X,Oprt_tmp$Y,Oprt_tmp$X_end,Oprt_tmp$Y_end,VehicleSpeed)
      Oprt_tmp$X<-position_update[1,]
      Oprt_tmp$Y<-position_update[2,]
      
      Oprt_tmp$state<-ifelse(Oprt_tmp$X==Oprt_tmp$X_end & Oprt_tmp$Y == Oprt_tmp$Y_end,"idle","operate")
    }
    
    OprtVehicle[[time+1]]<<- rbind(OprtVehicle[[time+1]], Oprt_tmp %>% filter(state=="operate"))
    IdleVehicle[[time+1]]<<- rbind(IdleVehicle[[time+1]], Oprt_tmp %>% filter(state=="idle") %>% select(colnames(IdleVehicle[[time]])))
    
    if (nrow(IdleVehicle[[time+1]])!=0) IdleVehicle[[time+1]]$grid_id<<-ZoneIdentifier(IdleVehicle[[time+1]])
  }
}