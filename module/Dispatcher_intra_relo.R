# Vehicle Dispacher
# 빈차를 승객과 매칭시킴
# 승객이 호출하는 순서대로 가장 가까이 위치한 차량을 매칭시킴
# Idle & InterRelo & IntraRelo >> Assign 상태로 변하는 차량들을 컨트롤
# IntraRelocation을 해줄 때만 사용 

Dispatcher_intra_relo <- function (OD_Pairs, IdleVehicle, time) {
  
  # Passenger OD-pair at time t
  OD_tmp<-OD_Pairs[[time]]
  if (nrow(OD_tmp)==0) {
    IdleVehicle[[time+1]] <<- IdleVehicle[[time]]
  } else {
    # Idle vehicle at time t
    if (time==1) Idle_tmp<-InitialVehicle else Idle_tmp<- IdleVehicle[[time]]
    if (time==1) InterRelo_tmp<-NULL else InterRelo_tmp<- InterReloVehicle[[time]]
    if (time==1) IntraRelo_tmp<-NULL else IntraRelo_tmp<- IntraReloVehicle[[time]]
    
    Dispatch_Veh <- Idle_tmp
    if (!is.null(InterRelo_tmp)) Dispatch_Veh <- rbind(Dispatch_Veh, InterRelo_tmp %>% select(colnames(Idle_tmp)))
    if (!is.null(IntraRelo_tmp)) Dispatch_Veh <- rbind(Dispatch_Veh, IntraRelo_tmp %>% select(colnames(Idle_tmp)))
    
    # Match passenger & vehicle (Shortest path)
    # State change (idle >> assign)
    assign_vehicle_id<-NULL
    for (i in 1:min(nrow(OD_tmp),nrow(Dispatch_Veh))){
      Dispatch_Veh_tmp <- Dispatch_Veh %>% filter(state!="assign")
      
      # Greed search
      passenger_vehicle_match <- which.min(abs(Dispatch_Veh_tmp$X-OD_tmp[i,]$X_start)+
                                             abs(Dispatch_Veh_tmp$Y-OD_tmp[i,]$Y_start))
      assign_vehicle_id_tmp <- Dispatch_Veh_tmp$VehicleID[passenger_vehicle_match]
      OD_tmp$VehicleID[i]<-assign_vehicle_id_tmp
      Dispatch_Veh[Dispatch_Veh$VehicleID==assign_vehicle_id_tmp,]$state<-"assign"
      assign_vehicle_id <- c(assign_vehicle_id,assign_vehicle_id_tmp)
    }
    
    # Update Idle vehicle
    if (sum(Idle_tmp$VehicleID %in% assign_vehicle_id)!=0){
      Idle_tmp[Idle_tmp$VehicleID %in% assign_vehicle_id,]$state<-"assign"}
    
    # Update InterRelocate vehicle
    if (sum(InterRelo_tmp$VehicleID %in% assign_vehicle_id)!=0) {
      InterRelo_tmp[InterRelo_tmp$VehicleID %in% assign_vehicle_id,]$state<-"assign"}
    
    # Update IntraRelocate vehicle
    if (sum(IntraRelo_tmp$VehicleID %in% assign_vehicle_id)!=0) {
      IntraRelo_tmp[IntraRelo_tmp$VehicleID %in% assign_vehicle_id,]$state<-"assign"}
    
    # Update Idle & Assigned vehicle
    Assign_tmp <- Dispatch_Veh %>% filter(state=="assign")
    Idle_tmp <- Idle_tmp %>% filter(state=="idle")
    
    # Update Inter-relo Table
    if (!is.null(InterRelo_tmp)){
      if (nrow(InterRelo_tmp %>% filter(state=="assign"))!=0){
        tmp <- InterRelo_tmp %>% filter(state=="assign") %>% 
          group_by(grid_start=grid_id,grid_end=grid_inter_relo) %>% 
          summarise(num_relo=n()) %>%
          arrange(grid_start,grid_end)
        tmp <- tmp %>% left_join(Tau, by=c("grid_start","grid_end"))
        InterReloTable[[time]] <<- rbind(InterReloTable[[time]],tmp)
      }
    }
    
    
    # Update Inter-relo vehicle
    if(!is.null(InterRelo_tmp)) InterReloVehicle[[time]] <<- InterRelo_tmp %>% filter(state=="inter-relocate")
    
    # Update Intra-relo vehicle
    if(!is.null(IntraRelo_tmp)) IntraReloVehicle[[time]] <<- IntraRelo_tmp %>% filter(state=="intra-relocate")
    
    # Join the information of passenger trip (start & end location of trip & grid_id)
    Assign_tmp <- Assign_tmp %>% 
      left_join(OD_tmp %>% select(VehicleID,X_start,Y_start,X_end,Y_end,dprt_grid,arrvl_grid), by="VehicleID")
    
    Idle_tmp <- Idle_tmp %>% filter(state=="idle")
    
    # Update passenger demands
    OD_Pairs[[time]]<<-OD_tmp[!is.na(OD_tmp$VehicleID),]
    
    # Update unassigned passenger to next timestamp
    unassigned_demand <- OD_tmp[is.na(OD_tmp$VehicleID),]
    unassigned_demand$wait_time <- unassigned_demand$wait_time+1
    OD_Pairs[[time+1]]<<-rbind(unassigned_demand, OD_Pairs[[time+1]])
    
    # Update Idle & Assign vehicle
    IdleVehicle[[time]]<<- Idle_tmp
    if (time!=1440) IdleVehicle[[time+1]]<<- Idle_tmp else print ("Simulation Finish!")
    if (time==1) AssignVehicle[[time]] <<- Assign_tmp else 
      AssignVehicle[[time]]<<- rbind(AssignVehicle[[time]],Assign_tmp)
  }
}
