# Relocation table을 기준으로 Relocated vehicle group을 업데이트 (Idle & Intra  >> Relocate)
# Intra-zonal 과 같이 하였을 때의 코드 

InterZonalRepositioner_intra<-function(i=1){
  # Relocation Table
  if (is.null(InterReloTable[[i]])) return(paste("All inter-zonal relocation is done at time",i))
  inter_relo_table <- InterReloTable[[i]]
  
  # Idle Vehicle
  idle_vehicle_tmp<-IdleVehicle[[i]]
  if (!is.null(IntraReloVehicle[[i]])) idle_vehicle_tmp <- rbind(idle_vehicle_tmp,IntraReloVehicle[[i]] %>% select(colnames(idle_vehicle_tmp)))
  
  idle_vehicle_tmp$grid_id <- ZoneIdentifier(idle_vehicle_tmp)
  idle_vehicle_tmp <- idle_vehicle_tmp %>% arrange(grid_id)
  
  inter_relo_vehicle_tmp <- NULL
  
  # Idle & intra >> InterRelocation
  for (k in 1:nrow(inter_relo_table)){
    grid_start <-inter_relo_table$grid_start[k]
    grid_end<-inter_relo_table$grid_end[k]
    num_relo<-inter_relo_table$num_relo[k]
    Relo_vehicle <- idle_vehicle_tmp %>% filter(grid_id==grid_start) %>% sample_n(min(length(.$VehicleID),num_relo))
    if (nrow(Relo_vehicle)==0) next
    Relo_vehicle$state <- "inter-relocate"
    
    # update Inter-relocation Table
    inter_relo_table$num_relo[k]<-inter_relo_table$num_relo[k]-nrow(Relo_vehicle)
    
    # Update Inter-relocation Vehicles
    Relo_vehicle$grid_inter_relo <- grid_end
    Relo_vehicle$X_inter_relo <- inter_relo_table$x_end[k] + runif(nrow(Relo_vehicle),min=-500, max=500)
    Relo_vehicle$Y_inter_relo <- inter_relo_table$y_end[k] + runif(nrow(Relo_vehicle),min=-500, max=500)
    
    inter_relo_vehicle_tmp <- rbind(inter_relo_vehicle_tmp,Relo_vehicle)
    idle_vehicle_tmp <- idle_vehicle_tmp %>% filter(!VehicleID %in% Relo_vehicle$VehicleID)
  }
  
  # Update Relocation Table
  inter_relo_table <- inter_relo_table %>% filter(num_relo>0)
  if (nrow(inter_relo_table)!=0) InterReloTable[[i+1]] <<- inter_relo_table
  
  # Update InterReloVehicle
  if (!is.null(inter_relo_vehicle_tmp)) {
    if(nrow(inter_relo_vehicle_tmp)!=0) 
      InterReloVehicle[[i]] <<- rbind(InterReloVehicle[[i]],inter_relo_vehicle_tmp)
  }
  
  # Update IdleVehicle
  if (nrow(idle_vehicle_tmp)==0) {IdleVehicle[[i]] <<- idle_vehicle_tmp} else
    IdleVehicle[[i]] <<- idle_vehicle_tmp %>% filter(state=="idle")
  
  # Update ItraReloVehicle
  if(!is.null(IntraReloVehicle[[i]])) IntraReloVehicle[[i]] <<- IntraReloVehicle[[i]] %>% filter(!VehicleID %in% InterReloVehicle[[i]]$VehicleID)
  
  # 이전에 t+1으로 넘겨줬던 Idle중에 Relocation으로 빠진 차량들 제거
  IdleVehicle[[i+1]] <<- IdleVehicle[[i+1]] %>% filter(!VehicleID %in% InterReloVehicle[[i]]$VehicleID)
}
