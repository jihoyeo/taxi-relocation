# Make the visualization data for plotly animation
# Three type c("no-relo", "inter-relo", "intra-relo")
# "inter-intra-relo" includes both stragegy 1 & 2

MakeVisDat<- function(type="inter-relo"){
  
  agg_IdleVehicle <- do.call(rbind,sapply(1:1440, function (k) IdleVehicle[[k]], simplify = FALSE))
  agg_IdleVehicle <-agg_IdleVehicle %>% select(VehicleID,X,Y,state,time)
  
  agg_AssignVehicle <- do.call(rbind,sapply(1:1440, function (k) AssignVehicle[[k]], simplify = FALSE))
  agg_AssignVehicle<-agg_AssignVehicle %>% select(VehicleID,X,Y,state,time)
  
  agg_OprtVehicle <- do.call(rbind,sapply(1:1440, function (k) OprtVehicle[[k]], simplify = FALSE))
  agg_OprtVehicle<-agg_OprtVehicle %>% select(VehicleID,X,Y,state,time)
  
  
  if (type=="no-relo") {
    # Combine idle, assign, oprt vehicles into one dataframe
    agg_vis_dat <- rbind(agg_IdleVehicle, agg_AssignVehicle, agg_OprtVehicle)
    
    # plotly 시각화를 위해 빈차한대를 임의로 추가
    agg_vis_dat <- rbind(agg_vis_dat, data.frame(VehicleID=999,X=0,Y=0,state="operate",time=1))
    
    return(agg_vis_dat)
    
  }
  
  if (type=="inter-relo") {
    agg_InterReloVehicle <- do.call(rbind,sapply(1:1440, function (k) InterReloVehicle[[k]], simplify = FALSE))
    agg_InterReloVehicle<-agg_InterReloVehicle %>% select(VehicleID,X,Y,state,time)
    
    # Combine idle, assign, oprt vehicles into one dataframe
    agg_vis_dat <- rbind(agg_IdleVehicle, agg_AssignVehicle, agg_OprtVehicle,agg_InterReloVehicle)
    
    # plotly 시각화를 위해 빈차한대를 임의로 추가
    agg_vis_dat <- rbind(agg_vis_dat, data.frame(VehicleID=999,X=0,Y=0,state="operate",time=1))
    
    return(agg_vis_dat)
  }
  
  if (type=="inter-intra-relo") { 
    agg_InterReloVehicle <- do.call(rbind,sapply(1:1440, function (k) InterReloVehicle[[k]], simplify = FALSE))
    agg_InterReloVehicle<-agg_InterReloVehicle %>% select(VehicleID,X,Y,state,time)
    
    agg_IntraReloVehicle <- do.call(rbind,sapply(1:1440, function (k) IntraReloVehicle[[k]], simplify = FALSE))
    agg_IntraReloVehicle<-agg_IntraReloVehicle %>% select(VehicleID,X,Y,state,time)
    
    # Combine idle, assign, oprt vehicles into one dataframe
    agg_vis_dat <- rbind(agg_IdleVehicle, agg_AssignVehicle, agg_OprtVehicle,agg_InterReloVehicle,agg_IntraReloVehicle)
    
    # plotly 시각화를 위해 빈차한대를 임의로 추가
    agg_vis_dat <- rbind(agg_vis_dat, data.frame(VehicleID=999,X=0,Y=0,state="operate",time=1))
    agg_vis_dat <- rbind(agg_vis_dat, data.frame(VehicleID=1000,X=0,Y=0,state="idle",time=1))
    
    return(agg_vis_dat)
  }
}