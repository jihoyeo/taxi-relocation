ModelCostCalculator <- function(decision,i=1, type="intra_s1"){
  # Relative # of vehicles at each zone
  if (length(decision)!=0){
    RelocationTable$num_relo<-decision
    inflow<-RelocationTable %>% group_by(grid_end) %>% summarise(inflow=sum(num_relo))
    outflow<-RelocationTable %>% group_by(grid_start) %>% summarise(outflow=sum(num_relo))
    
    
    
    in_out_flow<-grid %>% select(grid_id=dprt_grid) %>% arrange(grid_id)
    colnames(inflow)[1]<-"grid_id"
    colnames(outflow)[1]<-"grid_id"
    in_out_flow <- in_out_flow %>% left_join(inflow,by="grid_id")
    in_out_flow <- in_out_flow %>% left_join(outflow,by="grid_id")
    in_out_flow[is.na(in_out_flow)]<-0
    
    x<- in_out_flow$inflow-in_out_flow$outflow
  }

  # 7.05$/hour >> 3.5$/30min 
  # or 18626won/hr >> 15.73$/hr >> 7.89$/30min
  WaitTime <- (2^0.5)/(3*VehicleSpeed_km_h)/((A_delta[[i]]+x)^0.5 + A[[i]]^0.5)
  WaitTime[is.nan(WaitTime)]<-10
  WaitTime[is.infinite(WaitTime)]<-10
  PassengerCost <- sum(WaitTime*Phi[[i]]) * C_p # unit is '$'
  
  # 6km/liter >> 0.73$/liter >> 8.22km/$ >> 0.12$/km
  # speed: 15km/h >> 1.8$/hr
  AssignedCost <- sum(WaitTime*Phi[[i]])*C_s*Rho
  
  InterReloCost <- sum(RelocationTable$travel_time*abs(z))*C_s/60
  
  if (type=="inter-relo"){
    IntraReloCost <-0
    WaitTime <- (2^0.5)/(3*VehicleSpeed_km_h)/((A_delta[[i]]+x)^0.5 + A[[i]]^0.5)
    } else if (type=="intra_s1"){
    IntraReloCost <- sum((A[[i]]+A_delta[[i]]+x)/2*C_r*0.5)
    WaitTime <- (2^0.5)/(3*VehicleSpeed_km_h)/((A_delta[[i]]+x)^0.5 + A[[i]]^0.5)
    } else if (type=="intra_s2"){
      IntraReloCost <- sum((in_out_flow$inflow+Psi[[i]])*C_s*0.63/VehicleSpeed_km_h)
      
      # Derive the number of pods in Strategy 2
      pod <- trunc((((A[[i]]+Psi[[i]]+x- Phi[[i]])/1.96)^2/Phi[[i]]+1))
      
      # 수요가 0인 zone은 pod의 갯수 1개로 고정
      if (sum(is.infinite(pod)!=0)) {pod[is.infinite(pod)]<-1}
      if (sum(is.nan(pod)!=0)) {pod[is.nan(pod)]<-1}
      
      # pod수는 현재의 idle vehicle number보다 클 수 없음
      if(sum(pod>A[[i]])!=0) pod[pod>A[[i]]] <- A[[i]][pod>A[[i]]]
      if(sum(pod==0)!=0) pod[pod==0] <- 1
      
      # Derive the wait time of passengers
      WaitTime <- (2^0.5)/(3*VehicleSpeed_km_h)/(pod^0.5)
      }
  
  
  TotalCost<-PassengerCost + AssignedCost + InterReloCost + IntraReloCost
  return(data.frame(PassengerCost,AssignedCost,InterReloCost,IntraReloCost,TotalCost))
}
