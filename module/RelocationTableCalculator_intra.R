# Derive inter-relocation table (i.e. the number of relocated vehicles between zones)
# Three type of relocation
# 1. only inter-zonal relocation: inter-relo
# 2. Inter- and Intra-zonal relocation with Strategy 1 - intra_s1
# 3. 2. Inter- and Intra-zonal relocation with Strategy 2 - intra_s2
# The cost functions of three scenario are different 

RelocationTableCalculator_intra <- function (i=1,pmu=0.7,pcro=0.2,max_iter=100, type="inter-relo"){
  # Derive the number of idle vehicles at time t and t+1
  tmp<-OD_Pairs_tmp %>% group_by(dprt_grid,dprt_time_30min) %>% summarise(n=n())
  tmp2<-data.frame(grid_id=rep(sort(ROI),each=48),time=Time)
  tmp2 <-tmp2 %>% left_join(tmp, by=c("grid_id"="dprt_grid","time"="dprt_time_30min")) %>% arrange(time)
  tmp2[is.na(tmp2$n),]$n<-0
  
  A_tmp <- IdleVehicle[[(i-1)*30+1]]
  if (!is.null(IntraReloVehicle[[(i-1)*30+1]])){
    Intra_tmp <- IntraReloVehicle[[(i-1)*30+1]] %>% select(colnames(IdleVehicle[[1]]))
    A_tmp <- rbind(A_tmp,Intra_tmp)
  }
  
  
  if (!is.null(IntraReloVehicle[[(i-1)*30+1]])){
    if (nrow(IntraReloVehicle[[(i-1)*30+1]]!=0)) {
      A_tmp <- rbind(IdleVehicle[[(i-1)*30+1]],IntraReloVehicle[[(i-1)*30+1]] %>% select(colnames(IdleVehicle[[(i-1)*30+1]])))}
  }
  
  A_tmp <- A_tmp%>% group_by(grid_id) %>% arrange(grid_id) %>% summarise(n=n())
  
  A_tmp2<-data.frame(grid_id=rep(sort(ROI)))
  A_tmp2 <-A_tmp2 %>% left_join(A_tmp, by=c("grid_id"))
  if (sum(is.na(A_tmp2$n))!=0) A_tmp2[is.na(A_tmp2$n),]$n<-0
  
  A[[i]]<<-A_tmp2$n
  A_delta[[i]] <<- A[[i]] + Psi[[i]] - Phi[[i]] 
  
  f <- function(z){
    # Relative # of vehicles at each zone
    RelocationTable$num_relo<-z
    inflow<-RelocationTable %>% group_by(grid_end) %>% summarise(inflow=sum(num_relo))
    outflow<-RelocationTable %>% group_by(grid_start) %>% summarise(outflow=sum(num_relo))
    
    in_out_flow<-grid %>% select(grid_id=dprt_grid) %>% arrange(grid_id)
    colnames(inflow)[1]<-"grid_id"
    colnames(outflow)[1]<-"grid_id"
    in_out_flow <- in_out_flow %>% left_join(inflow,by="grid_id")
    in_out_flow <- in_out_flow %>% left_join(outflow,by="grid_id")
    in_out_flow[is.na(in_out_flow)]<-0
    
    x<- in_out_flow$inflow-in_out_flow$outflow
    
    InterReloCost <- sum(RelocationTable$travel_time*abs(z))*C_s/60
    
    if (type=="inter-relo"){
      IntraReloCost <-0
      WaitTime <- (2^0.5)/(3*VehicleSpeed_km_h)/((A_delta[[i]]+x)^0.5 + A[[i]]^0.5)
    } else if (type=="intra_s1"){
      IntraReloCost <- sum((A[[i]]+A_delta[[i]]+x)/2*C_r*0.5)
      WaitTime <- (2^0.5)/(3*VehicleSpeed_km_h)/((A_delta[[i]]+x)^0.5 + A[[i]]^0.5)
    } else {
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
      WaitTime <- (2^0.5)/(3*15)/(pod^0.5)
      
    }
    
    WaitTime[is.nan(WaitTime)]<-999
    WaitTime[is.infinite(WaitTime)]<-999
    
    # 7.05$/hour >> 3.5$/30min 
    # or 18626won/hr >> 15.73$/hr >> 7.89$/30min
    PassengerCost <- sum(WaitTime*Phi[[i]]) * C_p # unit is '$'
    
    # 6km/liter >> 0.73$/liter >> 8.22km/$ >> 0.12$/km
    # speed: 30km/h >> 15km/30min >> 3.6$/hr
    AssignedCost <- sum(WaitTime*Phi[[i]])*C_s*Rho
    
    TotalCost<-PassengerCost + AssignedCost + InterReloCost + IntraReloCost
    return(TotalCost)
  }
  
  # Constraint
  
  # Minimum number constraint in zones
  cons <- function(z) {
    RelocationTable$num_relo<-z
    inflow<-RelocationTable %>% group_by(grid_end) %>% summarise(inflow=sum(num_relo))
    outflow<-RelocationTable %>% group_by(grid_start) %>% summarise(outflow=sum(num_relo))
    
    in_out_flow<-grid %>% select(grid_id=dprt_grid) %>% arrange(grid_id)
    colnames(inflow)[1]<-"grid_id"
    colnames(outflow)[1]<-"grid_id"
    in_out_flow <- in_out_flow %>% left_join(inflow,by="grid_id")
    in_out_flow <- in_out_flow %>% left_join(outflow,by="grid_id")
    in_out_flow[is.na(in_out_flow)]<-0
    
    x<- in_out_flow$inflow-in_out_flow$outflow
    
    con<-0
    zone_cons <- A_delta[[i]] - uncertain*(SD_Phi[[i]] + SD_Psi[[i]]) + x
    
    for (k in 1:42){
      con <- con + min(0,zone_cons[k])*1000000
    }
    return(con)
  }
  
  
  # Final fitness function
  fitness_final<-function(z){
    z2<-floor(z)
    -f(z2)+cons(z2)
  }
  
  # Set and Run GA to find optimal x
  
  print(paste("Start calculating inter-zonal relocation table at time", (i-1)*30+1)) 
  
  ga2 <- ga(
    type = "real-valued",
    fitness = fitness_final,
    lower = rep(-20,nrow(RelocationTable)),
    upper = rep(20,nrow(RelocationTable)),
    popSize = 100, maxiter = max_iter, run = max_iter,
    pmutation = pmu,
    pcrossover = pcro
  )
  
  tmp<-summary(ga2)
  z_solution<-floor(tmp$solution[1,])
  RelocationTable$num_relo<-z_solution
  
  inflow<-RelocationTable %>% group_by(grid_end) %>% summarise(inflow=sum(num_relo))
  outflow<-RelocationTable %>% group_by(grid_start) %>% summarise(outflow=sum(num_relo))
  
  in_out_flow<-grid %>% select(grid_id=dprt_grid) %>% arrange(grid_id)
  colnames(inflow)[1]<-"grid_id"
  colnames(outflow)[1]<-"grid_id"
  in_out_flow <- in_out_flow %>% left_join(inflow,by="grid_id")
  in_out_flow <- in_out_flow %>% left_join(outflow,by="grid_id")
  in_out_flow[is.na(in_out_flow)]<-0
  
  x<- in_out_flow$inflow-in_out_flow$outflow
  
  # Derive the number of pods in Strategy 2
  pod <- trunc((((A[[i]]+Psi[[i]]+x- Phi[[i]])/1.96)^2/Phi[[i]]+1))
  
  # 수요가 0인 zone은 pod의 갯수 1개로 고정
  if (sum(is.infinite(pod)!=0)) pod[is.infinite(pod)]<-1
  if (sum(is.nan(pod)!=0)) pod[is.nan(pod)]<-1
  
  if(sum(pod>A[[i]])!=0) pod[pod>A[[i]]] <- A[[i]][pod>A[[i]]]
  if(sum(pod==0)!=0) pod[pod==0] <- 1
  
  P_n[[i]]<<-pod
  
  return(RelocationTable)
}
