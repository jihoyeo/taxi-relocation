# Calculate the number of vehicles at all states (idle;assign;oprt;etc)
# Derive each number of vehicles

TotalNumVeh <- function(time){
  print (paste("Idle is", nrow(IdleVehicle[[time]])))
  print (paste("Assign is", nrow(AssignVehicle[[time]])))
  print (paste("Oprt is", nrow(OprtVehicle[[time]])))
  print (paste("Inter-Relocate is", nrow(InterReloVehicle[[time]])))
  print (paste("Intra-Relocate is", nrow(IntraReloVehicle[[time]])))
  return(nrow(IdleVehicle[[time]])+
           nrow(AssignVehicle[[time]])+
           nrow(OprtVehicle[[time]])+
           nrow(InterReloVehicle[[time]])+
           nrow(IntraReloVehicle[[time]]))
}
