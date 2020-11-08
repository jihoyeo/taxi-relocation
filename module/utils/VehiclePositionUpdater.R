# Vehicle Position Updater 
# 현재위치, 목적지를 알 때 Vehicle의 위치를 time stamp마다 update 시킴

VehiclePositionUpdater <- function (x_start,y_start,x_end,y_end, VehicleSpeed){
  travel_distance <- VehicleSpeed
  x_gap <- x_end - x_start
  y_gap <- y_end - y_start
  
  if (abs(x_gap)+abs(y_gap)>travel_distance){
    
    if (abs(x_gap) > travel_distance) {
      x_update <- x_start + x_gap*(travel_distance/abs(x_gap))
      y_update<-y_start
    } else {
      x_update <- x_end
      travel_distance <- travel_distance - abs(x_gap)
      y_update <- y_start + y_gap*(travel_distance/abs(y_gap))
    }
  } else {
    x_update <- x_end
    y_update <- y_end
  }
  
  position_update <- c(x_update,y_update)
  return(position_update)
}