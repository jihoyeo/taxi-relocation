# ZoneIdentifier (Detect zone id from X,y Coordinate)

ZoneIdentifier <- function(dat){
  grid_id <- NULL
  for (i in 1:nrow(dat)){
    c1<-dat$X[i]-grid$X_lower_left>=0
    c2<-dat$Y[i]-grid$Y_lower_left>=0
    c3<-dat$X[i]-grid$X_upper_right<0
    c4<-dat$Y[i]-grid$Y_upper_right<0
    grid_id <- c(grid_id,grid$dprt_grid[c1&c2&c3&c4])
  }
  return(grid_id)
}
