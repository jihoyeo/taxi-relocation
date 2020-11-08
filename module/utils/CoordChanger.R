# 위경도/XY직각 좌표계 변환 함수

CoordChanger <- function(dat,
                         adjust_x=min(grid_xy$dprt_grid_x),
                         adjust_y=min(grid_xy$dprt_grid_y),
                         type="to_WGS"){
  if (type=="to_WGS") {
    x_new <- dat[,1] + adjust_x
    y_new <- dat[,2] + adjust_y
    tmp <- SpatialPoints(data.frame(x_new,y_new), CRS("+init=epsg:5181"))
    tmp2 <- spTransform(tmp, CRS("+init=epsg:4326"))
    tmp3 <- data.frame(tmp2)
  } else
  {
    tmp <- SpatialPoints(data.frame(x=dat[,1],y=dat[,2]), CRS("+init=epsg:4326"))
    tmp2 <- spTransform(tmp, CRS("+init=epsg:5181"))
    tmp3 <- data.frame(tmp2)
    tmp3[,1] <- tmp3[,1] - adjust_x + 500
    tmp3[,2] <- tmp3[,2] - adjust_y + 500
  }
  
  return(tmp3)
}