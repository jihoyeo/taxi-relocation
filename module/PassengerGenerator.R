# `passen_final_ROI`을 기반으로 randomly distributed 되게 승하차 좌표와 호출시간을 매핑
# 승객의 30분 단위의 O-D matrix 및 Uncertainty (Standard Deviation)에 따라 임의의 승객 O-D pairs를 생성
# Input: O-D matrix by 30 minutes interval
# Output: Randomly Generated O-D pairs over time (24 hours)
# Assumtion: O-D is uniformly generated in spatial-temporal area


PassengerGenerator <- function (passen_final_ROI) {
  OD_Pairs_tmp<-passen_final_ROI %>% 
    select(dprt_grid,arrvl_grid,dprt_time,dprt_time_30min) %>%
    arrange(dprt_time) %>% 
    left_join(grid, by="dprt_grid") %>% 
    select(X_start=X_centroid,Y_start=Y_centroid, dprt_grid, arrvl_grid, dprt_time, dprt_time_30min) %>%
    left_join(grid %>% select(dprt_grid,X_end=X_centroid,Y_end=Y_centroid), by=c("arrvl_grid"="dprt_grid")) %>%
    select(dprt_time,dprt_time_30min,X_start,Y_start,X_end,Y_end,dprt_grid,arrvl_grid)
  
  OD_Pairs_tmp$X_start<-OD_Pairs_tmp$X_start + runif(nrow(OD_Pairs_tmp), min=-500, max=500)
  OD_Pairs_tmp$Y_start<-OD_Pairs_tmp$Y_start + runif(nrow(OD_Pairs_tmp), min=-500, max=500)
  OD_Pairs_tmp$X_end<-OD_Pairs_tmp$X_end + runif(nrow(OD_Pairs_tmp), min=-500, max=500)
  OD_Pairs_tmp$Y_end<-OD_Pairs_tmp$Y_end + runif(nrow(OD_Pairs_tmp), min=-500, max=500)
  OD_Pairs_tmp$PassengerID <- 1:nrow(OD_Pairs_tmp)
  
  # 시간 변수를 numeric으로 변경
  OD_Pairs_tmp$time <- as.numeric(OD_Pairs_tmp$dprt_time)-1488412800
  OD_Pairs_tmp$time<-trunc(OD_Pairs_tmp$time/60)+1
  OD_Pairs_tmp$VehicleID<-NA
  OD_Pairs_tmp$wait_time <- 0
  
  return(OD_Pairs_tmp)
}