# Convert GA Table to InterReloTable (there is no negative # of relo_vehicles in InterReloTable)

ConvertTable<-function(x=InterReloTableGA[[1]]){
  inter_relo_table<-x
  inter_relo_table_p <- inter_relo_table %>% filter(num_relo>0)
  inter_relo_table_n <- inter_relo_table %>% filter(num_relo<0)
  inter_relo_table_n <- inter_relo_table_n %>% select(grid_start=grid_end,
                                                      grid_end=grid_start,
                                                      x_start=x_end,
                                                      y_start=y_end,
                                                      x_end=x_start,
                                                      y_end=y_start,
                                                      travel_time,
                                                      num_relo) %>%
    mutate(num_relo=-(num_relo))
  
  inter_relo_table <-rbind(inter_relo_table_p,inter_relo_table_n) %>% arrange(grid_start)
  return(inter_relo_table)
}