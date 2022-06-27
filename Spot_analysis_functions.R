##########
##
## 22JUN2022 Functions for Imaris vascular spot analysis
##          (includes pre-processing, manipulation, & results)
## Mina Peyton
##
##########

#Pre-processing functions:

Upper <- function(x) {x <- x %>% 
  mutate_at("Original Image Name", .funs = toupper)}


brain_region <- function(x) {x %>% 
    separate(col = "Original Image Name",
             into = c("brain_region", "image_num"),
             sep = "(?<=[A-Z]{3})", extra = "merge")}

Tspots <- function(x) {x %>% 
    group_by(brain_region) %>%
    count(brain_region)}

Aspots <- function(x) {x %>% 
    group_by(brain_region) %>%
    filter(`Shortest Distance to Surfaces Surfaces=Surfaces 1` <= 1) %>%
    count(brain_region)}

NAspots <- function(x) {x %>% 
    group_by(brain_region) %>%
    filter(`Shortest Distance to Surfaces Surfaces=Surfaces 1` > 1) %>%
    count(brain_region)} 

# Results
results <- function(x) {
  obj_name = deparse(substitute(x))
  x <- Upper(x)
  x <-brain_region(x)
  total_spots <- Tspots(x)
  A_spots <- Aspots(x)
  NA_spots <- NAspots(x)
  spots <- c(A_spots[[2]][1], 
             NA_spots[[2]][1], 
             A_spots[[2]][2],
             NA_spots[[2]][2])
  names(spots) <- c("PFC_Aspots", "PFC_NAspots", "STR_Aspots", "STR_NAspots")
  t_spots <- c(total_spots[[2]][1],
               total_spots[[2]][1],
               total_spots[[2]][2],
               total_spots[[2]][2])
  spot_percent <- (spots/t_spots)*100
  vessel_spot_results <- as_tibble(rbind(spots, t_spots,spot_percent))
  vessel_spot_results <- vessel_spot_results %>% 
    add_column(mouse_ID = substr(obj_name,1,4)) %>%
    add_column(counts = c("spots", "t_spots", "spot_percent"))
  write.csv(vessel_spot_results,
            file = paste0(obj_name, "_results.csv"),
            row.names = TRUE)
  return(vessel_spot_results)}

# calculate the group averages 

spot_means <- function(x){
  obj_name = deparse(substitute(x))
  averages <- x %>% 
    pivot_longer(cols = c(starts_with("PFC"),
                          starts_with("STR")),
                 names_to = "brain_spots",
                 values_to = "spot_percent") %>% 
    pivot_wider(names_from = counts, values_from = spot_percent) %>% 
    group_by(brain_spots) %>% 
    summarize(mean.spots = mean(spots),
              mean.t_spot = mean(t_spots),
              mean.spot_percent = mean(spot_percent))
  write.csv(averages,
            file = paste0(obj_name, "_results.csv"),
            row.names = TRUE)
  return(averages)
}
