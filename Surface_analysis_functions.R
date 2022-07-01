##########
##
## 29JUN2022 Functions for Imaris vascular surface analysis
##          (includes pre-processing & results)
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

# Results
results <- function(x) {
  obj_name = deparse(substitute(x)) 
  x <- Upper(x)
  x <-brain_region(x)
  surface_results <- x %>%  
    group_by(brain_region) %>%
    summarise(Total_Area = sum(Area),
              Avg_Area = mean(Area),
              Total_Volume = sum(Volume),
              Avg_Volume = mean(Volume))
  write.csv(surface_results,
            file = paste0(obj_name, "_results.csv"),
            row.names = TRUE)
  return(surface_results)
}