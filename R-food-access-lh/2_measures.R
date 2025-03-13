# ------ CALCULATE PROXIMITY MEASURES ------ #
proximity_measure <- function (pop_poi, near_poi) {
  # Calculate proximity measures
  st_nearest_feature(
    pop_poi,
    near_poi,
    check_crs = TRUE)
}