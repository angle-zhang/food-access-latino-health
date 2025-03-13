get_pop_weighted_centroid <- function(geo_data, group_col, weight_col) {
  # get centroid of blocks and remove empty geometries
  geo_data %>%
    st_make_valid() %>%
    st_centroid_within_poly() %>%
    subset(!st_is_empty(geo_data)) %>%
    st_transform(4326) %>%
    rename(geometry=geom) %>%
    mean_center(group=group_col, weight=weight_col)
}

# from stack overflow
st_centroid_within_poly <- function (poly) {
  
  # check if centroid is in polygon
  ctrd <- st_centroid(poly, of_largest_polygon = TRUE)
  ctrd$indicator <- st_within(ctrd, poly, sparse = T) %>% lengths > 0
  
  # replace geometries that are not within polygon with st_point_on_surface()
  st_geometry(ctrd[!ctrd$indicator,]) <- st_geometry(st_point_on_surface(poly[!ctrd$indicator,]))
  
  ctrd %>% select(-indicator)
}

