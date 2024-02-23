# Create data frame with lake information
data <- data.frame(
  GID_0 = 1,
  COUNTRY = "USA",
  Hylak_id = 1001,
  Lake_name = "Tahoe",
  Vol_total = 15000,
  Lake_area = 496.2,
  Shore_len = 115.1,
  Depth_avg = 99,
  Elevation = 622,
  stringsAsFactors = FALSE
)

# Create a polygon geometry for the lake
polygon <- st_polygon(list(rbind(c(-120.000, 39.000),
                                 c(-120.053, 38.924),
                                 c(-120.117, 38.868),
                                 c(-120.161, 38.782),
                                 c(-120.220, 38.716),
                                 c(-120.224, 38.661),
                                 c(-120.231, 38.598),
                                 c(-120.257, 38.549),
                                 c(-120.288, 38.482),
                                 c(-120.347, 38.401),
                                 c(-120.354, 38.330),
                                 c(-120.335, 38.270),
                                 c(-120.337, 38.218),
                                 c(-120.347, 38.145),
                                 c(-120.000, 39.000))))

# Convert data frame to sf object
data_sf <- st_sf(data, geometry = st_sfc(polygon), crs = 4326)

# Print the sf object
print(data_sf)


test_that("lk_click() works", {
  testServer(hydrolakesServer,
             args = list(countries_sf,
                         r_lake_data = reactiveVal(),
                         map_shape_click = reactiveVal(),
                         r_neosites_data = reactiveVal()),
             {
               r_lake_data("Species1")
               map_shape_click =
               map_shape_click()

   print(lk_click())



             })
})

test_that("the DT table is given correct area and longitude values", {
  testServer(hydrolakesServer,
             args = list(countries_sf,
                         r_lake_data = reactiveVal(),
                         map_shape_click = reactiveVal(),
                         r_neosites_data = reactiveVal()),
             {
               r_lake_data("Species1")
               map_shape_click =
                 map_shape_click()

               print(lk_click())



             })
})