
neosites_data <-   sites_country |>
    dplyr::mutate(siteid = as.character(siteid)) |>
    dplyr::filter(siteid %in% as.character(19477))


lake_data <-  lakes_country[st_is_within_distance(neosites_data, lakes_country, dist = 10000)[[1]],]


bbx = st_bbox(st_union(neosites_data, lake_data))

              lm =  leaflet::leaflet() |>
                leaflet::addTiles(group = "OpenStreetMap") |>
                leaflet::addProviderTiles("Esri.WorldImagery",
                                          group = "Esri.WorldImagery",
                                          options = providerTileOptions(attribution = 'Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community - Powered by Esri')) |>
                leaflet::addLayersControl(
                  baseGroups = c("EsriWorldImagery", "OpenStreetMap"),
                  options = layersControlOptions(collapsed = FALSE)
                )


if(sf::st_is(neosites_data, "POINT")){

  lm |>
    leaflet::addCircles(data = neosites_data) |> # neotoma sites
    leaflet::addPolygons(data = lake_data) |> # lakes
    fitBounds(lng1 = bbx$xmin[[1]],
              lat1= bbx$ymin[[1]],
              lng2 = bbx$xmax[[1]],
              lat2 = bbx$ymax[[1]]) |>
    leafem::addMouseCoordinates()

}else{

  lm |>
    leaflet::addPolygons(data = neosites_data)  |> # neotoma sites
    leaflet::addPolygons(data = lake_data)  |> # lakes
    fitBounds(lng1 = bbx$xmin,
              lat1= bbx$ymin,
              lng2 = bbx$xmax,
              lat2 = bbx$ymax) |>
    leafem::addMouseCoordinates()

}



