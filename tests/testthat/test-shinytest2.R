library(shinytest2)

test_that("{shinytest2} recording: neotoma-lakes", {
  app <- AppDriver$new(variant = platform_variant(),
                       name = "neotoma-lakes", height = 800,
      width = 1333)
  app$set_inputs(neositeid = "9606")
  app$set_window_size(width = 1333, height = 800)
  app$click("search")
  app$set_inputs(sidebar_accordion = c("Site metadata",
                                       "User options"))
  app$set_inputs(map_accordion = "Map displaying NeotomaDB and HYDROlakeDB")
  app$set_inputs(metadata_rows_current = c(1, 2, 3),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(metadata_rows_all = c(1, 2, 3),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(metadata_state = c(1701994404769, 0, 10, "",
                                    TRUE, FALSE, TRUE,
      c(TRUE, "", TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$set_inputs(map_groups = c("Esri.WorldImagery",
                                "EsriWorldImagery", "lakes"),
      allow_no_input_binding_ = TRUE)
  app$set_inputs(map_shape_mouseover = c("1381",
                                         "0.0375054082252357",
                                         "lakes", "35.3421898059077",
      "136.264055803996"), allow_no_input_binding_ = TRUE)
  app$set_inputs(map_shape_click = c("1381", "0.228217028688986",
                                     "lakes", "35.3354682677144",
      "136.156888389645"), allow_no_input_binding_ = TRUE)
  app$set_inputs(map_click = c(35.3354682677144,
                               136.156888389645, 0.961256834622747),
      allow_no_input_binding_ = TRUE)
  app$set_inputs(lakeinfo_rows_current = c(1, 2, 3, 4, 5),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(lakeinfo_rows_all = c(1, 2, 3, 4, 5),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(lakeinfo_state = c(1701994431296, 0, 10, "", TRUE, FALSE, TRUE,
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)),
      allow_no_input_binding_ = TRUE)
  app$set_inputs(map_bounds = c(36.0424367353279, 136.950073242188,
                                34.6535445827988,
      135.192260742188), allow_no_input_binding_ = TRUE)
  app$set_inputs(map_center = c(136.071166992188, 35.3509759564216),
                 allow_no_input_binding_ = TRUE)
  app$set_window_size(width = 1333, height = 800)
  app$set_inputs(map_shape_mouseout = c("1381", "0.0424642295802924",
                                        "lakes", "35.3914639984562",
      "136.273531098327"), allow_no_input_binding_ = TRUE)
  app$expect_screenshot()
  app$expect_values()
  app$set_inputs(map_groups = c("OpenStreetMap", "Esri.WorldImagery",
                                "lakes"), allow_no_input_binding_ = TRUE)
  app$expect_screenshot()
  app$set_window_size(width = 1333, height = 800)
  app$set_inputs(modify = "No")
  app$set_inputs(map_groups = c("Esri.WorldImagery",
                                "EsriWorldImagery", "lakes"),
      allow_no_input_binding_ = TRUE)
  app$set_inputs(map_accordion = c("Map displaying NeotomaDB and HYDROlakeDB",
                                   "User decision"))
  app$set_inputs(nooptions = "Create lake polygon")
  app$set_inputs(map_bounds = c(36.0424367353279, 136.963806152344, 34.6535445827988,
      135.170288085938), allow_no_input_binding_ = TRUE)
  app$set_inputs(map_center = c(136.067047119141, 35.3509759564216),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(map_groups = c("Esri.WorldImagery",
                                "EsriWorldImagery", "editableFeatureGroup"),
      allow_no_input_binding_ = TRUE)
  app$set_window_size(width = 1333, height = 800)
  app$set_inputs(lakes_or_polygons = character(0))
  app$set_inputs(lakes_or_polygons = "Polygon")
  app$set_inputs(map_draw_start = "polygon", allow_no_input_binding_ = TRUE)
  app$set_inputs(map_click = c(35.3780285653748,
                               135.835387446981, 0.607822248539458),
      allow_no_input_binding_ = TRUE)
  app$set_inputs(map_click = c(35.1515298906385,
                               135.821648034885, 0.268426044742668),
      allow_no_input_binding_ = TRUE)
  app$set_inputs(map_click = c(35.1964313731534,
                               136.107427806486, 0.452087482702625),
      allow_no_input_binding_ = TRUE)
  app$set_inputs(map_draw_new_feature = c("Feature", 366, "polygon",
                                          "Polygon", c(c(135.836883,
      35.376232), c(135.821868, 35.151419), c(136.107664, 35.196291),
      c(135.836883,
      35.376232))), allow_no_input_binding_ = TRUE)
  app$set_inputs(map_draw_all_features = c("FeatureCollection",
                                           c("Feature", 366,
      "polygon", "Polygon", c(c(135.836883, 35.376232),
                              c(135.821868, 35.151419),
          c(136.107664, 35.196291), c(135.836883, 35.376232)))),
      allow_no_input_binding_ = TRUE)
  app$set_inputs(map_draw_stop = "polygon", allow_no_input_binding_ = TRUE)
  app$set_inputs(polyinfo_rows_current = c(1, 2, 3, 4, 5, 6),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(polyinfo_rows_all = c(1, 2, 3, 4, 5, 6),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(polyinfo_state = c(1701994478257, 0, 10, "",
                                    TRUE, FALSE, TRUE,
      c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)),
      allow_no_input_binding_ = TRUE)
  app$set_inputs(map_bounds = c(36.0424367353279,
                                136.947326660156,
                                34.6535445827988,
      135.189514160156), allow_no_input_binding_ = TRUE)
  app$set_inputs(map_center = c(136.068420410156, 35.3509759564216),
                 allow_no_input_binding_ = TRUE)
  app$set_window_size(width = 1333, height = 800)
  app$expect_values()
  app$expect_screenshot()
})
