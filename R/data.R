#' sites country
#'
#' A data frame with
#'
#' @format ## `sites_country`
#' A data frame with 3 rows and 3 columns:
#' \describe{
#'   \item{siteid}{Notoma DB site identification number}
#'   \item{sitename}{Site name}
#'   \item{sitedescripcion}{Site description}
#'   \item{notes}{Notes}
#'   \item{lonc}{Longitude of the centroid}
#'   \item{latc}{Latitude of the centroid}
#'   \item{geom}{Polygon}
#' }
"sites_country"

#' Lakes from Hydrolakes DB
#'
#' Extract of the dataframe of Hydrolakes DB. Please, check Hydrolakes
#' documentation for more details. This is a toy dataset for demostrative
#' purpose only.
#'
#' @format ## `lakes_country`
#' A data frame with 2,753 rows and 10 columns:
#' \describe{
#'   \item{GID_0}{Country code}
#'   \item{COUNTRY}{Countries}
#'   \item{Hylak_id}{Hydrolakes identification number}
#'   \item{Lake_name}{Lake name}
#'   \item{Vol_total}{Volumen total}
#'   \item{Shore_len}{Shore length}
#'   \item{Depth_avg}{Average Depth}
#'   \item{Elevation}{Elevation}
#'   \item{geom}{Polygon}
#'   }
"lakes_country"

#' Countries
#'
#' Country polygons for the app example in the sf format.
#'
#' @format ## `countries_sf`
#' A data frame with 134,492 rows and 9 columns:
#' \describe{
#'   \item{GID_0}{Country code}
#'   \item{COUNTRY}{Countries}
#'   \item{geom}{Polygon}
#'   }
"countries_sf"