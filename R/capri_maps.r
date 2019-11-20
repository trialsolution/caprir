#' Prepares the data to be mapped directly
#'
#' Combines .kml data and NUTS2 regional mappings
#'
#' @return A tibble with the merged data
#'
#' @export
#'
#' @examples
#'
#' x <- prep_mapdata() %>% left_join(co2em, by = c("CAPRI_NUTS_ID" = "region"))
#
#' # remove Portuguese islands and Canarias to remove empty spaces on EU maps...
#' x <- x %>% filter(!grepl("PT20", CAPRI_NUTS_ID)) %>% filter(!grepl("PT30", CAPRI_NUTS_ID)) %>% filter(!grepl("ES70", CAPRI_NUTS_ID))
#
#' # prepare the map with ggplot
#
#' p <- x %>% filter(!grepl("TR.*", CAPRI_NUTS_ID)) %>%
#'   ggplot(aes(longitude, latitude, group = name, fill = pc)) +
#'   geom_polygon(color = "white") +
#'   coord_map("albers", lat0=30, lat1=35) +
#'   scale_fill_gradient(low = "red", high = "white") +
#'   labs(x = "", y = "") + theme(
#'     axis.text.x = element_blank(),
#'     axis.text.y = element_blank(),
#'     axis.ticks = element_blank())
#
#' p$labels$fill <- " "
#
#
#
#' # save map to .png
#' p
#' ggsave("mapout/emission_changes.png", width = 16, height = 9)
#'
prep_mapdata <- function(){

  # read mapping CAPRI NUTS codes (8 digit) to FADN NUTS code 4 digits
  capri2fadn <- as.tibble(read.csv("inst/extdata/map/capri2fadn.csv", encoding = "UTF-8"))

  # read .kml map data
  load("data/nuts2_polygons.RData")

  # merge polygons and CAPRI regional mapping
  tomap <- nuts2_polygons %>% left_join(capri2fadn, by = c("description" = "NUTS_ID"))

  tomap$CAPRI_NUTS_ID <- as.character(tomap$CAPRI_NUTS_ID)

  return(tomap)

}
