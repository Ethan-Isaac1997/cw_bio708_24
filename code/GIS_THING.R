## library can be used for vector analysis 
## -sf is for vector analysis 
## tidyverse: sf id compatible with tidyverse
install.packages("mapview")

# gis ---------------------------------------------------------------------

                 
pacman::p_load(tidyverse ,
                   sf)
##pre-loaded with sf package
sf_nc <- st_read(system.file("shape/nc.shp", package = "sf"))

## visualize
mapview::mapview(sf_nc)


##arrange by area
sf_nc %>% 
  arrange(AREA)
## extract by column
range(sf_nc$AREA)

## subset polygon by area

sf_nc %>% 
  filter(AREA > .05)

sf_nc_sub <- sf_nc %>% 
  filter(AREA > .1)

mapview::mapview(sf_nc_sub)



# calculate spatial properties --------------------------------------------
sf_nc_0 <- sf_nc %>% 
  dplyr::select(NULL)

##calculate area of ploygons 

sf_nc_0 %>% 
  mutate(area = st_area(.) %>% 
  units:: set_units("km^2"))

sf_nc_0 %>% 
  st_transform(crs = 5070) %>% 
  mutate(area = st_area(.) %>% 
           units:: set_units("km^2"))


# cross two layers --------------------------------------------------------

sf_point <- tibble() %>% 
  bind_rows(c(st_bbox(sf_nc))) %>% 
  reframe(Y = runif(100, ymin, ymax),
          X= runif(100, xmin, xmax)) %>% 
  mutate(point_id = row_number()) %>% 
  st_as_sf(coords = c("X", "Y")) %>% 
  st_set_crs(st_crs(sf_nc))
