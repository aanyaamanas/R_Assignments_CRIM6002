getwd()
library(readr)
#only to take a peek at the data 
read.csv("Crime_Data_from_2020_to_Present.csv", nrows = 10)
library(dplyr)
library(sqldf)
library(lubridate)
library(leaflet)
library(sf)
library(RSQLite)
main_file <- "Crime_Data_from_2020_to_Present.csv"
clean <- "LA_Crime_Data-clean.csv"

infile  <- file(main_file, 'r')
outfile <- file(clean, 'w')
header <- readLines(infile, n = 1)
header <- gsub(" ", "", header) 
header <- gsub(",", ";", header) 
writeLines(header, con = outfile)

cLines <- 0
while ((length(a <- readLines(infile, n = 100000)) > 0)) 
  {
  cLines <- cLines + length(a)
  a <- gsub(",(?=([^\"]|\"[^\"]*\")*$)", ";", a, perl = TRUE) 
  writeLines(a, con = outfile)
}

close(infile)
close(outfile)
# now reading table
a <- read.table("LA_Crime_Data-clean.csv",
           sep = ";",
           nrows = 100,
           header = TRUE)
con <- dbConnect(SQLite(), dbname = "la_crime.db")

variabletypes <- dbDataType(con, a)
variabletypes[c("CrmCd","RptDistNo","PremisCd","WeaponUsedCd")] <- "TEXT"
#removing old table 
if (dbExistsTable(con, "crime")) dbRemoveTable(con, "crime")
dbWriteTable(con, "crime",
             "LA_Crime_Data-clean.csv",
             row.names = FALSE,
             header = TRUE,
             field.types = variabletypes,
             sep = ";",
             overwrite = TRUE)

dbListTables(con)
dbListFields(con, "crime")
#3 Writing SQL query 
sql_query <- dbGetQuery(con, "
  SELECT 
    LAT, 
    LON 
  FROM 
    crime 
  WHERE 
    CrmCdDesc = 'BURGLARY' AND 
    LAT IS NOT NULL AND 
    LON IS NOT NULL AND
    LAT != 0 AND 
    LON != 0;
")

#4 Hotspot map of LA and highlighting areas with high crime 
library(MASS)
library(isoband)
data_sf <- sql_query |>
  st_as_sf(coords = c("LON", "LAT"), 
           crs = 4326, 
           remove = FALSE) |>
  st_transform(crs = 26946) 
#coordinates 
xy <- data_sf |>
  st_coordinates() |>
  data.frame()

h <- c(MASS::bandwidth.nrd(xy$X),
       MASS::bandwidth.nrd(xy$Y))
mapBurglary <- MASS::kde2d(
  xy$X, xy$Y, # coordinates
  n = 200,    # a 200 x 200 grid
  h = h)
#converting to per km^2
mapBurglary$z <- mapBurglary$z * nrow(data_sf) * 10^6
breaks <- mapBurglary$z |>
  range() |>
  pretty(n = 10) 
#contour polygons 
contourBurglary <-
  isobands(mapBurglary$x,
           mapBurglary$y,
           mapBurglary$z,
           levels_low  = breaks[-length(breaks)],
           levels_high = breaks[-1]) |>
  iso_to_sfg() |>
  st_sfc(crs = 26946) |> 
  st_sf(levels_low  = breaks[-length(breaks)],
        levels_high = breaks[-1],
        geometry = _) |>
  st_transform(4326) 


# color palette gradient 
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = contourBurglary$levels_high)
# final map 

final_map <- leaflet() |>
  #darker base map 
  addProviderTiles(providers$CartoDB.DarkMatter) |>
  setView(lng = -118.2437, lat = 34.0522, zoom = 11) |> 
  addPolygons(
    # This is the cropping part 
    data = contourBurglary |>
      st_intersection(data_sf |>
                        st_geometry() |>
                        st_combine() |>
                        st_concave_hull(ratio = 0.1) |> 
                        st_buffer(200) |> 
                        st_transform(crs = 4326)), 
    fillColor = ~pal(levels_high), 
    fillOpacity = 0.6,
    color = "white", # Border color
    weight = 0.5,    # Border thickness
    label = ~paste0("Density: ",
                    format(levels_low,  scientific = FALSE), "-",
                    format(levels_high, scientific = FALSE)) |>
      lapply(htmltools::HTML),
    #hover highlight
    highlightOptions =
      highlightOptions(weight = 2, bringToFront = TRUE, color = "red")
  ) |>
  #legend
  addLegend(
    position = "bottomright",
    pal = pal,
    values = contourBurglary$levels_high,
    title = "Burglary Density<br>(Hotspot)",
    opacity = 0.7
  )
install.packages("mapview")
library(mapview)
install.packages("webshot")
library(webshot)
mapshot(final_map, file = "LA_Burglary_Heatmap.pdf")
webshot::install_phantomjs()

#second map trial 
pal_2 <- colorNumeric(
  palette = "YlOrRd",
  domain = contourBurglary$levels_high
)

# PART 6: CREATE AND SAVE THE LA HOTSPOT MAP (WITH FILTERING)

# 1. Find the median density level
# Only showing contours *above* this level.
median_density <- median(contourBurglary$levels_low)

# 2. Creating the final map object 
final_map_kde <- leaflet() |>
  # Using a LIGHT basemap for better contrast
  addProviderTiles(providers$CartoDB.Positron) |>
  setView(lng = -118.2437, lat = 34.0522, zoom = 11) |> # Center on LA
  addPolygons(
    data = contourBurglary |>
      # 1. CROPPING the map to the shape of the data
      st_intersection(data_sf |>
                        st_geometry() |>
                        st_combine() |>
                        st_concave_hull(ratio = 0.1) |> 
                        st_buffer(200) |> 
                        st_transform(crs = 4326)) |>
      
      # 2. FILTERING to show only hotspots above the median
      filter(levels_low > median_density),
    
    # Styling for the heatmap:
    fillColor = ~pal_2(levels_high),
    fillOpacity = 0.7, # Making it a bit more solid
    color = "transparent", # Removing the white borders
    weight = 0.5,    
    
    # Adding the popup label
    label = ~paste0("Density: ",
                    format(levels_low,  scientific = FALSE), "-",
                    format(levels_high, scientific = FALSE)) |>
      lapply(htmltools::HTML),
    
    # Adding the hover highlight
    highlightOptions =
      highlightOptions(weight = 2, bringToFront = TRUE, color = "red")
  ) |>
  
  # Adding the legend
  addLegend(
    position = "bottomright",
    pal = pal_2,
    values = contourBurglary$levels_high,
    title = "Burglary Density<br>(Hotspots Only)",
    opacity = 0.7
  )
dbDisconnect(con)
