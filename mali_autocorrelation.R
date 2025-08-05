# ==================================================================
# SCRIPT NAME: Mali_Autocorrelation.R
# PURPOSE: Spatial autocorrelation of 2025 Albarka annual survey
# AUTHOR: Nael Jean-Baptiste
# DATE: 2025-08-05
# ==================================================================


# ==================================================================
# 1. Load Required Libraries
# ==================================================================

library(tidyverse)
library(sf)
library(dplyr)
library(leaflet)
library(spdep)
library(tmap)
library(ggpmisc)

# ==================================================================
# 2. Load the datasets ( women data and village location )
# ==================================================================

livelihood_path = "C:/Users/Nael/Documents/MWDD_manuscrit/dataset/Albarka_LZ_cleaned.csv"

women_path = "C:/Users/Nael/Documents/MWDD_manuscrit/dataset/data_women.csv"


my_data1 <- read_csv(
  women_path,
  locale = locale(encoding = "Latin1"
                  ))

my_data2 <- read_csv(
  livelihood_path,
  locale = locale(encoding = "Latin1"
                  ))
names(my_data1)
names(my_data2)

# ==================================================================
# 3. Data cleaning and pre-processing
# ==================================================================

# Data Cleaning and pre-processing of my_data1

my_data1 <- my_data1 %>% # 👈 piping for selecting relevant columns      
  select(
    A01,A07_Vill,C01,C02,C03,C04,C05,C06,C07,C08,C09_Autre_legume,C10_Autre_fruit
    )%>%                 # 👈 piping for converting string to numeric value
  mutate(
    across(c(A01,C01,C02,C03,C04,C05,C06,C07,C08,C09_Autre_legume,C10_Autre_fruit), ~ if_else(.x == "Oui", 1, 0))) %>%       # 👈 piping for changing columns name
  rename(
    ID = A01,
    village = A07_Vill,
    C09 = C09_Autre_legume,
    C10 = C10_Autre_fruit
    )%>%
  drop_na()               # 👈 piping to drop row with missing values

# Data Cleaning and preprocessing  of my_data2

my_data2 <- my_data2 %>%   #piping for selecting relevant columns    
  select(
    Village,Latitude,Longitude,Commune, LZNAMEEN
  )%>%
  rename(
    zone = LZNAMEEN)%>%    # 👈 piping for changing columns name
  drop_na()                # 👈 piping to drop rows with missing values

# Creating  a column for total food groups consumed in my_data1
my_data1 <- my_data1 %>%
  mutate(diet_total = rowSums(across(C01:C10)))

# Creating a column with dichotomous values for meeting MDD-W

my_data1 <- my_data1 %>%
  mutate(MDD_W = if_else(diet_total < 5,0,1))

# Generating a dataset with percentage of  MDD_W = 1 per village

mdd_by_village <- my_data1 %>%
  group_by(village) %>%
  summarise(
    percent_mdd = round(mean(MDD_W, na.rm = TRUE) * 100, 1),
    n = n()
  )

# Rename village column in my_dataset2 to match column village in mdd_by_village
my_data2 <- my_data2 %>%
  rename(village = Village)

# Joing  the two datasets to create geo-referencial dataset
mdd_geo <- mdd_by_village %>%
  left_join(my_data2, by = "village")%>%
  drop_na()

# Saving the joined geo-referencial datas into local machine

write.csv(mdd_geo, "C:/Users/Nael/Documents/Spatial_Data_Course_R/mdd_geo.csv", row.names = FALSE)


# Viewing datasets after cleaning and pre-processing
view(my_data1)
view(my_data2)
view(mdd_by_village)
view(mdd_geo)

# ==================================================================
# 4. Initial descriptive statistics of MDD-W
# ==================================================================

# Getting the percentage by MDD_Status
my_data1 %>%
  count(MDD_W) %>%
  mutate(percent = n / sum(n) * 100)

# Generating an histogram of percentage of MDD_W per village 

ggplot(mdd_by_village, aes(x = reorder(village, percent_mdd), y = percent_mdd)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "MDD-W Percentage by Village",
    x = "Village",
    y = "Percent MDD-W"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),       # Smaller village labels
    axis.text.x = element_text(size = 9),       # Smaller x-axis numbers
    axis.title = element_text(size = 10),       # Axis titles
    plot.title = element_text(size = 12, face = "bold")  # Main title
  )

# ==================================================================
# 5.Interactive map of MDD-W variation at village level 
# ==================================================================

# Define color palette
pal <- colorNumeric(
  palette = "Reds",
  domain = mdd_geo$percent_mdd
)

# Create the interactive map
leaflet(mdd_geo) %>%
  addTiles() %>%
  addCircleMarkers(
    ~Longitude, ~Latitude,
    label = ~paste0("Livelihood: ", zone, " , ", "Village: ", village,  "  ,   "  ,
                    "MDD-W: ", percent_mdd, "%"),
    color = ~pal(percent_mdd),
    radius = 4,
    stroke = TRUE,
    fillOpacity = 0.8
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = ~percent_mdd,
    title = "Percent MDD-W",
    opacity = 1
  ) %>%
  addMiniMap(
    position = "topright",
    toggleDisplay = TRUE
  ) %>%
  addScaleBar(
    position = "bottomleft"
  )

# ==================================================================
# 6. Spatial autocorrelation analyis with Global Moran I
# ==================================================================

# Convert  data to an sf object

village_sf <- mdd_geo %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)  # WGS 84

# Create a spatial weights matrix

coords <- st_coordinates(village_sf)
knn_neighbors <- knearneigh(coords, k = 4)
knn_weights <- nb2listw(knn2nb(knn_neighbors), style = "W")  # row-standardized weights

# Run Global Moran’s I

moran_result <- moran.test(village_sf$percent_mdd, knn_weights)
print(moran_result)

# Generating a Moran Scatter Plot with ggplot


# a) Compute the spatial lag of percent_mdd
village_sf$lag_mdd <- lag.listw(knn_weights, village_sf$percent_mdd)


# b) Plot
ggplot(village_sf, aes(x = percent_mdd, y = lag_mdd)) +
  geom_point(size = 2.5, alpha = 0.7, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred", linewidth = 0.8) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    label.x.npc = "right",
    label.y.npc = "top",
    size = 3
  ) +
  labs(
    title = "Moran Scatter Plot of villages with percent of  MDD-W",
    x = "Percent MDD-W",
    y = "Percent MDD-W Lag"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold")
  )

# ==================================================================
# 7. Local Indicators of Spatial Association (LISA) analysis
# ==================================================================

# Compute Local Moran’s I
local_moran <- localmoran(village_sf$percent_mdd, knn_weights)

# Add LISA results to the village_sf
village_sf$Ii       <- local_moran[, "Ii"]
village_sf$z_score  <- local_moran[, "Z.Ii"]
village_sf$p_value  <- local_moran[, "Pr(z != E(Ii))"]  

# Define Cluster Types (hotspot, coldspot, etc.)

village_sf <- village_sf %>%
  mutate(
    cluster = case_when(
      percent_mdd >= mean(percent_mdd, na.rm = TRUE) & Ii > 0 & p_value <= 0.05 ~ "High-High (Hotspot)",
      percent_mdd < mean(percent_mdd, na.rm = TRUE)  & Ii > 0 & p_value <= 0.05 ~ "Low-Low (Coldspot)",
      percent_mdd >= mean(percent_mdd, na.rm = TRUE) & Ii < 0 & p_value <= 0.05 ~ "High-Low (Outlier)",
      percent_mdd < mean(percent_mdd, na.rm = TRUE)  & Ii < 0 & p_value <= 0.05 ~ "Low-High (Outlier)",
      TRUE ~ "Not significant"
    )
  )

# viewing the new villag_sf objecte 
view(village_sf)

# Generate a LISA Cluster Map in leaflet


# a) Create a named color palette for the cluster categories
cluster_pal <- colorFactor(
  palette = c("Hotspot" = "red", 
              "Coldspot" = "blue", 
              "Spatial Outlier" = "purple", 
              "Not significant" = "gray"),
  domain = village_sf$cluster
)

#  b) Map 

leaflet(village_sf) %>%
  addTiles() %>%
  addCircleMarkers(
    radius = 6,
    stroke = TRUE,
    weight = 1,
    color = "black",
    fillColor = ~cluster_pal(cluster),
    fillOpacity = 0.8,
    label = ~paste0("Village: ", village, "<br>",
                    "Cluster: ", cluster, "<br>",
                    "Percent MDD-W: ", percent_mdd, "%"),
    labelOptions = labelOptions(direction = "auto")
  ) %>%
  addLegend(
    "bottomright",
    pal = cluster_pal,
    values = ~cluster,
    title = "Village Cluster Type",
    opacity = 1
  ) %>%
  addMiniMap(
    position = "topright",
    toggleDisplay = TRUE
  ) %>%
  addScaleBar(
    position = "bottomleft"
  )

# ==================================================================
# 8. Script Completed
# ==================================================================
cat("Script completed successfully ✅\n")
