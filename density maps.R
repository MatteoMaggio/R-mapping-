library(sf)
library(ggplot2)
library(dplyr)
library(classInt)
library(shadowtext)
library(stringr) 
# install.packages("systemfonts")

setwd("C:/Users/matteo.mazzeri/OneDrive - NHS/Desktop/GIS Projects/INT boundaries/Neighbourhood population density by LSOA 21")

# --- Read data ---
pop_density <- st_read("Population_density_LSOA21.geojson")
lsoa_boundaries <- st_read("C:/Users/matteo.mazzeri/OneDrive - NHS/Desktop/GIS Projects/INT boundaries/INT boundaries layers and csv/nel_lsoa21_int_boundaries.geojson")

int_boundaries <- st_read("C:/Users/matteo.mazzeri/OneDrive - NHS/Desktop/GIS Projects/INT boundaries/INT boundaries layers and csv/NEL_INT_Boundaries.geojson")

# --- Fix names ---
pop_density$place_nm <- ifelse(pop_density$place_nm %in% c("City of London", "Hackney"),
                               "City and Hackney", pop_density$place_nm)
places <- unique(na.omit(pop_density$place_nm))

# --- Output folder ---
dir.create("density_maps", showWarnings = FALSE)

# --- Function ---
create_density_map <- function(place_name, pop_data, boundary_data, int_boundaries) {
  message(paste("Creating map for:", place_name))
  
  # Filter data
  place_data <- pop_data[pop_data$place_nm == place_name, ]
  place_boundary <- boundary_data[boundary_data$place == place_name, ]
  place_int <- int_boundaries[int_boundaries$place == place_name, ]
  
  if (nrow(place_data) == 0) {
    message(paste("No data for:", place_name))
    return(NULL)
  }
  
  # Clean geometries
  place_data <- st_make_valid(place_data)
  place_boundary <- st_make_valid(place_boundary)
  place_int <- st_make_valid(place_int)
  
  # Jenks classification
  vals <- place_data$POP_DENS[is.finite(place_data$POP_DENS)]
  if (length(vals) < 2) {
    message(paste("Not enough data for:", place_name))
    return(NULL)
  }
  brks <- classIntervals(vals, n = 5, style = "jenks")$brks
  
  # Labels
  labels <- paste0(
    format(round(brks[-length(brks)], 0), big.mark = ","),
    " – ",
    format(round(brks[-1], 0), big.mark = ",")
  )
  
  place_data$density_class <- cut(
    place_data$POP_DENS,
    breaks = brks,
    include.lowest = TRUE,
    labels = labels
  )
  
  # Create labeled points before plotting
  place_int_labels <- place_int %>%
    st_point_on_surface() %>%
    st_as_sf() %>%
    mutate(int_name = str_wrap(int_name, width = 15)) 
  
  # --- Plot ---
  p <- ggplot() +
    geom_sf(data = place_data,
            aes(fill = density_class),
            color = "#222222",
            linewidth = 0.05) +
    
    # # Borough boundary (black)
    # geom_sf(data = place_boundary,
    #         fill = NA,
    #         color = "black",
    #         linewidth = 0.2) +
    
    # INT boundaries (dark red dashed lines)
    geom_sf(data = place_int,
            aes(linetype = "Neighbourhood boundary"),  # Add this mapping
            fill = NA,
            color = "#8B0000",
            linewidth = 0.5) +
    
    geom_shadowtext(
      data = place_int_labels,
      aes(label = int_name, geometry = geometry),
      stat = "sf_coordinates",
      color = "#8B0000",
      bg.color = "white",
      bg.r = 0.15,
      size = 3,
      fontface = "bold",
      check_overlap = TRUE
    ) + 
    
    scale_fill_brewer(palette = "Blues",
                      direction = 1,
                      guide = guide_legend(reverse = TRUE, order = 1),
                      name = "Population Density\n(per sq km)") +
    
    # Add manual scale for linetype
    scale_linetype_manual(
      name = "",  # Empty name so it appears below
      values = c("Neighbourhood boundary" = "dashed"),
      guide = guide_legend(
        order = 2,
        override.aes = list(color = "#8B0000", linewidth = 0.5)
      )
    ) + 
    
    coord_sf(datum = NA, expand = FALSE) + # remove excessive margins
    
    theme_minimal() +
    
    theme(
      plot.title.position = "plot",   # ensures true left alignment
      plot.title = element_text(hjust = 0, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0, size = 10),
      legend.position = "right",
      legend.spacing.y = unit(-0.5, "cm"),  # Add this line to reduce spacing
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    ) +
    
    labs(title = paste("Population Density -", place_name),
         subtitle = "LSOA 2021 Population density by sq km with Neighbourhood boundaries\n",
         x = NULL, y = NULL)
  
  # Save output
  filename <- paste0("density_maps/", gsub("[^A-Za-z0-9]", "_", place_name), ".jpg")
  ggsave(filename, p, width = 8, height = 6, dpi = 300)
  
  message(paste("✅ Saved:", filename))
  return(p)
}


# --- Run ---
for (p in places) {
  tryCatch({
    create_density_map(p, pop_density, lsoa_boundaries, int_boundaries)
  }, error = function(e) {
    message(paste("Error for", p, ":", e$message))
  })
}

message("\n✅ All maps saved in 'density_maps' folder.")

  



