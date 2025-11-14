library(sf)
library(leaflet)
library(dplyr)
library(RColorBrewer)
library(htmlwidgets)
library(classInt)

getwd()
setwd("C:/Users/matteo.mazzeri/OneDrive - NHS/Desktop/GIS Projects/PharmaOutcomes")

# Load data ----------------------------------------------------------------
hub_lines <- st_read("data/Hub_Lines.geojson")
imd <- st_read("Web maps/IMD Quintile by MSOA.geojson")
postal_district <- st_read("data/activity_post_dist.geojson")
activity_pharmacy <- st_read("data/activity_by_pharmacy.geojson")
boroughs_nel <- st_read("data/NEL boundaries.geojson")
post_dist_zero_act <- st_read('data/Zero Activity Post Dist.geojson')
pharmacy_breakdown <- read.csv("data/type_breakdown.csv")
# zero_activity_pharmacies <- st_read('zero activity pharmacies.geojson')


# Join the breakdown data with the spatial data
activity_pharmacy <- activity_pharmacy %>%
  left_join(pharmacy_breakdown, by = c("PharmacyCode" = "ODSCode"))

# -------------------------------------------------------------------------
# 1. HUB LINES CONFIGURATION
# -------------------------------------------------------------------------
hub_breaks <- c(1, 15, 49, 131, 281, 495)
hub_labels <- c("1–15", "15–49", "49–131", "131–281", "281–495")
hub_sizes  <- c(1, 4, 7, 9, 12) 

hub_lines <- hub_lines %>%
  mutate(
    bin  = cut(PatientLinkID, breaks = hub_breaks, labels = hub_labels, include.lowest = TRUE),
    size = case_when(
      bin == hub_labels[1] ~ hub_sizes[1],
      bin == hub_labels[2] ~ hub_sizes[2],
      bin == hub_labels[3] ~ hub_sizes[3],
      bin == hub_labels[4] ~ hub_sizes[4],
      bin == hub_labels[5] ~ hub_sizes[5]
    )
  )

# Single color with varying opacity for hub lines
hub_opacities <- c(0.2, 0.4, 0.6, 0.8, 1.0)  # Light to dark opacity

hub_lines <- hub_lines %>%
  mutate(
    opacity_level = case_when(
      bin == hub_labels[1] ~ hub_opacities[1],
      bin == hub_labels[2] ~ hub_opacities[2],
      bin == hub_labels[3] ~ hub_opacities[3],
      bin == hub_labels[4] ~ hub_opacities[4],
      bin == hub_labels[5] ~ hub_opacities[5]
    )
  )

# -------------------------------------------------------------------------
# 2. IMD QUINTILES CONFIGURATION
# -------------------------------------------------------------------------
# Convert to factor with descriptive labels
imd$MSOAQUINTI <- factor(imd$MSOAQUINTI,
                         levels = 1:5,
                         labels = c("1 (Most Deprived)", "2", "3", "4", "5 (Least Deprived)"))

# Create color palette
imd_colors <- rev(c("#FDE725", "#5DC863", "#21908C", "#3B528B", "#440154"))
imd_pal <- colorFactor(imd_colors, domain = imd$MSOAQUINTI)

# -------------------------------------------------------------------------
# 3. POSTAL DISTRICTS & PHARMACY SIZING
# -------------------------------------------------------------------------
# Postal Districts with Jenks breaks
pd_breaks <- classIntervals(postal_district$PatientLinkID, n = 5, style = "jenks")$brks
pd_labels <- paste(head(pd_breaks, -1), "-", tail(pd_breaks, -1))
pd_sizes  <- c(4, 8, 12, 16, 20)  # radii for your circles

postal_district <- postal_district %>%
  mutate(
    pd_bin  = cut(PatientLinkID, breaks = pd_breaks, labels = pd_labels, include.lowest = TRUE),
    circle_size = case_when(
      pd_bin == pd_labels[1] ~ pd_sizes[1],
      pd_bin == pd_labels[2] ~ pd_sizes[2],
      pd_bin == pd_labels[3] ~ pd_sizes[3],
      pd_bin == pd_labels[4] ~ pd_sizes[4],
      pd_bin == pd_labels[5] ~ pd_sizes[5]
    )
  )

# Pharmacies with Jenks breaks
ph_breaks <- classIntervals(activity_pharmacy$PatientLinkID, n = 5, style = "jenks")$brks
ph_labels <- paste(head(ph_breaks, -1), "-", tail(ph_breaks, -1))
ph_sizes  <- c(10, 15, 20, 25, 30)

activity_pharmacy <- activity_pharmacy %>%
  mutate(
    ph_bin  = cut(PatientLinkID, breaks = ph_breaks, labels = ph_labels, include.lowest = TRUE),
    diamond_size = case_when(
      ph_bin == ph_labels[1] ~ ph_sizes[1],
      ph_bin == ph_labels[2] ~ ph_sizes[2],
      ph_bin == ph_labels[3] ~ ph_sizes[3],
      ph_bin == ph_labels[4] ~ ph_sizes[4],
      ph_bin == ph_labels[5] ~ ph_sizes[5]
    )
  )

# -------------------------------------------------------------------------
# 4. DIAMOND ICON GENERATOR
# -------------------------------------------------------------------------
diamond_icon <- function(size = 15, color = "green", opacity = 0.7) {
  svg <- paste0(
    '<svg width="', size, '" height="', size, '" viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg">',
    '<polygon points="50,0 100,50 50,100 0,50" fill="', color, '" opacity="', opacity, '" stroke="white" stroke-width="2"/>',
    '</svg>'
  )
  makeIcon(
    iconUrl     = paste0("data:image/svg+xml;base64,", base64enc::base64encode(charToRaw(svg))),
    iconWidth   = size,
    iconHeight  = size,
    iconAnchorX = size / 2,
    iconAnchorY = size / 2
  )
}

# -------------------------------------------------------------------------
# 5. BUILD THE LEAFLET MAP
# -------------------------------------------------------------------------

# ---- Create borough interior points for label placement ----
borough_pts <- sf::st_point_on_surface(boroughs_nel) 

my_map <- leaflet() %>%
  addProviderTiles("Esri.WorldGrayCanvas") %>%
  addPolygons(
    data = boroughs_nel,
    color = "#666666", weight = 1.5,
    fillColor = "#cccccc", fillOpacity = 0.2, opacity = 1,
    options = pathOptions(interactive = FALSE),  # keep them non-blocking
    group = "Boroughs"
  )%>%
  
  # 2) Borough labels (centered, subtle grey, non-interactive)
  addLabelOnlyMarkers(
    data = borough_pts,
    label = ~lad20nm,
    labelOptions = labelOptions(
      noHide = TRUE,
      textOnly = TRUE,
      direction = "center",
      className = "borough-label"
    ),
    group = "Boroughs"
  ) %>%
  
  # IMD polygons below
  addPolygons(
    data = imd,
    fillColor = ~imd_pal(MSOAQUINTI),
    fillOpacity = 0.6,
    color = "#444444",
    weight = 0.3,
    group = "IMD Quintiles",
    options = pathOptions(interactive = FALSE)
  ) %>% 
  
  # Add Hub lines
  addPolylines(
    data = hub_lines,
    weight = ~size,
    color = "#e34a33",  # Single color for all lines
    opacity = ~opacity_level,  # Varying opacity based on value
    label = ~paste("Patients:", PatientLinkID),
    group = "Hub Lines"
  ) %>%
  
  # Add Postal District circle markers
  addCircleMarkers(
    data = postal_district,
    radius = ~circle_size,
    fillColor = "#c994c7",
    fillOpacity = 0.7,
    color = "#ffffff",
    weight = 1,
    popup = ~paste0("<strong>Postal District: </strong>", PostDist,
                    "<br><strong>Locale: </strong>", ifelse(is.na(Locale), "Unknown", Locale),
                    "<br><strong>Patient Count: </strong>", PatientLinkID),
    group = "Postal Districts"
  ) %>%
  
  # # Add Zero Activity Postal Districts (small red circles)
  addCircleMarkers(
    data = post_dist_zero_act,
    radius = 4,
    fillColor = "#ff0000",
    fillOpacity = 0.7,
    color = "#ffffff",
    weight = 1,
    popup = ~paste0(
      "<strong>Zero Activity District: </strong>", PostDist,
      "<br><strong>Status: </strong>No Activity"
    ),
    group = "Zero Activity Districts"
  )


# Add Pharmacy Diamond Icons with simple popups -------------------------
for (i in seq_len(nrow(activity_pharmacy))) {
  coords <- st_coordinates(activity_pharmacy[i, ])
  my_map <- my_map %>%
    addMarkers(
      lng   = coords[1],
      lat   = coords[2],
      icon = diamond_icon(activity_pharmacy$diamond_size[i], "green"),
      popup = paste0(
        "<strong>Pharmacy: </strong>", activity_pharmacy$Pharmacy.Name[i], "<br>",
        "<strong>Activity: </strong>", activity_pharmacy$PatientLinkID[i]
      ),
      group = "Pharmacy Activity"
    )
}

# Add Zero Activity Pharmacy Diamond Icons -------------------------------
# for (i in seq_len(nrow(zero_activity_pharmacies))) {
#   coords <- st_coordinates(zero_activity_pharmacies[i, ])
#   my_map <- my_map %>%
#     addMarkers(
#       lng   = coords[1],
#       lat   = coords[2],
#       icon = diamond_icon(10, "red", 0.7),
#       popup = paste0(
#         "<strong>Zero Activity Pharmacy</strong>",
#         ifelse("pharmacy_name" %in% names(zero_activity_pharmacies), 
#                paste0("<br><strong>Name: </strong>", zero_activity_pharmacies$pharmacy_name[i]), ""),
#         ifelse("name" %in% names(zero_activity_pharmacies), 
#                paste0("<br><strong>Name: </strong>", zero_activity_pharmacies$name[i]), ""),
#         ifelse("pharmacy.n" %in% names(zero_activity_pharmacies), 
#                paste0("<br><strong>Name: </strong>", zero_activity_pharmacies$pharmacy.n[i]), ""),
#         "<br><strong>Status: </strong>No Activity"
#       ),
#       group = "Zero Activity Pharmacies"
#     )
# }

# -------------------------------------------------------------------------
# 6. CREATE DYNAMIC LEGEND HTML
# -------------------------------------------------------------------------

# Create postal district legend HTML with actual break values
postal_legend_html <- paste0(
  '<div id="postal_legend" style="font-size:11px; display:none;">',
  '<strong>Activity by Postal District</strong>',
  '<div style="margin-top:2px;">',
  paste0(
    '<div style="display:flex; align-items:center; margin:2px 0;">',
    '<span style="display:inline-block; width:', pd_sizes * 2, 'px; height:', pd_sizes * 2, 
    'px; border-radius:50%; background:#c994c7; margin-right:6px;"></span>',
    round(head(pd_breaks, -1)), ' - ', round(tail(pd_breaks, -1)),
    '</div>',
    collapse = ''
  ),
  '</div></div>'
)

# Create hub lines legend HTML with actual break values and opacity visualization
hub_legend_html <- paste0(
  '<div id="lines_legend" style="font-size:11px; display:none;">',
  '<strong>Patients per Line</strong>',
  '<div style="margin-top:2px;">',
  paste0(
    '<div style="display:flex; align-items:center; margin:2px 0;">',
    '<span style="display:inline-block; width:20px; height:', hub_sizes, 
    'px; background:#e34a33; opacity:', hub_opacities, '; margin-right:6px;"></span>',
    hub_labels,
    '</div>',
    collapse = ''
  ),
  '</div></div>'
)

# Create pharmacy legend HTML with actual break values
pharmacy_legend_html <- paste0(
  '<div id="pharmacy_legend" style="font-size:11px; display:none;">',
  '<strong>Activity by Pharmacies</strong>',
  '<div style="margin-top:2px;">',
  paste0(
    '<div style="display:flex; align-items:center; margin:2px 0;">',
    '<svg width="', ph_sizes, '" height="', ph_sizes, 
    '" viewBox="0 0 100 100" style="margin-right:6px;">',
    '<polygon points="50,0 100,50 50,100 0,50" fill="green"/></svg>',
    round(head(ph_breaks, -1)), ' - ', round(tail(ph_breaks, -1)),
    '</div>',
    collapse = ''
  ),
  '</div></div>'
)

# -------------------------------------------------------------------------
# 7. ADD LEGENDS & CONTROLS
# -------------------------------------------------------------------------
my_map <- my_map %>%
  # IMD legend
  addLegend(
    position = "topleft",
    pal = imd_pal,
    values = imd$MSOAQUINTI,
    title = "IMD Quintile",
    opacity = 1,
    layerId = "imd_legend"
  ) %>%
  
  # Postal District legend (dynamic)
  addControl(
    html = postal_legend_html,
    position = "topleft"
  ) %>%
  
  # Hub Lines legend (dynamic)
  addControl(
    html = hub_legend_html,
    position = "topleft"
  ) %>%
  
  # Pharmacy legend (dynamic)
  addControl(
    html = pharmacy_legend_html,
    position = "topleft"
  ) %>%
  
  # Zero Activity Districts legend
  addControl(
    html = '<div id="zero_activity_legend" style="font-size:11px; display:none;">
              <strong>Zero Activity Districts</strong>
              <div style="margin-top:2px;">
                <div style="display:flex; align-items:center; margin:2px 0;"><span style="display:inline-block; width:8px; height:8px; border-radius:50%; background:#ff0000; opacity:0.7; border:1px solid #ffffff; margin-right:6px;"></span>No Activity</div>
              </div>
            </div>',
    position = "topleft"
  ) %>%
  
  # Layer controls
  addLayersControl(
    position = "topright",
    overlayGroups = c("IMD Quintiles", "Hub Lines", "Postal Districts", 
                      "Pharmacy Activity", "Zero Activity Districts"
                      # ,"Zero Activity Pharmacies"
    ),
    options = layersControlOptions(collapsed = FALSE)
  )

# ---- JavaScript: styling + legend show/hide ----
my_map <- my_map %>% htmlwidgets::onRender("
  function(el, x) {
    var map = this;

    // CSS per etichette borough (subtle grey, centered, readable)
    var style = document.createElement('style');
    style.innerHTML = `
    .leaflet-tooltip.borough-label {
        pointer-events: none;
        background: transparent;
        border: none;
        box-shadow: none;
        font-size: 20px;
        font-weight: 600;
        color: #595959;
        letter-spacing: 0.5px;
        text-shadow:
          0 0 2px rgba(255,255,255,0.7),
          0 0 4px rgba(255,255,255,0.5);
      }
    `;
    document.head.appendChild(style);

    // Funzioni helper per mostrare/nascondere legende
    function toggleLegend(legendId, isVisible) {
      var legend = document.getElementById(legendId);
      if (legend) legend.style.display = isVisible ? 'block' : 'none';
    }

    function toggleIMDLegend(isVisible) {
      var legends = document.querySelectorAll('.info.legend');
      legends.forEach(function(legend) {
        if (legend.innerHTML.includes('IMD')) {
          legend.style.display = isVisible ? 'block' : 'none';
        }
      });
    }

    // Stato iniziale: tutte le legende visibili
    toggleIMDLegend(true);
    toggleLegend('postal_legend', true);
    toggleLegend('lines_legend', true);
    toggleLegend('pharmacy_legend', true);
    toggleLegend('zero_activity_legend', true);

    // Gestione eventi on/off dei layer
    map.on('overlayadd', function(e) {
      switch(e.name) {
        case 'IMD Quintiles': toggleIMDLegend(true); break;
        case 'Postal Districts': toggleLegend('postal_legend', true); break;
        case 'Hub Lines': toggleLegend('lines_legend', true); break;
        case 'Pharmacy Activity': toggleLegend('pharmacy_legend', true); break;
        case 'Zero Activity Districts': toggleLegend('zero_activity_legend', true); break;
      }
    });

    map.on('overlayremove', function(e) {
      switch(e.name) {
        case 'IMD Quintiles': toggleIMDLegend(false); break;
        case 'Postal Districts': toggleLegend('postal_legend', false); break;
        case 'Hub Lines': toggleLegend('lines_legend', false); break;
        case 'Pharmacy Activity': toggleLegend('pharmacy_legend', false); break;
        case 'Zero Activity Districts': toggleLegend('zero_activity_legend', false); break;
      }
    });
  }
")

# Set map view and save
my_map <- my_map %>%
  setView(lng = 0.085, lat = 51.56, zoom=12)

saveWidget(my_map, file = "PharmOutcomes_map.html", selfcontained = TRUE)
