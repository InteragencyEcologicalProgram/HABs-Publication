# HABs Publication - HAB satellite data
# Purpose: Create figures of the HAB satellite data for the HABs publication:
  # 1) Area plot of pixel counts of Cyano Index categories for Franks Tract and
    # Mildred Island in 2020 and 2021
  # 2) A few maps of the HAB satellite data for a couple of days of interest in
    # 2021
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov


# 1. Global Code ----------------------------------------------------------

# Load packages
library(tidyverse)
library(lubridate)
library(scales)
library(EDBdata)
library(sf)
library(deltamapr)
library(curl)
library(glue)
library(stars)
library(patchwork)
library(here)

# Create a vector for the factor labels of the Cyano Index categories
ci_cat_labels <-
  c(
    "Non Detect",
    "Low",
    "Moderate",
    "High",
    "Very High"
  )

# Define custom color palette for the Cyano Index categories
ci_cat_color_pal <- c(
  "Non Detect" = "#CCCCCC",
  "Low" = "#7902A8FF",
  "Moderate" = "#C5407EFF",
  "High" = "#F48849FF",
  "Very High" = "#F7E225FF"
)

# Check if we are in the correct working directory
i_am("HAB_Satellite/HAB_satellite_figures.R")


# 2. Area Plot ------------------------------------------------------------

# 2.1 Prepare Count Data --------------------------------------------------

# Create a vector for the factor levels of the Cyano Index categories
ci_cat_levels <-
  c(
    "NonDetect",
    "Low",
    "Moderate",
    "High",
    "VeryHigh"
  )

# Prepare HAB satellite data for stacked area plot
df_hab_sat_area_plt <- hab_sat_ow_delta %>%
  # Restrict data to Jun-Oct in 2020-2021, Franks Tract and Mildred Island regions
  filter(
    year(Date) %in% c("2020", "2021"),
    month(Date) >= 6 & month(Date) <= 10,
    Region %in% c("Franks Tract", "Mildred Island")
  ) %>%
  # Add year variable
  mutate(Year = year(Date)) %>%
  # Add placeholder rows for data gaps that are greater than 7 days to prevent
    # interpolation of large data gaps in the plot
  group_by(Year, Region) %>%
  complete(Date = seq.Date(min(Date), max(Date), by = "day")) %>%
  arrange(Date) %>%
  mutate(
    na_val = is.na(NonDetect),
    na_val_run_total = sequence(rle(na_val)$lengths)
  ) %>%
  filter(!(na_val == TRUE & na_val_run_total < 8)) %>%
  ungroup() %>%
  select(!starts_with("na_val")) %>%
  # Restructure data to long format
  select(-InvalidOrMissing) %>%
  pivot_longer(
    cols = where(is.integer),
    names_to = "CIcategory",
    values_to = "CIcount"
  ) %>%
  # Apply factor order to the Cyano Index categories
  mutate(
    CIcategory = factor(
      CIcategory,
      levels = ci_cat_levels,
      labels = ci_cat_labels
    )
  ) %>%
  # Convert NA values in the count column to zero to break interpolation of
    # large data gaps in the plot
  replace_na(list(CIcount = 0))

# 2.2 Create Area Plot ----------------------------------------------------

area_plt_hab_sat <- df_hab_sat_area_plt %>%
  ggplot(aes(x = Date, y = CIcount, fill = CIcategory)) +
  geom_area(position = "fill") +
  facet_grid(
    rows = vars(Region),
    cols = vars(Year),
    scales = "free_x"
  ) +
  scale_x_date(
    name = "Date",
    breaks = breaks_pretty(5),
    labels = label_date_short(c(NA, "%b", "%d", NA)),
    expand = expansion(mult = 0.03)
  ) +
  scale_y_continuous(
    name = "Percent of valid pixels within each CIcyano Category",
    labels = percent_format()
  ) +
  scale_fill_manual(
    name = "CIcyano Category",
    values = ci_cat_color_pal
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  )

# Export plot as a .jpg
ggsave(
  here("HAB_Satellite/CI_category_area_plot.jpg"),
  plot = area_plt_hab_sat,
  width = 6,
  height = 5.5,
  units = "in"
)


# 3. HAB Satellite 2021 Map -----------------------------------------------

# 3.1 Import and Prepare Satellite Data -----------------------------------

# Download HAB satellite data for July and August 2021:
# Set download to TRUE if need to download harmful algal bloom (HAB) satellite data
download <- FALSE

# Download HAB satellite data if necessary
if (download == TRUE) {
  # Define subfolder directory to store .tif files
  dir_hab_sat <- here("HAB_Satellite")

  # Function to download and unzip harmful algal bloom (HAB) satellite data
    # (cyanobacteria abundance) from the https://fhab.sfei.org/ website
  download_hab <- function(hab_yr, hab_month) {
    hab_url <- glue("https://fhab.sfei.org/lib/download.php?request=download&dltype=month&year={hab_yr}&month={hab_month}&product=Mosaic")
    out_path <- file.path(dir_hab_sat, glue("mosaic_{hab_yr}_{hab_month}.zip"))

    curl_download(hab_url, out_path)
    unzip(out_path, exdir = dir_hab_sat)
    Sys.sleep(5)
  }

  # Download data for July and August 2021
  hab_2021 <- c(7, 8)
  for (i in hab_2021) {download_hab(2021, i)}

  # Remove .zip files and sentinel-3b .tif files
  invisible(file.remove(dir(path = dir_hab_sat, pattern = "zip$", full.names = TRUE)))
  invisible(file.remove(dir(path = dir_hab_sat, pattern = "sentinel-3b.+tif$", full.names = TRUE)))

  # Keep only four .tif files representing the beginning, peak, and end of the
    # 2021 Cyano bloom in Franks Tract
  fp_hab_sat <- dir(path = dir_hab_sat, pattern = "sentinel-3a.+tif$", full.names = TRUE)
  fp_hab_sat_rm <- str_subset(fp_hab_sat, "sentinel-3a.{9}(0710|0729|0810|0825)", negate = TRUE)
  invisible(file.remove(fp_hab_sat_rm))
}

# Import HAB satellite data for the beginning, peak, and end of the Cyano bloom
  # in Franks Tract in 2021:
fp_fr_bloom <- dir(
  path = here("HAB_Satellite"),
  pattern = "sentinel-3a.{9}(0710|0729|0810|0825)",
  full.names = TRUE
)

# Create a nested data frame to prepare the HAB satellite data for maps
ndf_hab_sat_map <-
  tibble(
    date_chr = c("July 10, 2021", "July 29, 2021", "Aug 10, 2021", "Aug 25, 2021"),
    strs_prx_obj = map(fp_fr_bloom, read_stars, proxy = TRUE)
  )

# Import shapefile for Franks Tract and Mildred Island
sf_franks_mildred <-
  read_sf(here("HAB_Satellite/Franks_Mildr_CCF_LibIsl.shp")) %>%
  filter(HNAME %in% c("Franks Tract", "Mildred Island"))

# Transform crs of Franks-Mildred and WW_Delta shapefiles to the crs of the HAB satellite data
crs_hab_sat <- st_crs(ndf_hab_sat_map$strs_prx_obj[[1]])
sf_franks_mildred_32611 <- st_transform(sf_franks_mildred, crs = crs_hab_sat)
WW_Delta_32611 <- st_transform(WW_Delta, crs = crs_hab_sat)

# Create a bounding box of the Franks-Mildred shapefile and add a 2.5 km buffer,
  # which will be used to crop the satellite data
bbox_franks_mildred <- st_bbox(st_buffer(sf_franks_mildred_32611, 2000))

# Prepare HAB satellite data for maps
ndf_hab_sat_map_c <- ndf_hab_sat_map %>%
  transmute(
    date_chr,
    strs_obj_f = map(
      strs_prx_obj,
      # Crop HAB satellite data to bounding box of the Franks-Mildred shapefile
      ~ st_crop(.x, bbox_franks_mildred) %>%
        # rename attribute to be more descriptive
        setNames("pixel_val") %>%
        # Convert pixel values to Cyano Index categories
        mutate(
          pixel_val = as.numeric(as.character(pixel_val)),
          pixel_val = case_when(
            pixel_val == 0 ~ "NonDetect",
            pixel_val <= 41 ~ "Low",
            pixel_val <= 99 ~ "Moderate",
            pixel_val <= 183 ~ "High",
            pixel_val <= 250 ~ "VeryHigh",
            TRUE ~ NA_character_
          ),
          pixel_val = factor(pixel_val, levels = ci_cat_levels, labels = ci_cat_labels)
        ) %>%
        # Convert to stars object
        st_as_stars()
    )
  )

# 3.2 Create Map of HAB Satellite Data ------------------------------------

# Function to create maps of HAB satellite data
create_hab_map <- function(strs_obj, map_title, x_txt_lab, y_txt_lab) {
  p <- ggplot() +
    geom_stars(data = strs_obj, na.rm = TRUE) +
    scale_fill_manual(
      name = "CIcyano Category",
      drop = FALSE,
      na.translate = FALSE,
      values = ci_cat_color_pal
    ) +
    geom_sf(
      data = WW_Delta_32611,
      alpha = 0,
      color = "royalblue3",
      size = 0.3
    ) +
    geom_sf(
      data = sf_franks_mildred_32611,
      alpha = 0,
      color = "green3",
      size = 1.1
    ) +
    coord_sf(
      xlim = c(bbox_franks_mildred$xmin, bbox_franks_mildred$xmax),
      ylim = c(bbox_franks_mildred$ymin, bbox_franks_mildred$ymax)
    ) +
    ggtitle(map_title) +
    ylab(NULL) +
    scale_x_continuous(
      name = NULL#,
      #breaks = seq(-121.68, -121.56, by = 0.04)
    ) +
    theme_bw()

  # Only include x-axis tick labels for the bottom most maps
  if (x_txt_lab == FALSE) {
    p <- p + theme(axis.text.x = element_blank())
  }

  # Only include y-axis tick labels for the left most maps
  if (y_txt_lab == FALSE) {
    p <- p + theme(axis.text.y = element_blank())
  }

  return(p)
}

# Create maps of HAB satellite data for each day
map_hab_sat <- ndf_hab_sat_map_c %>%
  mutate(
    x_txt = c(FALSE, FALSE, TRUE, TRUE),
    y_txt = c(TRUE, FALSE, TRUE, FALSE),
    hab_map = pmap(
      list(strs_obj_f, date_chr, x_txt, y_txt),
      create_hab_map
    )
  )

# Combine maps for each day into one
map_hab_sat_c <- wrap_plots(map_hab_sat$hab_map, ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")

# Export map as a .jpg
ggsave(
  here("HAB_Satellite/CI_category_map_2021.jpg"),
  plot = map_hab_sat_c,
  width = 6,
  height = 6.25,
  units = "in"
)

