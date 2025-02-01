library(ggplot2)

depths <- c(
  seq(0, 100, by = 5),
  seq(125, 500, by = 25),
  seq(550, 2000, by = 50),
  seq(2100, 5500, by = 100)
)
woa_colnames <- c("latitude", "longitude", paste0("depth_m_", depths))

ocean_atlas_wide <- read.csv("~/Downloads/woa23_decav_t00mn01.csv",
                             skip = 2,
                             header = FALSE,
                             col.names = woa_colnames)

nrow_wide <- nrow(ocean_atlas_wide)
nrow_long <- nrow_wide * length(depths)
ocean_atlas_long <- data.frame(longitude = rep(NA, nrow_long),
                               latitude = rep(NA, nrow_long),
                               depth_m = rep(NA, nrow_long),
                               temp_c = rep(NA, nrow_long))
row_idx <- rep(1:nrow_wide, each = length(depths))
depth_idx <- rep(1:length(depths), nrow_wide)
ocean_atlas_long$longitude <- ocean_atlas_wide[cbind(row_idx, 2)]
ocean_atlas_long$latitude <- ocean_atlas_wide[cbind(row_idx, 1)]
ocean_atlas_long$depth_m <- depths[depth_idx]
ocean_atlas_long$temp_c <- ocean_atlas_wide[cbind(row_idx, depth_idx + 2)]

# Lots of NA temperatures, we don't really need those rows
# Filter to non-NA temperatures and reassign to ocean_atlas_long
ocean_atlas_long <- ocean_atlas_long[!is.na(ocean_atlas_long$temp_c), ]

# How many locations (unique lat/lon pairs) have a temperature value at the
# surface? At 100, 1000, and 5500 m?
temps_surface <- ocean_atlas_long[ocean_atlas_long$depth_m == 0, ]
nrow(temps_surface)
temps_100m <- ocean_atlas_long[ocean_atlas_long$depth_m == 100, ]
nrow(temps_100m)
temps_1000m <- ocean_atlas_long[ocean_atlas_long$depth_m == 1000, ]
nrow(temps_1000m)
temps_5500m <- ocean_atlas_long[ocean_atlas_long$depth_m == 5500, ]
nrow(temps_5500m)

# The Mariana trench is located at 11°21′N 142°12′E
# What's the nearest lat, lon in the WOA?
mariana_coord <- c(11.5, 142.5)

# Filter the rows to that location and plot the temperature profile
mariana_temps <- ocean_atlas_long[ocean_atlas_long$latitude == 11.5 &
                                    ocean_atlas_long$longitude == 142.5, ]
ggplot(mariana_temps, aes(temp_c, depth_m)) +
  geom_path() +
  scale_y_reverse()

# Let's look at the distribution of ocean temperatures by depth. Make a
# hypothesis: do ocean temperatures have higher variance (i.e., are they more
# spread out) at the surface or at depth?

# Loop through all the depths and calculate the standard deviation (using sd())
# of the temperatures at that depth. Put the results in a data frame.
depth_temp_sd <- data.frame(
  depth_m = depths,
  temp_c_sd = NA
)
for (i in 1:nrow(depth_temp_sd)) {
  d <- depth_temp_sd$depth_m[i]
  temps <- ocean_atlas_long$temp_c[ocean_atlas_long$depth_m == d]
  depth_temp_sd$temp_c_sd[i] <- sd(temps)
}
ggplot(depth_temp_sd, aes(temp_c_sd, depth_m)) +
  geom_path() +
  scale_y_reverse()

# Does the figure provide evidence for or against your hypothesis? Explain
# briefly.

# Optional bonus


















