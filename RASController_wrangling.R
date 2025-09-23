# 11/13/24

# Emma House

# New script for processing and plotting data after mom terms calc - STEP 2

# Plots for multiple plans from different batch runs together


# Load required packages 
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)
library(patchwork)
library(stringr)
library(scales)

#### TO START: READ in pre-processed data  ####
#  (from "RASController_processing.R")

# Specify the directory containing the .RData files, list all files
data_directory <- "C:/Users/ehouse/Downloads/RAS_Controller_T2"  
rdata_files <- list.files(data_directory, pattern = "\\.RData$", full.names = TRUE)

# Read all .RData files into a list, name each element based on file names
data_lists <- lapply(rdata_files, readRDS)
names(data_lists) <- gsub("\\.RData$", "", basename(rdata_files))


# Define the look-up dictionaries for `u` and `g` indices
# ADD MORE AS NEEDED!
u_description <- list(
  "u01" = "Base Flood",
  "u02" = "Normal Depth",
  "u03" = "Normal Depth",
  "u06" = "Intense Flood",
  "u10" = "Low Intensity",
  "u04" = "Skew Base",
  "u05" = "Skew Intense",
  "u21" = "Skew Low",
  "u27" = "Multi-Pulse",
  "u28" = "Long MP",
  "u22" = "0.5x BW",
  "u29" = "0.75x BW",
  "u31" = "1.25x BW",
  "u25" = "Minor Flood",
  "u26" = "Skew Minor",
  "u23" = "Normal Depth",
  "u24" = "Normal Depth",
  "u33" = "HQRC",
  "u34" = "IVRC",
  "u35" = "Johns",
  "u36" = "Fenton"
)

g_description <- list(
  "g01" = "Base (n=0.03, S0=0.00027)",
  "g04" = "Rough (n=0.1, S0=0.00027)",
  "g10" = "Smooth (n=0.02, S0=0.00027)",
  "g09" = "Rougher (n=0.2, S0=0.00027)", 
  "g02" = "Steep (n=0.03, S0=0.0005)",
  "g05" = "Mild (n=0.03, S0=0.0001)",
  "g15" = "Steepest (n=0.03, S0=0.001)",
  "g03" = "Steep & Rough (n=0.1, S0=0.0005)",
  "g06" = "Mild & Rough (n=0.1, S0=0.0001)",
  "g07" = "Mild & Rougher (n=0.2, S0=0.0001)",
  "g08" = "Mild & Smooth (n=0.02, S0=0.0001)",
  "g11" = "Steep & Smooth (n=0.02, S0=0.0005)",
  "g12" = "Steep & Rougher (n=0.2, S0=0.0005)",
  "g14" = "Steepest & Smooth (n=0.02, S0=0.001)",
  "g16" = "Steepest & Rough (n=0.1, S0=0.001)",
  "g17" = "Steepest & Rougher (n=0.2, S0=0.001)",
  "g18" = "Contracting (n=0.03, S0=0.00027)",
  "g19" = "Expanding (n=0.03, S0=0.00027)",   
  "g23" = "Compound (n=0.03, S0=0.00027)"  
)

g_bedslopevalue <- list(
  "g01" = 0.00027,
  "g04" = 0.00027,
  "g10" = 0.00027,
  "g09" = 0.00027, 
  "g02" = 0.0005,
  "g05" = 0.0001,
  "g15" = 0.001,
  "g03" = 0.0005,
  "g06" = 0.0001,
  "g07" = 0.0001,
  "g08" = 0.0001,
  "g11" = 0.0005,
  "g12" = 0.0005,
  "g14" = 0.001,
  "g16" = 0.001,
  "g17" = 0.001,
  "g18" = 0.00027,
  "g19" = 0.00027,
  "g23" = 0.00027
)

g_roughnessvalue <- list(
  "g01" = 0.03,
  "g04" = 0.1,
  "g10" = 0.02,
  "g09" = 0.2, 
  "g02" = 0.03,
  "g05" = 0.03,
  "g15" = 0.03,
  "g03" = 0.1,
  "g06" = 0.1,
  "g07" = 0.2,
  "g08" = 0.02,
  "g11" = 0.02,
  "g12" = 0.2,
  "g14" = 0.02, 
  "g16" = 0.1,
  "g17" = 0.2,
  "g18" = 0.03,
  "g19" = 0.03,
  "g23" = 0.03
)

g_shapedescrip <- list(
  "g01" = "Rectangular/Prismatic",
  "g18" = "Rectangular/Contracting",
  "g19" = "Rectangular/Expanding",
  "g23" = "Compound/Prismatic",
  "g04" = "Rectangular",
  "g10" = "Rectangular",
  "g09" = "Rectangular", 
  "g02" = "Rectangular",
  "g05" = "Rectangular",
  "g15" = "Rectangular",
  "g03" = "Rectangular",
  "g06" = "Rectangular",
  "g07" = "Rectangular",
  "g08" = "Rectangular",
  "g11" = "Rectangular",
  "g12" = "Rectangular",
  "g14" = "Rectangular",
  "g16" = "Rectangular",
  "g17" = "Rectangular"
)

u_shapedescrip <- list(
  "u01" = "Base Flood",
  "u06" = "Intense Flood",
  "u10" = "Low Intensity",
  "u04" = "Skew Base",
  "u05" = "Skew Intense",
  "u21" = "Skew Low",
  "u25" = "Minor Flood",
  "u26" = "Skew Minor",
  "u02" = "Base Flood",
  "u03" = "Base Flood",
  "u27" = "Multi-Pulse",
  "u28" = "Long MP",
  "u22" = "Base Flood",
  "u29" = "Base Flood",
  "u31" = "Base Flood",
  "u23" = "Base Flood",
  "u24" = "Base Flood",
  "u33" = "Base Flood",
  "u34" = "Base Flood",
  "u35" = "Base Flood",
  "u36" = "Base Flood"
)

u_bwdescrip <- list(
  "u01" = "Base BW",
  "u02" = "Normal Depth",   # if you want to plot ND vs. backwater ...
  "u03" = "Normal Depth",
  "u06" = "Base Flood",
  "u10" = "Base Flood",
  "u04" = "Base Flood",
  "u05" = "Base Flood",
  "u21" = "Base Flood",
  "u27" = "Base Flood",
  "u28" = "Base Flood",
  "u22" = "0.5x BW",
  "u29" = "0.75x BW",
  "u31" = "1.25x BW",
  "u25" = "Base Flood",
  "u26" = "Base Flood",
  "u23" = "Normal Depth",
  "u24" = "Normal Depth",
  "u33" = "Base Flood",
  "u34" = "Base Flood",
  "u35" = "Base Flood",
  "u36" = "Base Flood"
)

n_description <- list(
  "g01" = "Base (n=0.03)",
  "g04" = "Rough (n=0.1)",
  "g10" = "Smooth (n=0.02)",
  "g09" = "Rougher (n=0.2)", 
  "g02" = "Base (n=0.03)",
  "g05" = "Base (n=0.03)",
  "g15" = "Base (n=0.03)",
  "g03" = "Rough (n=0.1)",
  "g06" = "Rough (n=0.1)",
  "g07" = "Rougher (n=0.2)",
  "g08" = "Smooth (n=0.02)",
  "g11" = "Smooth (n=0.02)",
  "g12" = "Rougher (n=0.2)",
  "g14" = "Smooth (n=0.02)",
  "g16" = "Rough (n=0.1)",
  "g17" = "Rougher (n=0.2)",
  "g18" = "Base (n=0.03)",
  "g19" = "Base (n=0.03)",   
  "g23" = "Base (n=0.03)"  
)

s_description <- list(
  "g01" = "Base (S0=0.00027)",
  "g04" = "Base (S0=0.00027)",
  "g10" = "Base (S0=0.00027)",
  "g09" = "Base (S0=0.00027)", 
  "g02" = "Steep (S0=0.0005)",
  "g05" = "Mild (S0=0.0001)",
  "g15" = "Steepest (S0=0.001)",
  "g03" = "Steep (S0=0.0005)",
  "g06" = "Mild (S0=0.0001)",
  "g07" = "Mild (S0=0.0001)",
  "g08" = "Mild (S0=0.0001)",
  "g11" = "Steep (S0=0.0005)",
  "g12" = "Steep (S0=0.0005)",
  "g14" = "Steepest (S0=0.001)",
  "g16" = "Steepest (S0=0.001)",
  "g17" = "Steepest (S0=0.001)",
  "g18" = "Base (S0=0.00027)",
  "g19" = "Base (S0=0.00027)",   
  "g23" = "Base (S0=0.00027)"  
)

# Reset combined_data_list if needed
combined_data_list <- list()


# Combine lists of data frames and trim the prefix, then add descriptions and values
for (lst in data_lists) {  # Loop through each list in the main list
  for (df_name in names(lst)) {
    # Remove "RASResults_" prefix and store in a new name
    new_name <- sub("RASResults_", "", df_name)
    
    # Extract `u` and `g` indices using regular expressions
    u_index <- sub(".*(u\\d{2})_.*", "\\1", new_name)    # Matches `u` pattern
    g_index <- sub(".*_(g\\d{2}).*", "\\1", new_name)    # Matches `g` pattern
    
    # Fetch descriptions based on indices
    u_label <- u_description[[u_index]]
    g_label <- g_description[[g_index]]
    g_shape <- g_shapedescrip[[g_index]]
    u_shape <- u_shapedescrip[[u_index]]
    u_bw <- u_bwdescrip[[u_index]]
    n_descrip <- n_description[[g_index]]
    s_descrip <- s_description[[g_index]]

    # Fetch roughness and bed slope values based on indices
    roughnessval <- g_roughnessvalue[[g_index]]
    bedslopeval <- g_bedslopevalue[[g_index]]
    
    # Add the data frame to the combined list with a renamed and annotated entry
    combined_data_list[[new_name]] <- lst[[df_name]]
    
    # Attach the `u` and `g` labels as attributes for later reference
    attr(combined_data_list[[new_name]], "flow_condition") <- u_label
    attr(combined_data_list[[new_name]], "geometry_condition") <- g_label
    
    # Attach the `g` values as attributes for later reference
    attr(combined_data_list[[new_name]], "roughness_value") <- roughnessval
    attr(combined_data_list[[new_name]], "bedslope_value") <- bedslopeval
    attr(combined_data_list[[new_name]], "channel_shape") <- g_shape
    attr(combined_data_list[[new_name]], "floodwave_shape") <- u_shape
    attr(combined_data_list[[new_name]], "backwater_condition") <- u_bw
    attr(combined_data_list[[new_name]], "roughness_condition") <- n_descrip
    attr(combined_data_list[[new_name]], "bed_slope_condition") <- s_descrip
  }
}


# View attributes to confirm the setup  
for (df_name in names(combined_data_list)) {
  cat("\nData Frame:", df_name)
  cat("\nFlow Condition:", attr(combined_data_list[[df_name]], "flow_condition"))
  cat("\nGeometry Condition:", attr(combined_data_list[[df_name]], "geometry_condition"))
  cat("\nBed Slope Value:", attr(combined_data_list[[df_name]], "bedslope_value"))
  cat("\nRoughness Value:", attr(combined_data_list[[df_name]], "roughness_value"))
  cat("\nChannel Shape:", attr(combined_data_list[[df_name]], "channel_shape"))
  cat("\nFloodwave Shape:", attr(combined_data_list[[df_name]], "floodwave_shape"))
  cat("\nBackwater Condition:", attr(combined_data_list[[df_name]], "backwater_condition"))
  cat("\nRoughness Condition:", attr(combined_data_list[[df_name]], "roughness_condition"))
  cat("\nBed Slope Condition:", attr(combined_data_list[[df_name]], "bed_slope_condition"), "\n")
}



# Convert the combined list into a single data frame, including attributes as columns
combined_data <- map_df(names(combined_data_list), function(df_name) {
  df <- combined_data_list[[df_name]]
  df %>%
    mutate(
      DateTime = df$DateTime,  
      FlowCondition = attr(df, "flow_condition"),
      GeometryCondition = attr(df, "geometry_condition"),
      BedSlopeValue = attr(df, "bedslope_value"),
      RoughnessValue = attr(df, "roughness_value"),
      ChannelShape = attr(df, "channel_shape"),
      FloodwaveShape = attr(df, "floodwave_shape"),
      BackwaterCondition = attr(df, "backwater_condition"), # redundant? we file it with flowcondition below.. above?
      RoughnessCondition = attr(df, "roughness_condition"),
      BedSlopeCondition = attr(df, "bed_slope_condition"),
      dataset = df_name  
    )
})



# ADD TO THIS AS YOU GO 
# Define order factors to control facet layout order by flow and geometry conditions
# Bed and roughness ordering is redundant, they self order since they're values
combined_data <- combined_data %>%
  mutate(
    FlowCondition = factor(FlowCondition, levels = c("Minor Flood", "Low Intensity", "Base Flood", "Intense Flood", 
                                                     "Skew Minor", "Skew Low", "Skew Base", "Skew Intense", 
                                                     "Normal Depth", "Multi-Pulse", "Long MP",
                                                     "IVRC", "HQRC", "Johns", "Fenton")),
    BackwaterCondition = factor(BackwaterCondition, levels = c("0.5x BW", "0.75x BW", "Base BW", "1.25x BW")),
    ChannelShape = factor(ChannelShape, levels = c("Rectangular/Prismatic", "Rectangular/Expanding", "Rectangular/Contracting", "Compound/Prismatic"))
  )


# A check
print(unique(combined_data$FlowCondition))
print(unique(combined_data$BackwaterCondition))
print(unique(combined_data$dataset))
print(unique(combined_data$ChannelShape))


# Debugging - if you don't have the expected number of scenarios (274) What's missing?
scenarios <- (unique(combined_data$dataset))
# Extract uXX and gXX using regular expressions
u_vals <- sub("_.*", "", scenarios)       # Gets the uXX part
g_vals <- sub(".*_", "", scenarios)       # Gets the gXX part
# Count occurrences
u_counts <- table(u_vals)
g_counts <- table(g_vals)
# Print the results and compare with sensitivity_scenario_trials... 
print(u_counts)
print(g_counts)




#### TO USE: CUSTOMIZE by selecting from below  ####

# Select subsection of data to plot... ONE of the below...
# u1: four flood intensities
data_subset <- combined_data %>%
  filter(FlowCondition %in% c("Minor Flood", "Base Flood", "Low Intensity", "Intense Flood"))
# u2: four skewed flood intensities
data_subset <- combined_data %>%
  filter(FlowCondition %in% c("Skew Minor", "Skew Base", "Skew Low", "Skew Intense"))  
# u3: normal depth condition
data_subset <- combined_data %>%
  filter(FlowCondition %in% c("Base Flood", "Normal Depth"))
# u4: backwater condition
data_subset <- combined_data %>%
  filter(BackwaterCondition %in% c("0.5x BW", "0.75x BW", "Base BW", "1.25x BW"))
# Matrix of roughness x bed geometries: do them one at a time, base flood first (or only)
data_subset <- combined_data %>%
  filter(str_detect(dataset, "u01"))
#  filter(str_detect(dataset, "u02|u03|u23|u24")) # For normal depth
# u5: floodwave shapes
data_subset <- combined_data %>%
  filter(FlowCondition %in% c("Base Flood", "Skew Base", "Multi-Pulse", "Long MP"))
# Q estimation methods
data_subset <- combined_data %>%
  filter(str_detect(dataset, "u33|u34|u35|u36")) 
# New Qestimation methods + ND -- run this then prismatic channel xs shapes below
data_subset <- combined_data %>%
  filter(str_detect(dataset, "u33|u34|u02")) 
# Newer Qestimation methods + base -- run this then prismatic channel xs shapes below
data_subset <- combined_data %>%
  filter(str_detect(dataset, "u01|u33|u34")) 



# Subset again just for geometries... ONE of the below...
# Roughness
data_subset <- data_subset %>%
  filter(str_detect(dataset, "g10|g01|g04|g09"))
# OR Bed slope
data_subset <- data_subset %>%
  filter(str_detect(dataset, "g05|g01|g02|g15")) 
# Both roughness and bed slope
data_subset <- data_subset %>%
  filter(str_detect(dataset, "g10|g01|g04|g09|g05|g02|g15|g08|g14|g11|g06|g03|g16|g07|g12|g17"))
# Channel XS Shapes
data_subset <- data_subset %>%
  #filter(str_detect(dataset, "g01|g18|g19|g23")) 
  filter(str_detect(dataset, "g01|g23"))     # prismatic only
# If you just want base geometry to compare flow files
data_subset <- data_subset %>%
  filter(str_detect(dataset, "g01"))


# Check
print(unique(data_subset$dataset))
print(unique(data_subset$BackwaterCondition))



# Filter the data to remove rows with noise via Profile values in the specified ranges
# Also solves the issue with large values at end of sim if you haven't fixed that
data_subset <- data_subset %>%
  filter(!(Profile >= 1 & Profile <= 150) & !(Profile >= 34550 & Profile <= 36001))

# Check out the data
tapply(data_subset$`W.S..El..(m)`, data_subset$dataset, summary)  # Stage
tapply(data_subset$`Tot.Q.(cms)`, data_subset$dataset, summary)  # Flow




#### TO PLOT: RUN EACH modified based on your selections ####

# Run one of the two blocks depending on how you subset it:

# Option 1: Add "n = " prefix to RoughnessCondition values for facet labels
data_subset <- data_subset %>%
  mutate(RoughnessValue = paste0("n = ", RoughnessValue))

# Option 2: For bed slope, first get rid of scientific notation
data_subset$BedSlopeValue <- format(as.numeric(data_subset$BedSlopeValue), scientific = FALSE)
data_subset$BedSlopeValue <- sub("0+$", "", as.character(data_subset$BedSlopeValue))
# Add "S0 = " prefix to BedSlopeValue values for facet labels
data_subset <- data_subset %>%
  mutate(BedSlopeValue = paste0("S0 = ", BedSlopeValue))


# Calculate limits for the subset
ws_el_limits <- range(data_subset$`W.S..El..(m)`, na.rm = TRUE)
tot_q_limits <- range(data_subset$`Tot.Q.(cms)`, na.rm = TRUE)
scaling_factor <- diff(ws_el_limits) / diff(tot_q_limits)


# First plotting function! Stage and Flow Hydrographs
p1 <- ggplot(data_subset, aes(x = DateTime)) +
  geom_line(aes(y = `W.S..El..(m)`, color = "Water Surface Elevation (m)"), linewidth = 0.8) + 
  geom_line(aes(y = (`Tot.Q.(cms)` - tot_q_limits[1]) * scaling_factor + ws_el_limits[1], color = "Discharge (cms)"), linewidth = 0.8) +
  # Select one facet_grid line based on your subset
  #facet_grid(FlowCondition ~ BedSlopeValue, scales = "free_y") +        # bed
  #facet_grid(FlowCondition ~ RoughnessValue, scales = "free_y") +       # rough
  #facet_grid(BackwaterCondition ~ BedSlopeValue, scales = "free_y") +        # bed x bw
  #facet_grid(BackwaterCondition ~ RoughnessValue, scales = "free_y") +       # rough x bw
  #facet_grid(RoughnessValue ~ BedSlopeValue, scales = "free_y") +       # rough x bed
  facet_grid(FlowCondition ~ ChannelShape, scales = "free_y") +        # flow condition x channel shape (for Q estimation, ND batches too)
  #facet_grid(BackwaterCondition ~ ChannelShape, scales = "free_y") +        # bw x channel shape 
  scale_y_continuous(
    name = "Water Surface Elevation (m)",  # Custom y-axis label for primary axis
    limits = ws_el_limits,
    labels = scales::label_number(accuracy = 1, big.mark = ","),
    sec.axis = sec_axis(
      ~ (. - ws_el_limits[1]) / scaling_factor + tot_q_limits[1],
      name = "Discharge (cms)",  # Custom y-axis label for secondary axis
      labels = scales::label_number(accuracy = 1, big.mark = ",")
    )
  ) +
  scale_x_datetime(
    date_labels = "%m/%d",
    date_breaks = "8 days"
  ) +
  labs(
    # Select one title line based on your subset
    #title = "Flow and Stage Hydrographs by Bed Slope, Flood Intensities",   #u1 
    #title = "Flow and Stage Hydrographs by Roughness Condition, Flood Intensities",   #u1 
    #title = "Flow and Stage Hydrographs by Channel Shape, Flood Intensities",   #u1  
    #title = "Flow and Stage Hydrographs by Bed Slope, Skewed Flood Intensities",   #u2 
    #title = "Flow and Stage Hydrographs by Roughness Condition, Skewed Flood Intensities",   #u2 
    #title = "Flow and Stage Hydrographs by Channel Shape, Skewed Flood Intensities",   #u2
    #title = "Flow and Stage Hydrographs by Bed Slope, Normal Depth comparison",   #u3 
    #title = "Flow and Stage Hydrographs by Roughness Condition, Normal Depth comparison",   #u3 
    #title = "Flow and Stage Hydrographs by Channel Shape, Normal Depth comparison",   #u3 
    #title = "Flow and Stage Hydrographs by Bed Slope, Backwater Conditions",   #u4 
    #title = "Flow and Stage Hydrographs by Roughness, Backwater Conditions",   #u4 
    #title = "Flow and Stage Hydrographs by Channel Shape, Backwater Conditions",   #u4
    #title = "Flow and Stage Hydrographs by Roughness, Hydrograph Shapes",   #u5 
    #title = "Flow and Stage Hydrographs by Bed Slope, Hydrograph Shapes",   #u5 
    #title = "Flow and Stage Hydrographs by Channel Shape, Hydrograph Shapes",   #u5 
    #title = "Flow and Stage Hydrographs by Bed Slope & Roughness (Normal Depth)",   #rough x bed 
    title = "Flow and Stage Hydrographs by Streamflow Estimation Method",   #u6 , #u7 is with ND, #u8 is with base!
    x = "DateTime"
  ) +
  scale_color_manual(
    values = c("Water Surface Elevation (m)" = "blue", "Discharge (cms)" = "black")
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text = element_text(size = 16),   # Reduces font size of axis labels
    strip.text = element_text(size = 16),  # Facet labels size
    panel.spacing = unit(1, "lines"),      # Adjust spacing between facets
    legend.text = element_text(size = 16),       # Increase legend text size
    legend.key.size = unit(2, "lines"),        # Increase legend icon size
    axis.title = element_text(size = 18),        # Increase axis labels font size
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5), # Larger and centered title
  )

#p1  # run this line to plot in R rn

# Save hydrograph plot, renaming with each permutation
#ggsave("p1_u1_rough.jpg", p1, width = 16, height = 8, dpi = 300)



# Second plot! Stage-Discharge Rating Curves differentiating rising and falling limbs
# Identify the rising and falling stages for each dataset
data_subset <- data_subset %>%
  group_by(dataset) %>%
  mutate(
    peak_index = which.max(`W.S..El..(m)`),  # Index of the peak W.S..El..(m)
    stage = ifelse(row_number() <= peak_index, "Rising Stage", "Falling Stage")  # Classify stages
  ) %>%
  ungroup()

# Create a scatter plot of W.S..El..(m) vs Tot.Q.(cms) for each dataset in separate panels
# Plot with different colors for rising and falling
p2 <- data_subset %>%
  ggplot(aes(x = `Tot.Q.(cms)`, y = `W.S..El..(m)`, color = stage)) +
  geom_point(size = 0.3) +  # Adjust point size in the plot
  # Select one facet_grid line based on your subset
  #facet_grid(FlowCondition ~ BedSlopeValue, labeller = label_wrap_gen(width = 10)) +  
  #facet_grid(FlowCondition ~ RoughnessValue, labeller = label_wrap_gen(width = 10)) +  
  #facet_grid(BackwaterCondition ~ BedSlopeValue, scales = "free_y") +        # bed x bw
  #facet_grid(BackwaterCondition ~ RoughnessValue, scales = "free_y") +       # rough x bw
  #facet_grid(RoughnessValue ~ BedSlopeValue, scales = "free_y") +        # rough x bed
  facet_grid(FlowCondition ~ ChannelShape, scales = "free_y") +        # flow condition x channel shape
  #facet_grid(BackwaterCondition ~ ChannelShape, scales = "free_y") +        # bw x channel shape 
  scale_x_continuous(
    labels = scales::comma,
    breaks = scales::breaks_pretty(n = 5)  # Dynamically adjust x-axis breaks
  ) +
  scale_y_continuous(labels = scales::comma,
                     limits = ws_el_limits,) +  # Format y-axis labels
  scale_color_manual(
    values = c("Rising Stage" = "red", "Falling Stage" = "blue")
  ) +
  labs(
    # Select one title line based on your subset
    #title = "Stage-Discharge Rating Curves by Bed Slope, Flood Intensities",  # u1
    #title = "Stage-Discharge Rating Curves by Roughness Conditions, Flood Intensities",  # u1
    #title = "Stage-Discharge Rating Curves by Channel Shape, Flood Intensities",   #u1  
    #title = "Stage-Discharge Rating Curves by Bed Slope, Skewed Flood Intensities",  # u2
    #title = "Stage-Discharge Rating Curves by Roughness Conditions, Skewed Flood Intensities",  # u2
    #title = "Stage-Discharge Rating Curves by Channel Shape, Skewed Flood Intensities",   #u2  
    #title = "Stage-Discharge Rating Curves by Bed Slope, Normal Depth comparison",  # u3
    #title = "Stage-Discharge Rating Curves by Roughness Conditions, Normal Depth comparison",  # u3
    #title = "Stage-Discharge Rating Curves by Channel Shape, Normal Depth comparison",  # u3
    #title = "Stage-Discharge Rating Curves by Bed Slope, Backwater Conditions",  # u4
    #title = "Stage-Discharge Rating Curves by Roughness, Backwater Conditions",  # u4
    #title = "Stage-Discharge Rating Curves by Channel Shape, Backwater Conditions",  # u4
    #title = "Stage-Discharge Rating Curves by Bed Slope & Roughness (Normal Depth)",   #rough x bed 
    #title = "Stage-Discharge Rating Curves by Roughness, Hydrograph Shapes",  # u5
    #title = "Stage-Discharge Rating Curves by Bed Slope, Hydrograph Shapes",  # u5
    #title = "Stage-Discharge Rating Curves by Channel Shape, Hydrograph Shapes",  # u5
    title = "Stage-Discharge Rating Curves by Streamflow Estimation Method",   #u6, #u8
    #title = "Stage-Discharge Rating Curves by Hydraulic Modeling Method",   #u7
    x = "Discharge (cms)",
    y = "Water Surface Elevation (m)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16),  # Rotate and resize x-axis labels
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text = element_text(size = 16),   # Resize axis text
    strip.text = element_text(size = 16),  # Facet labels size
    panel.spacing = unit(1, "lines"),      # Adjust spacing between facets
    legend.text = element_text(size = 16),       # Increase legend text size
    legend.key.size = unit(12, "points"),        # Increase legend key size
    axis.title = element_text(size = 18),        # Increase axis labels font size
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5) # Larger and centered title
  ) +
  guides(
    color = guide_legend(
      override.aes = list(size = 3)  # Increase legend icon size (points)
    )
  )
#p2

# Save streamflow hysteresis plot, renaming with each permutation
#ggsave("p2_u1_rough.jpg", p2, width = 16, height = 8, dpi = 300)



# Third plot! Kinematic (gASf, gAS0) and diffusive (gA(dy/dx)) terms
# Combine axis limits for the three variables across all datasets
kindiff_limits <- range(
  data_subset$`gASf`, 
  data_subset$`gAS0`, 
  data_subset$`diff.(m3/s2)`, 
  na.rm = TRUE
)

# Alternatively, since compound channel goes wild, restrict axis limits (these values just apply to our dataset)
kindiff_limits <- range(-2, 25) # channel shape x Q estimation batch
kindiff_limits <- range(-5, 40) # channel shape x flood intensities batch (also skewed flood intensities)
kindiff_limits <- range(-10, 25) # channel shape x normal depth batch
kindiff_limits <- range(-5, 30) # channel shape x hydrograph shape batch
kindiff_limits <- range(-10, 60) # channel shape x backwater batch

# Plot on the same y-axis
p3 <- ggplot(data_subset, aes(x = DateTime)) +
  geom_line(aes(y = `gASf`, color = "gASf")) + 
  geom_line(aes(y = `gAS0`, color = "gAS0")) + 
  geom_line(aes(y = `diff.(m3/s2)`, color = "gA(dy/dx)")) +
  #geom_point(aes(y = `gAS0`, color = "gAS0")) +                     # alternatively plot points
  #geom_point(aes(y = `diff.(m3/s2)`, color = "gA(dy/dx)")) +        # alternatively plot points
  # Select one facet_grid line based on your subset
  #facet_grid(FlowCondition ~ RoughnessValue, scales = "free_y") +
  #facet_grid(FlowCondition ~ BedSlopeValue, scales = "free_y") +
  #facet_grid(BackwaterCondition ~ BedSlopeValue, scales = "free_y") +        # bed x bw
  #facet_grid(BackwaterCondition ~ RoughnessValue, scales = "free_y") +       # rough x bw
  #facet_grid(RoughnessValue ~ BedSlopeValue, scales = "free_y") +        # rough x bed
  facet_grid(FlowCondition ~ ChannelShape, scales = "free_y") +        # flow condition x channel shape
  #facet_grid(BackwaterCondition ~ ChannelShape, scales = "free_y") +        # bw x channel shape 
  scale_y_continuous(name = "Term (m3/s2)", 
                     limits = kindiff_limits,
                     labels = comma) +
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "8 days") +  
  # Select one title line based on your subset
  #labs(title = "Kinematic and Diffusive Terms by Roughness Conditions, Flood Intensities",  # u1
  #labs(title = "Kinematic and Diffusive Terms by Bed Slope, Flood Intensities",    #u1
  #labs(title = "Kinematic and Diffusive Terms by Channel Shape, Flood Intensities",    #u1
  #labs(title = "Kinematic and Diffusive Terms by Roughness Conditions, Skewed Flood Intensities",  # u2
  #labs(title = "Kinematic and Diffusive Terms by Bed Slope, Skewed Flood Intensities",    #u2
  #labs(title = "Kinematic and Diffusive Terms by Channel Shape, Skewed Flood Intensities",    #u2
  #labs(title = "Kinematic and Diffusive Terms by Roughness Conditions, Normal Depth comparison",   # u3
  #labs(title = "Kinematic and Diffusive Terms by Bed Slope, Normal Depth comparison",     #u3
  #labs(title = "Kinematic and Diffusive Terms by Channel Shape, Normal Depth comparison",     #u3
  #labs(title = "Kinematic and Diffusive Terms by Roughness, Backwater Conditions",   # u4
  #labs(title = "Kinematic and Diffusive Terms by Bed Slope, Backwater Conditions",     #u4
  #labs(title = "Kinematic and Diffusive Terms by Channel Shape, Backwater Conditions",     #u4
  #labs(title = "Kinematic and Diffusive Terms by Bed Slope & Roughness (Normal Depth)",   #rough x bed 
  #labs(title = "Kinematic and Diffusive Terms by Roughness, Hydrograph Shapes",   # u5
  #labs(title = "Kinematic and Diffusive Terms by Bed Slope, Hydrograph Shapes",   # u5
  #labs(title = "Kinematic and Diffusive Terms by Channel Shape, Hydrograph Shapes",   # u5
  labs(title = "Kinematic and Diffusive Terms by Streamflow Estimation Method",     #u6, #u8
  #labs(title = "Kinematic and Diffusive Terms by Hydraulic Modeling Method",   #u7
       x = "DateTime") +
  scale_color_manual(values = c("gA(dy/dx)" = "gray", "gASf" = "orange", "gAS0" = "black")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16),  # Rotate and resize x-axis labels
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text = element_text(size = 16),   # Resize axis text
    strip.text = element_text(size = 16),  # Facet labels size
    panel.spacing = unit(1, "lines"),      # Adjust spacing between facets
    legend.text = element_text(size = 16),       # Increase legend text size
    legend.key.size = unit(2, "lines"),        # Increase legend key size
    axis.title = element_text(size = 18),        # Increase axis labels font size
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5) # Larger and centered title
  ) 

#p3

# Save bulk terms plot, renaming with each permutation
#ggsave("p3_u1_rough.jpg", p3, width = 16, height = 8, dpi = 300)



# Fourth plot! Dynamic momentum terms 
# Combine limits for the three variables across all datasets
dyn_limits <- range(
  data_subset$`dQ/dt.(m3/s2)`, 
  data_subset$`dQV/dx.(m3/s2)`, 
  na.rm = TRUE)

# Alternatively, restrict limits (these values just apply to our dataset)
dyn_limits <- range(-0.4, 0.4) # channel shape x flood intensities and ND batches (also skewed flood intensities)
dyn_limits <- range(-0.4, 0.2) # channel shape x hydrograph shape batch
dyn_limits <- range(-0.095, 0.01) # channel shape x Q estimation batch 
dyn_limits <- range(-0.3, 0.3) # channel shape x backwater batches


# Plot on the same y-axis
p4 <- ggplot(data_subset, aes(x = DateTime)) +
  geom_line(aes(y = `dQ/dt.(m3/s2)`, color = "dQ/dt.(m3/s2)")) + 
  geom_line(aes(y = `dQV/dx.(m3/s2)`, color = "dQV/dx.(m3/s2)")) + 
  # Select one facet_grid line based on your subset
  #facet_grid(FlowCondition ~ RoughnessValue, scales = "free_y") +
  #facet_grid(FlowCondition ~ BedSlopeValue, scales = "free_y") +
  #facet_grid(BackwaterCondition ~ BedSlopeValue, scales = "free_y") +        # bed x bw
  #facet_grid(BackwaterCondition ~ RoughnessValue, scales = "free_y") +       # rough x bw
  #facet_grid(RoughnessValue ~ BedSlopeValue, scales = "free_y") +        # rough x bed
  facet_grid(FlowCondition ~ ChannelShape, scales = "free_y") +        # flow condition x channel shape
  #facet_grid(BackwaterCondition ~ ChannelShape, scales = "free_y") +        # bw x channel shape 
  scale_y_continuous(name = "Term (m3/s2)", 
                     limits = dyn_limits,
                     labels = comma) +
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "8 days") +  
  # Select one title line based on your subset
  #labs(title = "Dynamic Terms by Roughness Conditions, Flood Intensities",  #u1
  #labs(title = "Dynamic Terms by Bed Slope, Flood Intensities",          #u1
  #labs(title = "Dynamic Terms by Channel Shape, Flood Intensities",          #u1
  #labs(title = "Dynamic Terms by Roughness Conditions, Skewed Flood Intensities",  #u2
  #labs(title = "Dynamic Terms by Bed Slope, Skewed Flood Intensities",          #u2
  #labs(title = "Dynamic Terms by Channel Shape, Skewed Flood Intensities",          #u2
  #labs(title = "Dynamic Terms by Roughness Conditions, Normal Depth comparison",  #u3
  #labs(title = "Dynamic Terms by Bed Slope, Normal Depth comparison",          #u3
  #labs(title = "Dynamic Terms by Channel Shape, Normal Depth comparison",          #u3
  #labs(title = "Dynamic Terms by Roughness, Backwater Conditions",          #u4
  #labs(title = "Dynamic Terms by Bed Slope, Backwater Conditions",          #u4
  #labs(title = "Dynamic Terms by Channel Shape, Backwater Conditions",          #u4
  #labs(title = "Dynamic Terms by Bed Slope & Roughness (Normal Depth)",   #rough x bed 
  #labs(title = "Dynamic Terms by Roughness, Hydrograph Shapes",          #u5
  #labs(title = "Dynamic Terms by Bed Slope, Hydrograph Shapes",          #u5
  #labs(title = "Dynamic Terms by Channel Shape, Hydrograph Shapes",          #u5
  labs(title = "Dynamic Terms by Streamflow Estimation Method",          #u6, u8
  #labs(title = "Dynamic Terms by Hydraulic Modeling Method",   #u7
       x = "DateTime") +
  scale_color_manual(values = c("dQ/dt.(m3/s2)" = "yellow2", "dQV/dx.(m3/s2)" = "dodgerblue")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16),  # Rotate and resize x-axis labels
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text = element_text(size = 16),   # Resize axis text
    strip.text = element_text(size = 16),  # Facet labels size
    panel.spacing = unit(1, "lines"),      # Adjust spacing between facets
    legend.text = element_text(size = 16),       # Increase legend text size
    legend.key.size = unit(2, "lines"),        # Increase legend key size
    axis.title = element_text(size = 18),        # Increase axis labels font size
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) # Larger and centered title)

#p4

# Save dynamic terms plot, renaming with each permutation
#ggsave("p4_u1_rough.jpg", p4, width = 16, height = 8, dpi = 300)



# Fifth plot! Absolute value of kinematic, diffusive, and dynamic terms (5 lines, logscale)
# Calculate absolute values and create a new combined dataframe for plotting
data_subset_abs <- data_subset %>%
  mutate(
    abs_gASf = abs(`gASf`),
    abs_gAS0 = abs(`gAS0`),
    abs_diff = abs(`diff.(m3/s2)`),
    abs_loc = abs(`dQ/dt.(m3/s2)`),
    abs_conv = abs(`dQV/dx.(m3/s2)`)  
  )

# Define consistent y-axis limits
y_limits <- c(0.0001, 100) # These values are specific to our data, you may need to adjust

# Plot absolute values on log scale
p5 <- ggplot(data_subset_abs, aes(x = DateTime)) +
  geom_line(aes(y = `abs_gASf`, color = "gASf")) + 
  geom_line(aes(y = `abs_gAS0`, color = "gAS0")) + 
  geom_line(aes(y = `abs_diff`, color = "gA(dy/dx)")) +
  geom_line(aes(y = `abs_loc`, color = "dQ/dt")) + 
  geom_line(aes(y = `abs_conv`, color = "dQV/dx")) + 
  # Select one facet_grid line based on your subset
  #facet_grid(FlowCondition ~ RoughnessValue, scales = "free_y") +
  #facet_grid(FlowCondition ~ BedSlopeValue, scales = "free_y") +
  #facet_grid(BackwaterCondition ~ BedSlopeValue, scales = "free_y") +        # bed x bw
  #facet_grid(BackwaterCondition ~ RoughnessValue, scales = "free_y") +       # rough x bw
  #facet_grid(RoughnessValue ~ BedSlopeValue, scales = "free_y") +        # rough x bed
  facet_grid(FlowCondition ~ ChannelShape, scales = "free_y") +        # flow condition x channel shape
  #facet_grid(BackwaterCondition ~ ChannelShape, scales = "free_y") +        # bw x channel shape 
  scale_y_continuous(name = "Absolute Term (m3/s2)", 
                     trans = 'log10', 
                     limits = y_limits,  # Apply consistent y-axis limits
                     labels = scales::comma) +
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "8 days") +  
  # Select one title line based on your subset
  #labs(title = "Absolute Kinematic, Diffusive & Dynamic Terms by Roughness Conditions, Flood Intensities",   #u1
  #labs(title = "Absolute Kinematic, Diffusive & Dynamic Terms by Bed Slope, Flood Intensities",   #u1
  #labs(title = "Absolute Kinematic, Diffusive & Dynamic Terms by Channel Shape, Flood Intensities",   #u1
  #labs(title = "Absolute Kinematic, Diffusive & Dynamic Terms by Roughness Conditions, Skewed Flood Intensities",   #u2
  #labs(title = "Absolute Kinematic, Diffusive & Dynamic Terms by Bed Slope, Skewed Flood Intensities",   #u2
  #labs(title = "Absolute Kinematic, Diffusive & Dynamic Terms by Channel Shape, Skewed Flood Intensities",   #u2
  #labs(title = "Absolute Kinematic, Diffusive & Dynamic Terms by Roughness Conditions, Normal Depth comparison",   #u3
  #labs(title = "Absolute Kinematic, Diffusive & Dynamic Terms by Bed Slope, Normal Depth comparison",   #u3
  #labs(title = "Absolute Kinematic, Diffusive & Dynamic Terms by Channel Shape, Normal Depth comparison",   #u3
  #labs(title = "Absolute Kinematic, Diffusive & Dynamic Terms by Roughness, Backwater Conditions",   #u4
  #labs(title = "Absolute Kinematic, Diffusive & Dynamic Terms by Bed Slope, Backwater Conditons",   #u4
  #labs(title = "Absolute Kinematic, Diffusive & Dynamic Terms by Channel Shape, Backwater Conditions",   #u4
  #labs(title = "Absolute Kinematic, Diffusive & Dynamic Terms by Bed Slope & Roughness (Normal Depth)",   #rough x bed
  #labs(title = "Absolute Kinematic, Diffusive & Dynamic Terms by Roughness, Hydrograph Shapes",   #u5
  #labs(title = "Absolute Kinematic, Diffusive & Dynamic Terms by Bed Slope, Hydrograph Shapes",   #u5
  #labs(title = "Absolute Kinematic, Diffusive & Dynamic Terms by Channel Shape, Hydrograph Shapes",   #u5
  labs(title = "Absolute Kinematic, Diffusive & Dynamic Terms by Streamflow Estimation Method",   #u6, #u8
  #labs(title = "Absolute Kinematic, Diffusive & Dynamic Terms by Hydraulic Modeling Method",   #u7
       x = "DateTime") +
  scale_color_manual(values = c("gASf" = "orange", 
                                "gAS0" = "black", 
                                "gA(dy/dx)" = "gray",
                                "dQ/dt" = "yellow2", 
                                "dQV/dx" = "dodgerblue")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16),  
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text = element_text(size = 16),  
        strip.text = element_text(size = 16), 
        panel.spacing = unit(1, "lines"),      
        legend.text = element_text(size = 16),       
        legend.key.size = unit(2, "lines"),       
        axis.title = element_text(size = 18),       
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) 

#p5

# Save all terms plot, renaming with each permutation
#ggsave("p5_u1_rough.jpg", p5, width = 16, height = 8, dpi = 300)



# Sixth plot! Normalized momentum term time series
# Identify max and min values for each term, grouped by scenario
data_subset <- data_subset %>%
  group_by(dataset) %>%
  mutate(
    max_gAS0_time = DateTime[which.max(Norm_gAS0)],  # Get time of max gAS0
    min_gAS0_time = DateTime[which.min(Norm_gAS0)],  # Get time of min gAS0
    max_gASf_time = DateTime[which.max(Norm_gASf)],
    min_gASf_time = DateTime[which.min(Norm_gASf)],
    max_diff_time = DateTime[which.max(`Norm_diff.(m3/s2)`)],
    min_diff_time = DateTime[which.min(`Norm_diff.(m3/s2)`)],
    max_conv_time = DateTime[which.max(`Norm_dQV/dx.(m3/s2)`)],
    min_conv_time = DateTime[which.min(`Norm_dQV/dx.(m3/s2)`)]
  ) %>%
  ungroup()

# Preprocess data to ensure maximum values occur in the middle
# Mutate columns to inverse values if the first value is greater than 40 (this nearly always works for our data, may need to adjust)
data_subset <- data_subset %>%
  group_by(dataset) %>%
  mutate(
    Norm_gASf = if (first(Norm_gASf) > 40) 100 - Norm_gASf else Norm_gASf,
    Norm_gAS0 = if (first(Norm_gAS0) > 40) 100 - Norm_gAS0 else Norm_gAS0,
    Norm_diff = if (first(`Norm_diff.(m3/s2)`) > 40) 100 - `Norm_diff.(m3/s2)` else `Norm_diff.(m3/s2)`,
    Norm_conv = if (first(`Norm_dQV/dx.(m3/s2)`) > 40) 100 - `Norm_dQV/dx.(m3/s2)` else `Norm_dQV/dx.(m3/s2)`
  ) %>%
  ungroup()

# Check out data
summary(data_subset)
colnames(data_subset)

# Plot the normalized momentum term time series
p6 <- ggplot(data_subset, aes(x = DateTime)) +   
  geom_line(aes(y = Norm_gASf, color = "gASf")) + 
  geom_line(aes(y = Norm_gAS0, color = "gAS0")) + 
  geom_line(aes(y = Norm_diff, color = "gA(dy/dx)")) +
  geom_line(aes(y = Norm_conv, color = "dQV/dx")) + 
  # Select one facet_grid line based on your subset
  #facet_grid(FlowCondition ~ RoughnessValue, scales = "free_y") +
  #facet_grid(FlowCondition ~ BedSlopeValue, scales = "free_y") +
  #facet_grid(BackwaterCondition ~ BedSlopeValue, scales = "free_y") +        # bed x bw
  #facet_grid(BackwaterCondition ~ RoughnessValue, scales = "free_y") +       # rough x bw
  #facet_grid(RoughnessValue ~ BedSlopeValue, scales = "free_y") +        # rough x bed
  facet_grid(FlowCondition ~ ChannelShape, scales = "free_y") +        # flow condition x channel shape
  #facet_grid(BackwaterCondition ~ ChannelShape, scales = "free_y") +        # bw x channel shape 
  scale_y_continuous(name = "Normalized Term (m3/s2)", 
                     labels = scales::comma) +
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "8 days") +  
  labs(
    # Select one title line based on your subset
    #title = "Normalized Momentum Terms by Roughness Conditions, Flood Intensities",  #u1
    #title = "Normalized Momentum Terms by Roughness Conditions, Skewed Flood Intensities",  #u2
    #title = "Normalized Momentum Terms by Roughness Conditions, Normal Depth Comparison",  #u3
    #title = "Normalized Momentum Terms by Roughness, Backwater Conditions",  #u4
    #title = "Normalized Momentum Terms by Bed Slope, Flood Intensities",  #u1
    #title = "Normalized Momentum Terms by Bed Slope, Skewed Flood Intensities",  #u2
    #title = "Normalized Momentum Terms by Bed Slope, Normal Depth Comparison",  #u3
    #title = "Normalized Momentum Terms by Bed Slope, Backwater Conditions",  #u4
    #title = "Normalized Momentum Terms by Bed Slope & Roughness (Normal Depth)",   #rough x bed 
    #title = "Normalized Momentum Terms by Roughness, Hydrograph Shapes",  #u5
    #title = "Normalized Momentum Terms by Bed Slope, Hydrograph Shapes",  #u5
    #title = "Normalized Momentum Terms by Streamflow Estimation Method",  #u6
    #title = "Normalized Momentum Terms by Channel Shape, Flood Intensities",  #u1
    #title = "Normalized Momentum Terms by Channel Shape, Skewed Flood Intensities",  #u2
    #title = "Normalized Momentum Terms by Channel Shape, Normal Depth Comparison",  #u3
    #title = "Normalized Momentum Terms by Channel Shape, Backwater Conditions",  #u4
    #title = "Normalized Momentum Terms by Channel Shape, Hydrograph Shapes",  #u5
    title = "Normalized Momentum Terms by Streamflow Estimation Method",  #u8
    #title = "Normalized Momentum Terms by Hydraulic Modeling Method",   #u7
       x = "DateTime") +
  scale_color_manual(values = c("gASf" = "orange", 
                                "gAS0" = "black", 
                                "gA(dy/dx)" = "gray",
                                "dQV/dx" = "dodgerblue")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16), 
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text = element_text(size = 16),  
        strip.text = element_text(size = 16),  
        panel.spacing = unit(1, "lines"),      
        legend.text = element_text(size = 16),      
        legend.key.size = unit(2, "lines"),        
        axis.title = element_text(size = 18),        
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) 

#p6


# Save normalized all terms plot, renaming with each permutation
#ggsave("p6_u1_rough.jpg", p6, width = 16, height = 8, dpi = 300)




# IF ANY TIME SERIES PROBLEMATIC (Upside-down), fill in their info here, then go back up and re-run p6 plotting:
# Apply flipping ONLY to the identified problematic case(s)
# u1_roughness
#data_subset <- data_subset %>%
#  mutate(
#    Norm_conv = ifelse(RoughnessCondition == "n = 0.1" & FlowCondition == "Low Intensity", 100 - Norm_conv, Norm_conv)
#  )
# u3_chanshape
#data_subset <- data_subset %>%
#  mutate(
#    `Norm_Sw.(m/m)` = ifelse(ChannelShape == "Rectangular/Contracting" & FlowCondition == "Normal Depth", 100 - `Norm_Sw.(m/m)`, `Norm_Sw.(m/m)`)
#  )


# Seventh plot! Normalized flow variable time series
p7<- ggplot(data_subset, aes(x = DateTime)) +
  #geom_line(aes(y = `Norm_Sw.(m/m)`, color = "FSS")) + 
  geom_line(aes(y = `Norm_Avg..Vel..(m/s)`, color = "Velocity")) + 
  geom_line(aes(y = `Norm_Tot.Q.(cms)`, color = "Flow")) +
  geom_line(aes(y = `Norm_W.S..El..(m)`, color = "Stage")) + 
  # Select one facet_grid line based on your subset
  #facet_grid(FlowCondition ~ RoughnessValue, scales = "free_y") +
  #facet_grid(FlowCondition ~ BedSlopeValue, scales = "free_y") +
  #facet_grid(BackwaterCondition ~ BedSlopeValue, scales = "free_y") +        # bed x bw
  #facet_grid(BackwaterCondition ~ RoughnessValue, scales = "free_y") +       # rough x bw
  #facet_grid(RoughnessValue ~ BedSlopeValue, scales = "free_y") +        # rough x bed
  facet_grid(FlowCondition ~ ChannelShape, scales = "free_y") +        # flow condition x channel shape
  #facet_grid(BackwaterCondition ~ ChannelShape, scales = "free_y") +        # bw x channel shape 
  scale_y_continuous(name = "Normalized Term (m3/s2)", 
                     labels = scales::comma) +
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "8 days") +  
  labs(
    # Select one title line based on your subset
    #title = "Normalized Flow Variables by Roughness Conditions, Flood Intensities", #u1
    #title = "Normalized Flow Variables by Roughness Conditions, Skewed Flood Intensities", #u2
    #title = "Normalized Flow Variables by Roughness Conditions, Normal Depth Comparison", #u3
    #title = "Normalized Flow Variables by Roughness, Backwater Conditions", #u4
    #title = "Normalized Flow Variables by Bed Slope, Flood Intensities", #u1
    #title = "Normalized Flow Variables by Bed Slope, Skewed Flood Intensities", #u2
    #title = "Normalized Flow Variables by Bed Slope, Normal Depth Comparison", #u3
    #title = "Normalized Flow Variables by Bed Slope, Backwater Conditions", #u4
    #title = "Normalized Flow Variables by Bed Slope & Roughness (Normal Depth)",   #rough x bed 
    #title = "Normalized Flow Variables by Roughness, Hydrograph Shapes", #u5
    #title = "Normalized Flow Variables by Bed Slope, Hydrograph Shapes", #u5
    #title = "Normalized Flow Variables by Streamflow Estimation Method", #u6
    #title = "Normalized Flow Variables by Channel Shape, Flood Intensities", #u1
    #title = "Normalized Flow Variables by Channel Shape, Skewed Flood Intensities", #u2
    #title = "Normalized Flow Variables by Channel Shape, Normal Depth Comparison", #u3
    #title = "Normalized Flow Variables by Channel Shape, Backwater Conditions", #u4
    #title = "Normalized Flow Variables by Channel Shape, Hydrograph Shapes", #u5
    title = "Normalized Flow Variables by Streamflow Estimation Method",   #u8
    #title = "Normalized Flow Variables by Hydraulic Modeling Method",   #u7
       x = "DateTime") +
  scale_color_manual(values = c("FSS" = "springgreen4", 
                                "Velocity" = "red", 
                                "Flow" = "black",
                                "Stage" = "blue")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16),  
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text = element_text(size = 16),  
        strip.text = element_text(size = 16), 
        panel.spacing = unit(1, "lines"),      
        legend.text = element_text(size = 16),       
        legend.key.size = unit(2, "lines"),       
        axis.title = element_text(size = 18),       
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) 

#p7

# Save normalized flow variables plot, renaming with each permutation
#ggsave("p7_u1_rough.jpg", p7, width = 16, height = 8, dpi = 300)



# Ensure Norm_gAS0 is numeric
data_subset$Norm_gAS0 <- as.numeric(data_subset$Norm_gAS0)
# Check all cols
str(data_subset)

# Eighth plot! Normalized momentum term loops
p8 <- ggplot(data_subset, aes(x = Norm_gAS0)) +
  geom_point(aes(y = Norm_gASf, color = "gASf"), size = 0.3) +  # Adjust point size
  geom_point(aes(y = Norm_gAS0, color = "gAS0"), size = 0.3) + 
  geom_point(aes(y = Norm_diff, color = "gA(dy/dx)"), size = 0.3) +  # comment out for NA
  geom_point(aes(y = Norm_conv, color = "dQV/dx"), size = 0.3) +     # comment out for NA
  #geom_point(size = 0.3) +  # Adjust point size in the plot
  # Select one facet_grid line based on your subset
  #facet_grid(FlowCondition ~ RoughnessValue, scales = "free_y") +
  #facet_grid(FlowCondition ~ BedSlopeValue, scales = "free_y") +
  #facet_grid(BackwaterCondition ~ BedSlopeValue, scales = "free_y") +        # bed x bw
  #facet_grid(BackwaterCondition ~ RoughnessValue, scales = "free_y") +       # rough x bw
  #facet_grid(RoughnessValue ~ BedSlopeValue, scales = "free_y") +        # rough x bed
  facet_grid(FlowCondition ~ ChannelShape, scales = "free_y") +        # flow condition x channel shape
  #facet_grid(BackwaterCondition ~ ChannelShape, scales = "free_y") +        # bw x channel shape 
  scale_y_continuous(name = "Normalized Momentum Term", 
                     labels = scales::comma) +
  labs(
    # Select one title line based on your subset
    #title = "Normalized Momentum Term Loops by Roughness Conditions, Flood Intensities",  # u1
    #title = "Normalized Momentum Term Loops by Roughness Conditions, Skewed Flood Intensities",  # u2
    #title = "Normalized Momentum Term Loops by Roughness Conditions, Normal Depth Comparison",  # u3
    #title = "Normalized Momentum Term Loops by Roughness, Backwater Conditions",  # u4
    #title = "Normalized Momentum Term Loops by Bed Slope, Flood Intensities",  # u1
    #title = "Normalized Momentum Term Loops by Bed Slope, Skewed Flood Intensities",  # u2
    #title = "Normalized Momentum Term Loops by Bed Slope, Normal Depth Comparison",  # u3
    #title = "Normalized Momentum Term Loops by Bed Slope, Backwater Conditions",  # u4
    #title = "Normalized Momentum Term Loops by Bed Slope & Roughness (Normal Depth)",   #rough x bed 
    #title = "Normalized Momentum Term Loops by Roughness, Hydrograph Shapes",  # u5
    #title = "Normalized Momentum Term Loops by Bed Slope, Hydrograph Shapes",  # u5
    #title = "Normalized Momentum Term Loops by Streamflow Estimation Method",  # u6
    #title = "Normalized Momentum Term Loops by Channel Shape, Flood Intensities",  # u1
    #title = "Normalized Momentum Term Loops by Channel Shape, Skewed Flood Intensities",  # u2
    #title = "Normalized Momentum Term Loops by Channel Shape, Normal Depth Comparison",  # u3
    #title = "Normalized Momentum Term Loops by Channel Shape, Backwater Conditions",  # u4
    #title = "Normalized Momentum Term Loops by Channel Shape, Hydrograph Shapes",  # u5
    #title = "Normalized Momentum Term Loops by Hydraulic Modeling Method",   #u7
    title = "Normalized Momentum Term Loops by Streamflow Estimation Method",   #u8
       x = "Normalized gAS0") +
  scale_color_manual(values = c("gASf" = "orange", 
                                "gAS0" = "black", 
                                "gA(dy/dx)" = "gray",
                                "dQV/dx" = "dodgerblue")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16),  # Rotate and resize x-axis labels
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text = element_text(size = 16),   # Resize axis text
    strip.text = element_text(size = 16),  # Facet labels size
    panel.spacing = unit(1, "lines"),      # Adjust spacing between facets
    legend.text = element_text(size = 16),       # Increase legend text size
    legend.key.size = unit(12, "points"),        # Increase legend key size
    axis.title = element_text(size = 18),        # Increase axis labels font size
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5) # Larger and centered title
  ) +
  guides(
    color = guide_legend(
      override.aes = list(size = 3)  # Increase legend icon size (points)
    ))

#p8

# Save normalized terms loop plot, renaming with each permutation
#ggsave("p8_u1_rough.jpg", p8, width = 16, height = 8, dpi = 300)



# Normalized flow variable loops 
p9 <- ggplot(data_subset, aes(y = `Norm_W.S..El..(m)`)) +
  geom_point(aes(x = `Norm_Sw.(m/m)`, color = "FSS"), size = 0.3) + 
  geom_point(aes(x = `Norm_Avg..Vel..(m/s)`, color = "Velocity"), size = 0.3) + 
  geom_point(aes(x = `Norm_Tot.Q.(cms)`, color = "Flow"), size = 0.3) +
  geom_point(aes(x = `Norm_W.S..El..(m)`, color = "Stage"), size = 0.3) + 
  # Select one facet_grid line based on your subset
  #facet_grid(FlowCondition ~ RoughnessValue, scales = "free_y") +
  #facet_grid(FlowCondition ~ BedSlopeValue, scales = "free_y") +
  #facet_grid(BackwaterCondition ~ BedSlopeValue, scales = "free_y") +        # bed x bw
  #facet_grid(BackwaterCondition ~ RoughnessValue, scales = "free_y") +       # rough x bw
  #facet_grid(RoughnessValue ~ BedSlopeValue, scales = "free_y") +        # rough x bed
  facet_grid(FlowCondition ~ ChannelShape, scales = "free_y") +        # flow condition x channel shape
  #facet_grid(BackwaterCondition ~ ChannelShape, scales = "free_y") +        # bw x channel shape 
  scale_x_continuous(name = "Normalized Flow Variable", 
                     labels = scales::comma) +
  labs(
    # Select one title line based on your subset
    #title = "Normalized Flow Variable Loops by Roughness Conditions, Flood Intensities",  # u1
    #title = "Normalized Flow Variable Loops by Roughness Conditions, Skewed Flood Intensities",  # u2
    #title = "Normalized Flow Variable Loops by Roughness Conditions, Normal Depth Comparison",  # u3
    #title = "Normalized Flow Variable Loops by Roughness, Backwater Conditions",  # u4
    #title = "Normalized Flow Variable Loops by Bed Slope, Flood Intensities",  # u1
    #title = "Normalized Flow Variable Loops by Bed Slope, Skewed Flood Intensities",  # u2
    #title = "Normalized Flow Variable Loops by Bed Slope, Normal Depth Comparison",  # u3
    #title = "Normalized Flow Variable Loops by Bed Slope, Backwater Conditions",  # u4
    #title = "Normalized Flow Variable Loops by Bed Slope & Roughness (Base Flood)",   #rough x bed 
    #title = "Normalized Flow Variable Loops by Roughness, Hydrograph Shapes",  # u5
    #title = "Normalized Flow Variable Loops by Bed Slope, Hydrograph Shapes",  # u5
    #title = "Normalized Flow Variable Loops by Streamflow Estimation Method",  # u6
    #title = "Normalized Flow Variable Loops by Channel Shape, Flood Intensities",  # u1
    #title = "Normalized Flow Variable Loops by Channel Shape, Skewed Flood Intensities",  # u2
    #title = "Normalized Flow Variable Loops by Channel Shape, Normal Depth Comparison",  # u3
    #title = "Normalized Flow Variable Loops by Channel Shape, Backwater Conditions",  # u4
    #title = "Normalized Flow Variable Loops by Channel Shape, Hydrograph Shapes",  # u5
    #title = "Normalized Flow Variable Loops by Hydraulic Modeling Method",   #u7
    title = "Normalized Flow Variable Loops by Streamflow Estimation Method",   #u8
       y = "Normalized Stage") +
  scale_color_manual(values = c("FSS" = "springgreen4", 
                                "Velocity" = "red", 
                                "Flow" = "black",
                                "Stage" = "blue")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16),  # Rotate and resize x-axis labels
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text = element_text(size = 16),   # Resize axis text
    strip.text = element_text(size = 16),  # Facet labels size
    panel.spacing = unit(1, "lines"),      # Adjust spacing between facets
    legend.text = element_text(size = 16),       # Increase legend text size
    legend.key.size = unit(12, "points"),        # Increase legend key size
    axis.title = element_text(size = 18),        # Increase axis labels font size
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5) # Larger and centered title
  ) +
  guides(
    color = guide_legend(
      override.aes = list(size = 3)  # Increase legend icon size (points)
    ))

#p9

# Save normalized flow variables loop plot, renaming with each permutation
#ggsave("p9_u1_rough.jpg", p9, width = 16, height = 8, dpi = 300)



# Find summary gradient and cloud plot scripts in "RASController_summary.R"