# 3/29/25

# Emma House

# New script for processing and plotting SUMMARY PLOTS - STEP 3
# Plots for multiple plans from different batch runs together


# Install any required packages
#install.packages("remotes")
#remotes::install_version("shiny", version = "1.8.1.1")
#install.packages("promises")
#install.packages("ggnewscale")
#install.packages("ggplot2")

# Load required packages 
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)
library(patchwork)
library(stringr)
library(scales)
library(sensitivity)
library(fastDummies)
library(randomForest)
library(broom)
library(car)
library(ggnewscale)


# The beginning ~400 lines of this code are from "RASController_wrangling.R" 

#### TO START: READ in pre-processed data  ####
#  (from "RASController_processing.R")

# Specify the directory containing the .RData files, list all files
data_directory <- "C:/Users/ehouse/Downloads/RAS_Controller_T2"
rdata_files <- list.files(data_directory, pattern = "\\.RData$", full.names = TRUE)

# Read all .RData files into a list, name each element based on file names
data_lists <- lapply(rdata_files, readRDS)
names(data_lists) <- gsub("\\.RData$", "", basename(rdata_files))

# If it's giving an issue or FYI, read one and check it out
#load("C:/Users/ehouse/Downloads/RAS_Controller/compound_new.RData")


##### Define the look-up dictionaries for `u` and `g` indices####
# ADD MORE AS NEEDED!
# QUESTION -- later, do we use all of these? Streamline as needed...
u_description <- list(
  "u01" = "Base Flood",
  "u02" = "Normal Depth",
  "u03" = "Normal Depth",
  "u06" = "Intense Flood",
  "u10" = "Low Intensity",
  "u04" = "Skewed Base",
  "u05" = "Skewed Intense",
  "u21" = "Skewed Low",
  "u27" = "Multi-Pulse",
  "u28" = "Long Multi-Pulse",
  "u22" = "0.5x BW",
  "u29" = "0.75x BW",
  "u31" = "1.25x BW",
  "u25" = "Minor Flood",
  "u26" = "Skewed Minor",
  "u23" = "Normal Depth",
  "u24" = "Normal Depth",
  "u33" = "HQRC",
  "u34" = "IVRC",
  "u35" = "Johns",
  "u36" = "Fenton"
)

qest_descrip <- list(
  "u01" = "Base Flood",
  "u02" = "Normal Depth",
  "u03" = "Normal Depth",
  "u06" = "Base Flood",
  "u10" = "Base Flood",
  "u04" = "Base Flood",
  "u05" = "Base Flood",
  "u21" = "Base Flood",
  "u27" = "Base Flood",
  "u28" = "Base Flood",
  "u22" = "Base Flood",
  "u29" = "Base Flood",
  "u31" = "Base Flood",
  "u25" = "Base Flood",
  "u26" = "Base Flood",
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
  "g04" = "Rectangular/Prismatic",
  "g10" = "Rectangular/Prismatic",
  "g09" = "Rectangular/Prismatic", 
  "g02" = "Rectangular/Prismatic",
  "g05" = "Rectangular/Prismatic",
  "g15" = "Rectangular/Prismatic",
  "g03" = "Rectangular/Prismatic",
  "g06" = "Rectangular/Prismatic",
  "g07" = "Rectangular/Prismatic",
  "g08" = "Rectangular/Prismatic",
  "g11" = "Rectangular/Prismatic",
  "g12" = "Rectangular/Prismatic",
  "g14" = "Rectangular/Prismatic",
  "g16" = "Rectangular/Prismatic",
  "g17" = "Rectangular/Prismatic"
)

u_shapedescrip <- list(
  "u01" = "Base",
  "u06" = "Base",
  "u10" = "Base",
  "u04" = "Skewed",
  "u05" = "Skewed",
  "u21" = "Skewed",
  "u25" = "Base",
  "u26" = "Skewed",
  "u02" = "Base",
  "u03" = "Base",
  "u27" = "Multi-Pulse",
  "u28" = "Long Multi-Pulse",
  "u22" = "Base",
  "u29" = "Base",
  "u31" = "Base",
  "u23" = "Base",
  "u24" = "Base",
  "u33" = "Base",
  "u34" = "Base",
  "u35" = "Base",
  "u36" = "Base"
)

u_intdescrip <- list(
  "u01" = "Base Flood",
  "u06" = "Intense Flood",
  "u10" = "Low Intensity",
  "u04" = "Base Flood",
  "u05" = "Intense Flood",
  "u21" = "Low Intensity",
  "u25" = "Minor Flood",
  "u26" = "Minor Flood",
  "u02" = "Base Flood",
  "u03" = "Base Flood",
  "u27" = "Base Flood",
  "u28" = "Base Flood",
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

u_intvalue <- list(
  "u01" = 3000,
  "u06" = 4500,
  "u10" = 2000,
  "u04" = 3000,
  "u05" = 4500,
  "u21" = 2000,
  "u25" = 1000,
  "u26" = 1000,
  "u02" = 3000,
  "u03" = 3000,
  "u27" = 3000,
  "u28" = 3000,
  "u22" = 3000,
  "u29" = 3000,
  "u31" = 3000,
  "u23" = 3000,
  "u24" = 3000,
  "u33" = 3000,
  "u34" = 3000,
  "u35" = 3000,
  "u36" = 3000
)


u_bwdescrip <- list(
  "u01" = "Base Flood",
  "u02" = "Normal Depth",   # Idk if ND are going to be plotted vs. backwater but just in case...
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

u_bwvalue <- list(
  "u01" = 1,
  "u02" = 0,   # Idk if ND are going to be plotted vs. backwater but just in case...
  "u03" = 0,
  "u06" = 1,
  "u10" = 1,
  "u04" = 1,
  "u05" = 1,
  "u21" = 1,
  "u27" = 1,
  "u28" = 1,
  "u22" = 0.5,
  "u29" = 0.75,
  "u31" = 1.25,
  "u25" = 1,
  "u26" = 1,
  "u23" = 0,
  "u24" = 0,
  "u33" = 1,
  "u34" = 1,
  "u35" = 1,
  "u36" = 1
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


### Run these PREPROCESSING STEPS ####

# Reset if needed
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
    u_method <- qest_descrip[[u_index]]
    g_label <- g_description[[g_index]]
    g_shape <- g_shapedescrip[[g_index]]
    u_shape <- u_shapedescrip[[u_index]]
    u_int <- u_intdescrip[[u_index]]
    u_bw <- u_bwdescrip[[u_index]]
    n_descrip <- n_description[[g_index]]
    s_descrip <- s_description[[g_index]]
    
    # Fetch values based on indices
    roughnessval <- g_roughnessvalue[[g_index]]
    bedslopeval <- g_bedslopevalue[[g_index]]
    intval <- u_intvalue[[u_index]]
    bwval <- u_bwvalue[[u_index]]
    
    # Add the data frame to the combined list with a renamed and annotated entry
    combined_data_list[[new_name]] <- lst[[df_name]]
    
    # Attach the `u` and `g` labels as attributes for later reference
    attr(combined_data_list[[new_name]], "flow_condition") <- u_label
    attr(combined_data_list[[new_name]], "geometry_condition") <- g_label
    
    # Attach the 'u' and `g` descriptions as attributes for later reference
    attr(combined_data_list[[new_name]], "streamflow_method") <- u_method
    attr(combined_data_list[[new_name]], "roughness_value") <- roughnessval
    attr(combined_data_list[[new_name]], "bedslope_value") <- bedslopeval
    attr(combined_data_list[[new_name]], "int_value") <- intval
    attr(combined_data_list[[new_name]], "bw_value") <- bwval
    attr(combined_data_list[[new_name]], "channel_shape") <- g_shape
    attr(combined_data_list[[new_name]], "floodwave_shape") <- u_shape
    attr(combined_data_list[[new_name]], "floodwave_intensity") <- u_int
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
  cat("\nQ Estimation Method:", attr(combined_data_list[[df_name]], "streamflow_method"))
  cat("\nBed Slope Value:", attr(combined_data_list[[df_name]], "bedslope_value"))
  cat("\nFlood Intensity Value:", attr(combined_data_list[[df_name]], "int_value"))
  cat("\nBackwater Condition Value:", attr(combined_data_list[[df_name]], "bw_value"))
  cat("\nRoughness Value:", attr(combined_data_list[[df_name]], "roughness_value"))
  cat("\nChannel Shape:", attr(combined_data_list[[df_name]], "channel_shape"))
  cat("\nFloodwave Shape:", attr(combined_data_list[[df_name]], "floodwave_shape"))
  cat("\nFloodwave Intensity:", attr(combined_data_list[[df_name]], "floodwave_intensity"))
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
      QEstMethod = attr(df, "streamflow_method"),
      BedSlopeValue = attr(df, "bedslope_value"),
      RoughnessValue = attr(df, "roughness_value"),
      IntensityValue = attr(df, "int_value"),
      BackwaterValue = attr(df, "bw_value"),
      ChannelShape = attr(df, "channel_shape"),
      FloodwaveShape = attr(df, "floodwave_shape"),
      FloodwaveIntensity = attr(df, "floodwave_intensity"),
      BackwaterCondition = attr(df, "backwater_condition"), 
      RoughnessCondition = attr(df, "roughness_condition"),
      BedSlopeCondition = attr(df, "bed_slope_condition"),
      dataset = df_name  
    )
})



# ADD TO THIS AS YOU GO - important indeed
# Define order factors to control facet layout by flow and geometry conditions
combined_data <- combined_data %>%
  mutate(
    FlowCondition = factor(FlowCondition, levels = c("Minor Flood", "Low Intensity", "Base Flood", "Intense Flood", 
                                                     "Skewed Minor", "Skewed Low", "Skewed Base", "Skewed Intense", 
                                                     "Normal Depth", "Multi-Pulse", "Long Multi-Pulse",
                                                     "HQRC", "IVRC", "Johns", "Fenton", "0.5x BW", "0.75x BW", "1.25x BW")),
    BackwaterCondition = factor(BackwaterCondition, levels = c("0.5x BW", "0.75x BW", "Base Flood", "1.25x BW")),
    ChannelShape = factor(ChannelShape, levels = c("Rectangular/Prismatic", "Rectangular/Expanding", "Rectangular/Contracting", "Compound/Prismatic"))
  )
# Bed and roughness below maybe redundant, they self order since they're values?
#BedCondition = factor(BedCondition, levels = c("Mild (S0=0.0001)", "Base (S0=0.00027)", "Steep (S0=0.0005)", "Steepest (S0=0.001)")),
#RoughnessCondition = factor(RoughnessCondition, levels = c("Smooth (n=0.02)", "Base (n=0.03)", "Rough (n=0.1)", "Rougher (n=0.2)"))

# A check
print(unique(combined_data$FlowCondition))
# Does it work? Maybe Minor Flood slightly messed up...
print(unique(combined_data$BackwaterCondition))
print(unique(combined_data$dataset))
print(unique(combined_data$ChannelShape))



# Before subsetting, or after if you want, you can plot the stage-discharge loop cloud!
# Actually this is a separate thing just be careful

# TEMP RESET ASSIGNMENT if desired
data_subset <- combined_data # temporary for making some summary plots...

# Remove most of the points because there are a lot..... (just for this new plot...)
data_subset <- data_subset %>%
  filter(format(DateTime, "%M") %in% c("00", "30"))

# Stage-Discharge Rating Curves differentiating rising and falling limbs (if you want, I've opted out for now..)
# Identify the rising and falling stages for each dataset
combined_data <- combined_data %>%
  group_by(dataset) %>%
  mutate(
    peak_index = which.max(`W.S..El..(m)`),  # Index of the peak W.S..El..(m)
    stage = ifelse(row_number() <= peak_index, "Rising Stage", "Falling Stage")  # Classify stages
  ) %>%
  ungroup()

# Calculate limits for the subset
ws_el_limits <- range(data_subset$`W.S..El..(m)`, na.rm = TRUE)
tot_q_limits <- range(data_subset$`Tot.Q.(cms)`, na.rm = TRUE)
scaling_factor <- diff(ws_el_limits) / diff(tot_q_limits)

# PLOT NEW PLOT
cloud <- data_subset %>%
  ggplot(aes(x = `Tot.Q.(cms)`, y = `W.S..El..(m)`, color = stage)) +
  geom_point(size = 0.3, alpha = 0.1) +  # Adjust point size in the plot
  #geom_point(color = "black", alpha = 0.1, size = 0.5) +  # Semi-transparent black points
  scale_x_continuous(
    labels = scales::comma,
    breaks = scales::breaks_pretty(n = 5)  # Dynamically adjust x-axis breaks
  ) +
  scale_y_continuous(labels = scales::comma, limits = ws_el_limits) +  # Format y-axis labels
  scale_color_manual(
    values = c("Rising Stage" = "red", "Falling Stage" = "blue")
  ) +
  labs(
    title = "Stage-Discharge Rating Curves",   
    x = "Discharge (cms)",
    y = "Water Surface Elevation (m)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16),  
    legend.position = "bottom",
    #legend.position = "none",
    legend.title = element_blank(),
    axis.text = element_text(size = 16),   
    strip.text = element_text(size = 16),  
    panel.spacing = unit(1, "lines"),      
    legend.text = element_text(size = 16),       
    legend.key.size = unit(12, "points"),        
    axis.title = element_text(size = 18),        
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5) 
  ) +
  guides(
    color = guide_legend(
      override.aes = list(size = 3)  ))

cloud

# Save cloud plot
ggsave("all_cloud_colors.jpg", cloud, width = 10, height = 8, dpi = 300)



#### TO USE: CUSTOMIZE by selecting from below  ####


# Select subsection of data to plot... 
# u1: four flood intensities
data_subset <- combined_data %>%
  filter(FlowCondition %in% c("Minor Flood", "Base Flood", "Low Intensity", "Intense Flood"))
  #filter(FlowCondition %in% c("Base Flood", "Low Intensity", "Intense Flood"))  # Only three
  #filter(FlowCondition %in% c("Minor Flood", "Intense Flood"))  # Only two
# u2: four skewed flood intensities
data_subset <- combined_data %>%
  filter(FlowCondition %in% c("Skewed Minor", "Skewed Base", "Skewed Low", "Skewed Intense"))  
# u3: normal depth condition
data_subset <- combined_data %>%
  filter(FlowCondition %in% c("Base Flood", "Normal Depth"))
# u4: backwater condition
data_subset <- combined_data %>%
  #filter(BackwaterCondition %in% c("0.5x BW", "0.75x BW", "Base Flood", "1.25x BW"))  # Not explicitly (includes others with Base Flood)
  filter(str_detect(dataset, "u01|u22|u29|u31"))   # Explicitly
# Matrix of roughness x bed geometries: do them one at a time, base flood first
data_subset <- combined_data %>%
  filter(str_detect(dataset, "u01"))
#  filter(str_detect(dataset, "u02|u03|u23|u24")) # For normal depth
# u5: floodwave shapes
data_subset <- combined_data %>%
  filter(FlowCondition %in% c("Base Flood", "Skewed Base", "Multi-Pulse", "Long Multi-Pulse"))
# Q estimation methods
data_subset <- combined_data %>%
  filter(str_detect(dataset, "u33|u34|u35|u36")) 


# Check
print(unique(data_subset$dataset))
print(unique(data_subset$FlowCondition))
print(unique(data_subset$ChannelShape))


# Subset again just for geometries
# Roughness
data_subset <- data_subset %>%
  filter(str_detect(dataset, "g10|g01|g04|g09"))
# OR Bed slope
data_subset <- data_subset %>%
  filter(str_detect(dataset, "g05|g01|g02|g15")) 
# LATER: Add channel shape, too
# Both roughness and bed slope
data_subset <- data_subset %>%
  filter(str_detect(dataset, "g10|g01|g04|g09|g05|g02|g15|g08|g14|g11|g06|g03|g16|g07|g12|g17"))
# If you just want base geometry to compare flow files
data_subset <- data_subset %>%
  filter(str_detect(dataset, "g01"))
# Channel XS Shapes
data_subset <- data_subset %>%
  filter(str_detect(dataset, "g01|g18|g19|g23")) 


# OR if you just want to remove the Q-estimation scenarios (for statistics & cloud plots) 
data_subset <- combined_data %>%
  filter(!str_detect(dataset, "u33|u34|u35|u36")) 
# May also want to remove the compound channel
data_subset <- data_subset %>%
  filter(!str_detect(dataset, "g23")) 


# OR subset for our new summary plot (the one that used to have arrows) all together
# IntensityValue, BackwaterValue, FloodwaveShape, QEstMethod, 
data_subset_g <- combined_data %>%
  filter(str_detect(dataset, "g01|g05|g02|g15|g10|g04|g09|g18|g19|g23")) %>%   # Roughness, backwater, and channel shape
  filter(str_detect(dataset, "u01"))  
data_subset_u <- combined_data %>%
  filter(str_detect(dataset, "u06|u10|u25|u22|u29|u31|u04|u27|u28|u33|u34")) %>%  # IntensityValue, BackwaterValue, FloodwaveShape, QEstMethod 
  filter(str_detect(dataset, "g01"))  
# Combine 
data_subset <- rbind(data_subset_g, data_subset_u) # shouldn't have any duplicates because didn't include "u01" in data_subset_u...


# TEMP RESET ASSIGNMENT if desired
data_subset <- combined_data # temporary for making some summary plots...


# Filter the data to remove rows with noise via Profile values in the specified ranges
# Also solves the issue with crazy large values at end of sim if haven't fixed that
data_subset <- data_subset %>%
  filter(!(Profile >= 1 & Profile <= 150) & !(Profile >= 34550 & Profile <= 36001))


# Check out the data
tapply(data_subset$`W.S..El..(m)`, data_subset$dataset, summary)  # Stage
tapply(data_subset$`Tot.Q.(cms)`, data_subset$dataset, summary)  # Flow


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



# Calculate the Froude number: Fr = v/sqrt(g*y)
data_subset$Fr <- data_subset$`Avg..Vel..(m/s)` / sqrt(9.81 * data_subset$`Hyd..Depth.(m)`)
print(summary(data_subset$Fr))

# Normalize this column (function defined in RASController_processing.R)
data_subset$Norm_Fr <- rescale_column(data_subset$Fr)
print(summary(data_subset$Norm_Fr)) # check that it normalized 0-100 properly


# Absolute value of kinematic, diffusive, and dynamic terms (5 lines, logscale) (now 6 lines... doing bulk kinematic too)
# Calculate absolute values and create a new combined dataframe for plotting
data_subset_abs <- data_subset %>%
  mutate(
    abs_kin = abs(`kin.(m3/s2)`),
    abs_gASf = abs(`gASf`),
    abs_gAS0 = abs(`gAS0`),
    abs_diff = abs(`diff.(m3/s2)`),
    abs_loc = abs(`dQ/dt.(m3/s2)`),
    abs_conv = abs(`dQV/dx.(m3/s2)`)  
  )


# Normalized momentum term time series
# Identify max and min values for each term, grouped by scenario
data_subset_abs <- data_subset_abs %>%
  group_by(dataset) %>%
  mutate(
    max_gAS0_time = DateTime[which.max(Norm_gAS0)],  # Get time of max gAS0
    min_gAS0_time = DateTime[which.min(Norm_gAS0)],  # Get time of min gAS0
    max_gASf_time = DateTime[which.max(Norm_gASf)],
    min_gASf_time = DateTime[which.min(Norm_gASf)],
    #max_kin_time = DateTime[which.max(Norm_Kin)],  # Didn't normalize kin in previous script but we don't need it anyways
    #min_kin_time = DateTime[which.min(Norm_Kin)],  # Didn't normalize kin in previous script but we don't need it anyways
    max_diff_time = DateTime[which.max(`Norm_diff.(m3/s2)`)],
    min_diff_time = DateTime[which.min(`Norm_diff.(m3/s2)`)],
    max_conv_time = DateTime[which.max(`Norm_dQV/dx.(m3/s2)`)],
    min_conv_time = DateTime[which.min(`Norm_dQV/dx.(m3/s2)`)]
  ) %>%
  ungroup()


# Preprocess data to ensure maximum values occur in the middle
# Mutate columns to inverse values if the first value is greater than 40
data_subset_abs <- data_subset_abs %>%
  group_by(dataset) %>%
  mutate(
    #Norm_Kin = if (first(Norm_Kin) > 40) 100 - Norm_Kin else Norm_Kin,
    Norm_gASf = if (first(Norm_gASf) > 40) 100 - Norm_gASf else Norm_gASf,
    Norm_gAS0 = if (first(Norm_gAS0) > 40) 100 - Norm_gAS0 else Norm_gAS0,
    Norm_diff = if (first(`Norm_diff.(m3/s2)`) > 40) 100 - `Norm_diff.(m3/s2)` else `Norm_diff.(m3/s2)`,
    Norm_conv = if (first(`Norm_dQV/dx.(m3/s2)`) > 40) 100 - `Norm_dQV/dx.(m3/s2)` else `Norm_dQV/dx.(m3/s2)`
  ) %>%
  ungroup()


# Check out data
summary(data_subset_abs)
colnames(data_subset_abs)


# Save the data & read it later to save time and your computer's memory
#saveRDS(data_subset_abs, file = "combined_data_abs.RData")   # WHOLE DATASET (ik it says subset but it's the whole thing) 5/19/25
saveRDS(data_subset_abs, file = "combined_data_abs2.RData")   # WHOLE DATASET FIXED S0 ISSUE 8/27/25
# Read it in if you need it
combined_data_abs <- readRDS("combined_data_abs.RData")        # TIME AND LIFE SAVER - full dataset



#### TO PLOT GRADIENTS: CUSTOMIZE based on your selections  ####

# Create groups... varied bed slope for a given flow scenario, for example. This may have to be manual assignment of variables but we got this. 
# Color them differently for certain groups we are interested in. 
# If you just want base flow to compare geometry files
data_subset <- combined_data_abs %>%
  filter(str_detect(dataset, "u01"))
# u1: four flood intensities
data_subset <- combined_data_abs %>%
  filter(FlowCondition %in% c("Minor Flood", "Base Flood", "Low Intensity", "Intense Flood"))
# u4: backwater condition
data_subset <- combined_data_abs %>%
  filter(str_detect(dataset, "u01|u22|u29|u31"))  
# u5: floodwave shapes
data_subset <- combined_data_abs %>%
  filter(FlowCondition %in% c("Base Flood", "Skewed Base", "Multi-Pulse", "Long Multi-Pulse"))
# Q estimation methods
data_subset <- combined_data_abs %>%
  filter(str_detect(dataset, "u33|u34|u35|u36")) 

# Subset again just for geometries
#If you just want base geometry to compare flow files
data_subset <- data_subset %>%
  filter(str_detect(dataset, "g01"))
# Roughness
data_subset <- data_subset %>%
  filter(str_detect(dataset, "g10|g01|g04|g09"))
# Bed slope
data_subset <- data_subset %>%
  filter(str_detect(dataset, "g05|g01|g02|g15")) 
# Channel XS Shapes
data_subset <- data_subset %>%
  filter(str_detect(dataset, "g01|g18|g19|g23")) 

# Check
print(unique(data_subset$dataset))


# Create gradient plots for each group we are studying... 

# Create gradient difference plots! Colors for each parameter and term (?)
# Define colors  (pick the one you subset for)
colors <- c("Mild (S0=0.0001)" = "black", "Base (S0=0.00027)" = "blue", "Steep (S0=0.0005)" = "orange", "Steepest (S0=0.001)" = "red")
colors <- c("Smooth (n=0.02)" = "black", "Base (n=0.03)" = "blue", "Rough (n=0.1)" = "orange", "Rougher (n=0.2)" = "red")
colors <- c("0.5x BW" = "red", "0.75x BW" = "orange", "Base Flood" = "blue", "1.25x BW" = "black")
colors <- c("Minor Flood" = "red", "Low Intensity" = "orange", "Base Flood" = "blue", "Intense Flood" = "black")
colors <- c("Compound/Prismatic" = "red", "Rectangular/Contracting" = "orange", "Rectangular/Prismatic" = "blue", "Rectangular/Expanding" = "black")
colors <- c("Base" = "blue", "Skewed" = "black", "Multi-Pulse" = "orange", "Long Multi-Pulse" = "red") # NEW


# Also specify the factor levels for the ordering of your legend
data_subset$BedSlopeCondition <- factor(data_subset$BedSlopeCondition, levels = c("Mild (S0=0.0001)", "Base (S0=0.00027)", "Steep (S0=0.0005)", "Steepest (S0=0.001)"))
data_subset$RoughnessCondition <- factor(data_subset$RoughnessCondition, levels = c("Smooth (n=0.02)", "Base (n=0.03)", "Rough (n=0.1)", "Rougher (n=0.2)"))
data_subset$BackwaterCondition <- factor(data_subset$BackwaterCondition, levels = c("0.5x BW", "0.75x BW", "Base Flood", "1.25x BW"))
data_subset$FlowCondition <- factor(data_subset$FlowCondition, levels = c("Minor Flood", "Low Intensity", "Base Flood", "Intense Flood"))
data_subset$ChannelShape <- factor(data_subset$ChannelShape, levels = c("Rectangular/Prismatic", "Rectangular/Expanding", "Rectangular/Contracting", "Compound/Prismatic"))
data_subset$FlowCondition <- factor(data_subset$FlowCondition, levels = c("Base", "Skewed", "Multi-Pulse", "Long Multi-Pulse"))


# Convert DateTime to proper format
data_subset$DateTime <- as.POSIXct(data_subset$DateTime, format="%Y-%m-%d %H:%M:%S")

# For all panels below, set "color =" to match your desired grouping
# Panel a: W.S..El..(m) as time series
g1 <- ggplot(data_subset, aes(x = DateTime, y = `W.S..El..(m)`, color = BedSlopeCondition)) +  # or BedSlopeCondition, RoughnessCondition, FlowCondition, ChannelShape
  geom_line() +
  scale_x_datetime(labels = date_format("%m/%d")) +  
  scale_color_manual(values = colors) +
  labs(y = "Stage (m)", x = "", title = "a)") +
  theme_minimal() + theme(legend.position = "none")

# Panel b: Tot.Q.(cms) vs. W.S..El..(m)
g2 <- ggplot(data_subset, aes(x = `Tot.Q.(cms)`, y = `W.S..El..(m)`, color = BedSlopeCondition)) +
  geom_point(size = 0.2) +
  #geom_point(size = 0.2, alpha = 0.03) +  # If you need to add some transparency
  scale_color_manual(values = colors) +
  labs(y = "Stage (m)", x = "Streamflow (cms)", title = "b)") +
  theme_minimal() + theme(legend.position = "none")

# Panel c: dQ/dt.(m3/s3) as time series
g3 <- ggplot(data_subset, aes(x = DateTime, y = `dQ/dt.(m3/s2)`, color = BedSlopeCondition)) +
  geom_line() +
  #geom_line(alpha = 0.5) +    # If you need to add some transparency
  scale_x_datetime(labels = date_format("%m/%d")) +  
  scale_color_manual(values = colors) +
  labs(y = "dQ/dt (m3/s2)", x = "", title = "c)") +
  theme_minimal() + theme(legend.position = "none")

# Panel d: dQV/dx.(m3/s2) as time series
g4 <- ggplot(data_subset, aes(x = DateTime, y = `dQV/dx.(m3/s2)`, color = BedSlopeCondition)) +
  geom_line() +
  scale_color_manual(values = colors) +
  labs(y = "dQV/dx (m3/s2)", x = "", title = "d)") +
  theme_minimal() + theme(legend.position = "none")

# Panel e: diff.(m3/s2) as time series
g5 <- ggplot(data_subset, aes(x = DateTime, y = `diff.(m3/s2)`, color = BedSlopeCondition)) +
  geom_line() +
  scale_x_datetime(labels = date_format("%m/%d")) +  
  scale_y_continuous(limits = c(-2, 15)) +    # Specific to compound channel
  scale_color_manual(values = colors) +
  labs(y = "gAdy/dx (m3/s2)", x = "", title = "e)") +
  theme_minimal() + theme(legend.position = "none")

# Panel f: gAS0 and gASf as time series, gASf as dotted line
g6 <- ggplot(data_subset) +
  geom_line(aes(x = DateTime, y = gAS0, color = BedSlopeCondition)) +
  geom_line(aes(x = DateTime, y = gASf, color = BedSlopeCondition), linetype = "dashed") +
  scale_x_datetime(labels = date_format("%m/%d")) +  
  scale_color_manual(values = colors) +
  labs(y = "gAS0 & gASf (m3/s2)", x = "", title = "f)") +
  theme_minimal() #+ theme(legend.position = "bottom")

# Arrange panels in a grid
final_plot <- g1 + g2 + g3 + g4 + g5 + g6 + plot_layout(ncol = 2)

# Plot to console
final_plot

# Save as .jpg, changing name each time
ggsave("bedslope_gradients_base.jpg", final_plot, width = 10, height = 8, dpi = 300)



# Gradients of Fr 
# For the two panels below, set "color =" to match your desired grouping

# Fr Time series
ggplot(data_subset, aes(x = DateTime, y = Fr, color = BedSlopeCondition)) +  # or BedSlopeCondition, RoughnessCondition, FloodwaveIntensity, FloodwaveShape, ChannelShape, BackwaterCondition
  geom_line() +
  scale_x_datetime(labels = date_format("%m/%d")) +  
  scale_color_manual(values = colors) +
  labs(y = "Froude Number", x = "", title = "Froude Number Time Series for Channel Shapes") +  # Change title as needed
  theme_minimal() + theme(legend.position = "none")

# Stage-Fr loops
ggplot(data_subset, aes(x = Fr, y = `W.S..El..(m)`, color = BedSlopeCondition)) +   # or BedSlopeCondition, RoughnessCondition, FloodwaveIntensity, FloodwaveShape, ChannelShape, BackwaterCondition
  geom_point(size = 0.2) +
  #geom_point(size = 0.2, alpha = 0.03) +  # If you need to add some transparency
  scale_color_manual(values = colors) +
  labs(y = "Stage (m)", x = "Froude Number", title = "Stage-Froude Number Loops for Channel Shapes") +  # Change title as needed
  theme_minimal() + theme(legend.position = "none")


# Fr Plot for manuscript:
# Consistent minimalist theme
base_theme <- theme_minimal() +
  theme(
    #plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.title = element_text(size= 12),
    legend.text = element_text(size = 12)
  )
# Plot a: Froude Number Time Series — keeps the line legend
plot_a <- ggplot(data_subset, aes(x = DateTime, y = Fr, color = ChannelShape)) +  # or BedSlopeCondition, RoughnessCondition, FloodwaveIntensity, FloodwaveShape, ChannelShape, BackwaterCondition
  geom_line() +
  scale_x_datetime(labels = date_format("%m/%d")) +
  scale_color_manual(values = colors, name = "Channel Shape") +  # or Bed Slope, Manning's Roughness, Flood Wave Intensity, Flood Wave Shape, Backwater, Channel Shape, etc.
  labs(
    y = "Froude Number",
    x = "",
    #title = "a) Froude Number Time Series"
  ) +
  base_theme +
  theme(legend.position = "right")
# Plot b: Stage-Froude Number Loops — suppress the legend
plot_b <- ggplot(data_subset, aes(x = Fr, y = `W.S..El..(m)`, color = ChannelShape)) +
  geom_point(size = 0.2, show.legend = FALSE) +  # Hide point legend explicitly
  scale_color_manual(values = colors) +
  labs(
    y = "Stage (m)",
    x = "Froude Number",
    #title = "b) Stage-Froude Number Loops"
  ) +
  base_theme +
  theme(legend.position = "none")
# Combine plots: side-by-side, collect shared legend
combined_plot <- plot_a + plot_b +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "right")
# Save as figure: 2 panels at 5x5 inches + ~2" for legend = 12x5
ggsave("combined_froude_chanshape.png", combined_plot, width = 12, height = 4, dpi = 300)
# Display in viewer
print(combined_plot)



# Exploration of Fr timing amongst the other variables
# subset for just base scenario
data_subset_base <- data_subset_abs %>%   # OR data_subset_abs
  filter(str_detect(dataset, "u01_g01")) 

# check? Whats going on
print(summary(data_subset_abs$Norm_Fr))
print(summary(data_subset_base$Norm_Fr))

# Renormalize if you have to
data_subset_base$Norm_Fr <- rescale_column(data_subset_base$Fr)
print(summary(data_subset_base$Norm_Fr)) # check that it normalized properly


# Plot the normalized momentum term time series (with Fr now)
ggplot(data_subset_base, aes(x = DateTime)) +   
  geom_line(aes(y = Norm_Fr, color = "Fr")) + 
  geom_line(aes(y = Norm_gASf, color = "gASf")) + 
  geom_line(aes(y = Norm_gAS0, color = "gAS0")) + 
  #geom_line(aes(y = Norm_diff, color = "gA(dy/dx)")) +  # Plots if you use data_subset_abs, but then Fr is messed up
  #geom_line(aes(y = Norm_conv, color = "dQV/dx")) +   # Plots if you use data_subset_abs, but then Fr is messed up
  scale_y_continuous(name = "Normalized Value", 
                     labels = scales::comma) +
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "8 days") +  
  labs(
    title = "Normalized Momentum Terms and Fr",  
    x = "DateTime") +
  scale_color_manual(values = c("Fr" = "purple",
                                "gASf" = "orange", 
                                "gAS0" = "black", 
                                "gA(dy/dx)" = "gray",
                                "dQV/dx" = "dodgerblue")) +
  theme_minimal() +
  theme(legend.position = "bottom") 


# Normalized flow variable time series (with Fr now)
ggplot(data_subset_base, aes(x = DateTime)) +
  geom_line(aes(y = Norm_Fr, color = "Fr")) + 
  geom_line(aes(y = `Norm_Sw.(m/m)`, color = "FSS")) + 
  geom_line(aes(y = `Norm_Avg..Vel..(m/s)`, color = "Velocity")) + 
  geom_line(aes(y = `Norm_Tot.Q.(cms)`, color = "Flow")) +
  geom_line(aes(y = `Norm_W.S..El..(m)`, color = "Stage")) + 
  scale_y_continuous(name = "Normalized Term (m3/s2)", 
                     labels = scales::comma) +
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "8 days") +  
  labs(
    title = "Normalized Flow Variables and Fr",   
    x = "DateTime") +
  scale_color_manual(values = c("Fr" = "purple",
                                "FSS" = "springgreen4", 
                                "Velocity" = "red", 
                                "Flow" = "black",
                                "Stage" = "blue")) +
  theme_minimal() +
  theme(legend.position = "bottom") #+
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



#### SUMMARIZE #####
# Extract maximum absolute value of gAS0 for all dataframes and bind into a single dataframe
# Also for the rest of the momentum terms to compare
# THIS WORKS ONLY IF YOU DONT STILL HAVE ANY FLIPPED!!
summarized_gAS0 <- combined_data_abs %>%  # {combined_data_abs, data_subset_abs}
  group_by(dataset) %>%
  reframe(
    # Response variables (WSE)
    Max_WSE = max(`W.S..El..(m)`, na.rm = TRUE),   
    WSE_DateTime = first(DateTime[which.max(`W.S..El..(m)`)]),  
    #Fr_Max = max(`Fr`, na.rm = TRUE),                            # Include if you want (must run above code)
    #Fr_Max_DateTime = first(DateTime[which.max(`Fr`)]),          # Include if you want (must run above code)

    #Flow Variables
    Max_Q = max(`Tot.Q.(cms)`, na.rm = TRUE),   
    Q_DateTime = first(DateTime[which.max(`Tot.Q.(cms)`)]),  
    Max_FSS = max(`Sw.(m/m)`, na.rm = TRUE),   
    FSS_DateTime = first(DateTime[which.max(`Sw.(m/m)`)]),  
    Max_V = max(`Avg..Vel..(m/s)`, na.rm = TRUE),   
    V_DateTime = first(DateTime[which.max(`Avg..Vel..(m/s)`)]), 
  
    # Momentum Terms
    #Max_Norm_Kin = max(Norm_Kin, na.rm = TRUE),                  # Include if you want (must calculate first)
    #Max_Kin = first(Kin[which.max(Norm_Kin)]),                   # Include if you want (must calculate first)
    #Kin_DateTime = first(DateTime[which.max(Norm_Kin)]),         # Include if you want (must calculate first)
    Max_Norm_gAS0 = max(Norm_gAS0, na.rm = TRUE),  
    Max_gAS0 = first(gAS0[which.max(Norm_gAS0)]),   
    gAS0_DateTime = first(DateTime[which.max(Norm_gAS0)]),  
    Max_gASf = first(gASf[which.max(Norm_gASf)]),   
    gASf_DateTime = first(DateTime[which.max(Norm_gASf)]), 
    Max_gAdydx = first(`diff.(m3/s2)`[which.max(`Norm_diff.(m3/s2)`)]),   
    gAdydx_DateTime = first(DateTime[which.max(`Norm_diff.(m3/s2)`)]),
    Max_dQVdx = first(`dQV/dx.(m3/s2)`[which.max(`dQV/dx.(m3/s2)`)]),   
    dQVdx_DateTime = first(DateTime[which.max(`dQV/dx.(m3/s2)`)]),
    BedSlopeCondition = first(BedSlopeCondition),  
    RoughnessCondition = first(RoughnessCondition),
    QEstMethod = first(QEstMethod),
    ChannelShape = first(ChannelShape),
    FloodwaveShape = first(FloodwaveShape),
    FloodwaveIntensity = first(FloodwaveIntensity),
    BackwaterCondition = first(BackwaterCondition),
    GeometryCondition = first(GeometryCondition),  
    BedSlopeValue = first(BedSlopeValue),
    RoughnessValue = first(RoughnessValue),
    IntensityValue = first(IntensityValue),
    BackwaterValue = first(BackwaterValue),
    FlowCondition = first(FlowCondition)
  )



# Calculate the time difference from peak WSE for each plan 
summarized_gAS0 <- summarized_gAS0 %>%
  mutate(
    #Time_Kin = as.numeric(difftime(WSE_DateTime, Kin_DateTime, units = "hours")),
    Time_gAS0 = as.numeric(difftime(WSE_DateTime, gAS0_DateTime, units = "hours")),
    Time_gASf = as.numeric(difftime(WSE_DateTime, gASf_DateTime, units = "hours")),
    Time_gAdydx = as.numeric(difftime(WSE_DateTime, gAdydx_DateTime, units = "hours")),
    Time_dQVdx = as.numeric(difftime(WSE_DateTime, dQVdx_DateTime, units = "hours")),
    Time_V = as.numeric(difftime(WSE_DateTime, V_DateTime, units = "hours")),
    Time_Q = as.numeric(difftime(WSE_DateTime, Q_DateTime, units = "hours")),
    Time_FSS = as.numeric(difftime(WSE_DateTime, FSS_DateTime, units = "hours")),
    Time_WSE = as.numeric(difftime(WSE_DateTime, WSE_DateTime, units = "hours")),
    #Time_Fr = as.numeric(difftime(WSE_DateTime, Fr_Max_DateTime, units = "hours"))
  )



#### TO PLOT THOSE NEW plots for hysteresis impacts from batch permutations ####

# Plot the time difference between V-WSE peak (calculated above) for a given batch
ggplot(summarized_gAS0, aes(x = Time_V, y = ChannelShape)) +   # Swap out color as needed # y = {BedSlopeValue, RoughnessValue, IntensityValue, BackwaterValue, FloodwaveShape, QEstMethod, ChannelShape}
  geom_line(aes(group = 1), color = "purple") +  # Connect points in order with red (decreasing hysteretic signal) or springgreen4 (increasing hysteretic signal) or purple (unsure) line
  geom_point(size = 3) + # , alpha = 0.7
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "Channel Shape vs. V-WSE Peak Phase Timing \nDifference (base other scenarios)",  # Bed Slope, Roughness, Backwater, Flood Intensity, Flood Wave Shape, Q-Estimation Method, Channel Shape...
    x = "Peak Phasing Time between V-WSE (hours)",
    y = "Channel Shape") + #,   # Bed Slope (m/m), Manning's Roughness, Backwater (x Base depth), Flood Intensity (cms), Flood Wave Shape, Q-Estimation Method, Channel Shape...
    #color = "Bed Slope Condition",  # Add legend title for clarity (Flow Condition, Roughness Condition, Bed Slope Condition, etc.)
    #fill = "Bed Slope Condition") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black")  # Add axis lines
  )


# Plot all batches in one plot... 
# First subset by those specific trials....above in second "OR"
# Then duplicate the base scenario row since you need it for all comparisons
#summarized_gAS0_filtered <- summarized_gAS0
# NEW: 5/17 just replaced summarized_gAS0 with summarized_gAS0_filtered below
base_row <- summarized_gAS0_filtered %>% filter(dataset == "u01_g01")  # Extract the row to duplicate
base_duplicated <- bind_rows(replicate(7, base_row, simplify = FALSE))  # Duplicate it 5 times
# Add the group column to label each copy 
base_duplicated <- base_duplicated %>%
  mutate(Group = c("BedSlopeValue", "RoughnessValue", "IntensityValue", "BackwaterValue", "FloodwaveShape", "QEstMethod", "ChannelShape"))
# Combine with original data
summarized_gAS0_filtered <- bind_rows(summarized_gAS0_filtered, base_duplicated)
summarized_gAS0_filtered <- summarized_gAS0_filtered %>%
  filter(!(dataset == "u01_g01" & is.na(Group)))  # Remove non-labeled base scenario, no longer needed
# Then fill in the group column for the rest
summarized_gAS0_filtered <- summarized_gAS0_filtered %>%
  mutate(Group = if_else(
    is.na(Group),
    case_when(
      dataset %in% c("u01_g02", "u01_g05", "u01_g15") ~ "BedSlopeValue",
      dataset %in% c("u06_g01", "u10_g01", "u25_g01") ~ "IntensityValue",
      dataset %in% c("u01_g04", "u01_g10", "u01_g09") ~ "RoughnessValue",
      dataset %in% c("u22_g01", "u29_g01", "u31_g01") ~ "BackwaterValue",
      dataset %in% c("u04_g01", "u27_g01", "u28_g01") ~ "FloodwaveShape",
      dataset %in% c("u33_g01", "u34_g01") ~ "QEstMethod",
      dataset %in% c("u01_g18", "u01_g19", "u01_g23") ~ "ChannelShape",
      TRUE ~ NA_character_
    ),
    Group
  ))


# NEW TO help plot lines:
# When duplicating u01_g01, do this:
u01_base <- summarized_gAS0_filtered %>% filter(dataset == "u01_g01") #, is.na(Group))

# Duplicate and assign Group with a unique dataset ID
bed_slope <- u01_base %>%
  mutate(Group = "BedSlopeValue", dataset = "u01_g01_BedSlope")
intensity <- u01_base %>%
  mutate(Group = "IntensityValue", dataset = "u01_g01_Intensity")
roughness <- u01_base %>%
  mutate(Group = "RoughnessValue", dataset = "u01_g01_roughness")
backwater <- u01_base %>%
  mutate(Group = "BackwaterValue", dataset = "u01_g01_backwater")
ushape <- u01_base %>%
  mutate(Group = "FloodwaveShape", dataset = "u01_g01_ushape")
qest <- u01_base %>%
  mutate(Group = "QEstMethod", dataset = "u01_g01_qest")
gshape <- u01_base %>%
  mutate(Group = "ChannelShape", dataset = "u01_g01_gshape")

# Then bind them together:
summarized_gAS0_filtered <- bind_rows(
  summarized_gAS0_filtered %>% filter(!(dataset == "u01_g01" & is.na(Group))),
  bed_slope, intensity, roughness, backwater, ushape, qest, gshape)

# Remove duplicates that popped up
summarized_gAS0_filtered <- summarized_gAS0_filtered %>%
  filter(dataset != "u01_g01") %>%    # Remove the original u01_g01 row
  distinct(dataset, .keep_all = TRUE)  # Keep only unique dataset values, retaining all other columns


# Create numeric columns for normalization
summarized_gAS0_filtered <- summarized_gAS0_filtered %>%
  mutate(
    BedSlopeValue_num = as.numeric(str_remove(BedSlopeValue, "S0 = ")),
    RoughnessValue_num = as.numeric(str_remove(RoughnessValue, "n = "))
  ) %>%
  mutate(
    FloodwaveValue = case_when(
      FloodwaveShape == "Skewed" ~ 0,
      FloodwaveShape == "Base Flood" ~ 0.25,
      FloodwaveShape == "Long Multi-Pulse" ~ 0.5,
      FloodwaveShape == "Multi-Pulse" ~ 0.75,
      TRUE ~ NA_real_),
    QEstValue = case_when(
      QEstMethod == "HQRC" ~ 0,
      QEstMethod == "IVRC" ~ 0.5,
      QEstMethod == "Base Flood" ~ 0.75,
      TRUE ~ NA_real_),
    ChannelShapeValue = case_when(
      ChannelShape == "Rectangular/Contracting" ~ 0,
      ChannelShape == "Rectangular/Prismatic" ~ 0.5,
      ChannelShape == "Rectangular/Expanding" ~ 1.0,
      #ChannelShape == "Compound/Prismatic" ~ 1.0,  # taking it out
      TRUE ~ NA_real_)
  )

# Normalize (0-1 with boundaries as boundaries)
summarized_gAS0_norm <- summarized_gAS0_filtered %>%
  group_by(Group) %>%
  mutate(
    NormValue = case_when(
      Group == "BedSlopeValue" ~ (BedSlopeValue_num - min(BedSlopeValue_num, na.rm = TRUE)) /
        (max(BedSlopeValue_num, na.rm = TRUE) - min(BedSlopeValue_num, na.rm = TRUE)),
      Group == "RoughnessValue" ~ (RoughnessValue_num - min(RoughnessValue_num, na.rm = TRUE)) /
        (max(RoughnessValue_num, na.rm = TRUE) - min(RoughnessValue_num, na.rm = TRUE)),
      Group == "IntensityValue" ~ (IntensityValue - min(IntensityValue, na.rm = TRUE)) /
        (max(IntensityValue, na.rm = TRUE) - min(IntensityValue, na.rm = TRUE)),
      Group == "BackwaterValue" ~ (BackwaterValue - min(BackwaterValue, na.rm = TRUE)) /
        (max(BackwaterValue, na.rm = TRUE) - min(BackwaterValue, na.rm = TRUE)),
      Group == "FloodwaveShape" ~ FloodwaveValue,
      Group == "QEstMethod" ~ QEstValue,
      Group == "ChannelShape" ~ ChannelShapeValue,
      TRUE ~ NA_real_)) %>%
  ungroup()

# OR Normalize (0-1 with upper boundary as boundary, lower boundary is 0) - only working with bed slope, roughness, and backwater now
# For working in Excel doing composite effect of variables
summarized_gAS0_norm <- summarized_gAS0_filtered %>%
  group_by(Group) %>%
  mutate(
    NormValue = case_when(
      Group == "BedSlopeValue" ~ (BedSlopeValue_num - 0) /
        (max(BedSlopeValue_num, na.rm = TRUE) - 0),
      Group == "RoughnessValue" ~ (RoughnessValue_num - 0) /
        (max(RoughnessValue_num, na.rm = TRUE) - 0),
      Group == "BackwaterValue" ~ (BackwaterValue - 0) /
        (max(BackwaterValue, na.rm = TRUE) - 0),
      TRUE ~ NA_real_)) %>%
  ungroup()

# Invert the ChannelShape and BackwaterValue for monotonically decreasing trends
summarized_gAS0_norm <- summarized_gAS0_norm %>%
  mutate(
    NormValue = case_when(
      Group == "BackwaterValue" ~ 1 - NormValue,
      Group == "ChannelShape" ~ 1 - NormValue,
      TRUE ~ NormValue  # keep existing values for other groups
    )
  )

# Ensure Time_V is numeric
summarized_gAS0_norm <- summarized_gAS0_norm %>%
  mutate(Time_V = as.numeric(Time_V)) %>% 
  arrange(Group, dataset, Time_V)  # and sort data by V

# Remove flood wave shape and Qest method since not plotting well
summarized_gAS0_fewer <- summarized_gAS0_norm %>%
  filter(!Group %in% c("QEstMethod", "FloodwaveShape"))

# Plot
ggplot(summarized_gAS0_fewer, aes(x = Time_V, y = NormValue, color = Group)) +
  geom_line(aes(group = interaction(Group)), linewidth = 1.25) +  # <-- This ensures lines connect
  geom_point(size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  #scale_x_continuous(limits = c(-5, 150)) +
  scale_x_continuous(limits = c(-5, 100)) +  # for "fewer"
  labs(
    title = "Normalized Parameter vs. V-WSE Peak Phase Timing Difference",
    x = "Peak Phasing Time between V-WSE (hours)",
    y = "Normalized Parameter",
    color = "Parameter"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )






#### TO PLOT SUMMARY SCATTER PLOTS: CUSTOMIZE based on your selections  ####

# Read in summarized_gAS0
#saveRDS(summarized_gAS0, file = "summarized_gAS0.RData")
write.csv(summarized_gAS0_norm, "summarized_gAS0_norm.csv")
#saveRDS(summarized_gAS0_filtered, file = "summarized_gAS0_filtered.RData")
summarized_gAS0 <- readRDS("summarized_gAS0_filtered.RData")        # TIME AND LIFE SAVER - full dataset minus Q-est methods and MP scenarios
summarized_gAS0 <- readRDS("summarized_gAS0.RData")        # TIME AND LIFE SAVER - full dataset


# Create cloud plot for each variable (summarized magnitude and timing values) 

# Create new columns for 'u' and 'g' groups
summarized_gAS0 <- summarized_gAS0 %>%
  mutate(
    u_group = sub("_.*", "", dataset),        # Extracts "uXX"
    g_group = sub(".*_", "", dataset)         # Extracts "gXX"
  )

# Create another dataset for all but without multipulse scenarios, Q-est methods, OR compound cross-section
# just rewriting the file because I can go back easily a few lines up...
summarized_gAS0_filtered <- summarized_gAS0 %>%
  filter(!str_detect(dataset, "u27|u28|u33|u34|u35|u36|g23"))  

# Create another dataset for backwater conditions
summarized_gAS0_filtered_bw <- summarized_gAS0_filtered %>%
  filter(str_detect(dataset, "u01|u22|u29|u31"))     # Filter for backwater scenarios & base

# Use the absolute value of Response variables
summarized_gAS0_filtered$Max_gAdydx <- abs(summarized_gAS0_filtered$Max_gAdydx)



# Remove outliers for the Intense flood
summarized_gAS0_filtered_int <- summarized_gAS0_filtered %>%
  group_by(FloodwaveIntensity) %>%
  filter(
    Time_V > quantile(Time_V, 0.25, na.rm = TRUE) - 10 * IQR(Time_V, na.rm = TRUE),
    Time_V < quantile(Time_V, 0.75, na.rm = TRUE) + 10 * IQR(Time_V, na.rm = TRUE)
  )

# Remove most groups to get just four points (FloodWaveIntensity Scenarios)
filtered_data <- summarized_gAS0_filtered_int %>%
  filter(str_detect(FloodwaveShape, "Base"))
# AND
filtered_data <- filtered_data %>%
  filter(str_detect(GeometryCondition, "Base")) 
# AND
filtered_data <- filtered_data %>%
  filter(str_detect(BackwaterCondition, "Base")) 

# Subsetting for Bed Slope varied, other base conditions
filtered_data <- summarized_gAS0_filtered %>%
  filter(str_detect(FloodwaveShape, "Base")) %>%
  filter(str_detect(FloodwaveIntensity, "Base")) %>%
  filter(str_detect(ChannelShape, "Rectangular/Prismatic")) %>%
  filter(str_detect(BackwaterCondition, "Base")) %>%
  filter(str_detect(RoughnessCondition, "Base"))  

# Subsetting for Roughness varied, other base conditions
filtered_data <- summarized_gAS0_filtered %>%
  filter(str_detect(FloodwaveShape, "Base")) %>%
  filter(str_detect(FloodwaveIntensity, "Base")) %>%
  filter(str_detect(ChannelShape, "Rectangular/Prismatic")) %>%
  filter(str_detect(BedSlopeCondition, "Base")) %>%   
  filter(str_detect(BackwaterCondition, "Base"))  

# Subsetting for Backwater varied, other base conditions
filtered_data <- summarized_gAS0_filtered_bw %>%
  filter(str_detect(FloodwaveShape, "Base")) %>%
  filter(str_detect(FloodwaveIntensity, "Base")) %>%
  filter(str_detect(ChannelShape, "Rectangular/Prismatic")) %>%
  filter(str_detect(BedSlopeCondition, "Base")) %>%  
  filter(str_detect(RoughnessCondition, "Base")) 



# PLOT Diffusive vs. V-WSE phasing scatter plot (with trendlines NEW JULY 2025)
ggplot(summarized_gAS0_filtered_int, aes(x = Time_V, y = Max_gAdydx)) +   # summarized_gAS0_filtered_int or summarized_gAS0_filtered_bw
  scale_x_continuous(
    limits = c(0, max(summarized_gAS0_filtered$Time_V, na.rm = TRUE)+5),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, max(summarized_gAS0_filtered$Max_gAdydx, na.rm = TRUE)+1),
    expand = c(0, 0)
  ) +
  # TRENDLINES FIRST
  # New: linear regression lines with no intercept
  geom_smooth(
    aes(color = BedSlopeCondition, fill = BedSlopeCondition),  # color  fill = {FloodwaveIntensity, RoughnessCondition, BedSlopeCondition, maybe BackwaterCondition}
    method = "lm",
    formula = y ~ x - 1,   # No intercept, line goes through (0,0)
    se = FALSE,
    linewidth = 1.25,   # made thicker
    alpha = 1
  ) +
  # Manual fill and color scale for parameter 
  scale_fill_manual(
    name = "Bed Slope Condition",    # name = {"Floodwave Intensity", "Roughness Condition", "Bed Slope Condition"}
    values = c(
      #"Intense Flood" = "black", "Base Flood" = "blue", "Low Intensity" = "orange", "Minor Flood" = "red"),
      #"Smooth (n=0.02)" = "black", "Base (n=0.03)" = "blue", "Rough (n=0.1)" = "orange", "Rougher (n=0.2)" = "red"),
      "Mild (S0=0.0001)" = "black", "Base (S0=0.00027)" = "blue", "Steep (S0=0.0005)" = "orange", "Steepest (S0=0.001)" = "red"),
      #"1.25x BW" = "black", "Base Flood" = "blue", "0.75x BW" = "orange", "0.5x BW" = "red"),  # fill = BackwaterCondition
    na.translate = FALSE
  ) +
  scale_color_manual(
    name = "Bed Slope Condition",    # name = {"Floodwave Intensity", "Roughness Condition", "Bed Slope Condition"}
    values = c(
      #"Intense Flood" = "black", "Base Flood" = "blue", "Low Intensity" = "orange", "Minor Flood" = "red"),
      #"Smooth (n=0.02)" = "black", "Base (n=0.03)" = "blue", "Rough (n=0.1)" = "orange", "Rougher (n=0.2)" = "red"),
      "Mild (S0=0.0001)" = "black", "Base (S0=0.00027)" = "blue", "Steep (S0=0.0005)" = "orange", "Steepest (S0=0.001)" = "red"),
      #"1.25x BW" = "black", "Base Flood" = "blue", "0.75x BW" = "orange", "0.5x BW" = "red"),  # fill = BackwaterCondition
    na.translate = FALSE#, guide = "none"  # hide redundant color legend 
  ) +
  # NEW fill scale (so the next fill/color mappings are distinct)
  new_scale_fill() +
  new_scale_color() +
  # POINTS: use color (not fill) so we can use ggplot default palette
  geom_point(
    aes(color = BedSlopeCondition), #shape = ShapeLabel, color = FlowCondition, fill = FlowCondition),  # color & fill = {FlowCondition, RoughnessCondition, BedSlopeCondition, (old)GeometryCondition}
    size = 2, stroke = 0.3, alpha = 0.8   
  ) +
  scale_color_manual(
    name = "Bed Slope Condition",    # name = {"Floodwave Intensity", "Roughness Condition", "Bed Slope Condition"}
    values = c(
    #"Intense Flood" = scales::alpha("black", 0.5), "Base Flood" = scales::alpha("blue", 0.5), "Low Intensity" = scales::alpha("orange", 0.5), "Minor Flood" = scales::alpha("red", 0.5)),
    #"Smooth (n=0.02)" = scales::alpha("black", 0.5), "Base (n=0.03)" = scales::alpha("blue", 0.5), "Rough (n=0.1)" = scales::alpha("orange", 0.5), "Rougher (n=0.2)" = scales::alpha("red", 0.5)),
    "Mild (S0=0.0001)" = scales::alpha("black", 0.5), "Base (S0=0.00027)" = scales::alpha("blue", 0.5), "Steep (S0=0.0005)" = scales::alpha("orange", 0.5), "Steepest (S0=0.001)" = scales::alpha("red", 0.5)),
    #"1.25x BW" = scales::alpha("black", 0.5), "Base Flood" = scales::alpha("blue", 0.5), "0.75x BW" = scales::alpha("orange", 0.5), "0.5x BW" = scales::alpha("red"), 0.5),  # fill = BackwaterCondition
    na.translate = FALSE#, guide = "none"  # hide redundant color legend 
  ) +
  # FOUR HIGHLIGHTED BASE CONDITION POINTS (filter appropriately above first..)
  geom_point(
    data = filtered_data,
    aes(x = Time_V, y = Max_gAdydx, color = BedSlopeCondition),
    shape = 21,              # Circle with fill and color (stroke)
    size = 6,                # Twice as large (main points are size = 3)
    stroke = 2,            # Thicker outline
    #alpha = 1,
    fill = "transparent",          # Transparent color to see underlying points
    #color = "black"          # Outline color
  ) +
  scale_color_manual(
    name = "Bed Slope Condition",    # name = {"Floodwave Intensity", "Roughness Condition", "Bed Slope Condition"}
    values = c(
      #"Intense Flood" = "black", "Base Flood" = "blue", "Low Intensity" = "orange", "Minor Flood" = "red"),
      #"Smooth (n=0.02)" = "black", "Base (n=0.03)" = "blue", "Rough (n=0.1)" = "orange", "Rougher (n=0.2)" = "red"),
      "Mild (S0=0.0001)" = "black", "Base (S0=0.00027)" = "blue", "Steep (S0=0.0005)" = "orange", "Steepest (S0=0.001)" = "red"),
      #"1.25x BW" = "black", "Base Flood" = "blue", "0.75x BW" = "orange", "0.5x BW" = "red"),  # fill = BackwaterCondition
    na.translate = FALSE#, guide = "none"  # hide redundant color legend 
  ) +
  # Labels
  labs(
    #title = "Kinematic Term: Max Value \nvs. V-WSE Timing Difference (All Scenarios)",
    #title = "Diffusive Term: Max Value \nvs. V-WSE Timing Difference (All Scenarios)",  # No title, actually
    #title = "Convective Acceleration Term: Max Value \nvs. V-WSE Timing Difference (All Scenarios)",
    #title = "Velocity: Max Value \nvs. V-WSE Timing Difference (All Scenarios)",
    #title = "Froude Number: Max Value \nvs. V-WSE Timing Difference (All Scenarios)",
    x = "Phasing Time between V and WSE peaks (hours)",
    y = "Diffusive Term Maximum Value"   # Change for each variable you're plotting {Velocity, Diffusive Term, Convective Acceleration Term, Froude Number}
  ) +
  # Theme
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),     # Axis labels: x and y titles
    axis.text = element_text(size = 14),      # Axis tick numbers
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )



# NEW Part TWO: Fr (for many : 1 grouping)  # x = {Time_V, Time_Fr}
ggplot(summarized_gAS0_filtered, aes(x = Time_V, y = Fr_Max, color = GeometryCondition)) +   # Swap out color as needed (FlowCondition, GeometryCondition, etc.)
  geom_point(size = 3, alpha = 0.7) +
  stat_ellipse(aes(fill = RoughnessCondition), geom = "polygon", alpha = 0.15, level = 0.95, color = NA) +  # Ellipses: fill = {BedSlopeCondition, RoughnessCondition, FloodwaveIntensity, BackwaterCondition
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  # Customize axes
  #scale_x_continuous(limits = c(-5, 1)) +    
  labs(
    title = "Froud Number: Maximum Value \nvs. V-WSE Timing Difference (All Scenarios)",
    #x = "Time Difference from WSE peak (hours)",
    x = "Phasing Time from V-WSE peaks (hours)",  # NEW
    y = "Maximum Value",
    color = "Geometry Condition",  # Add legend title for clarity (Flow Condition, Geometry Condition, etc.)
    fill = "Roughness Condition") +  # Ellipses: fill = {Bed Slope Condition, Roughness Condition, Flood Wave Intensity, Backwater Condition
  scale_color_discrete(na.translate = FALSE) +  # Removes NA from the legend
  scale_fill_manual(
    #values = c("Mild (S0=0.0001)" = "black", "Base (S0=0.00027)" = "blue", "Steep (S0=0.0005)" = "orange", "Steepest (S0=0.001)" = "red"),   # fill = BedSlopeCondition
    values = c("Smooth (n=0.02)" = "black", "Base (n=0.03)" = "blue", "Rough (n=0.1)" = "orange", "Rougher (n=0.2)" = "red"),  # fill = RoughnessCondition
    #values = c("Intense Flood" = "black", "Base Flood" = "blue", "Low Intensity" = "orange", "Minor Flood" = "red"),  # fill = FloodwaveIntensity
    #values = c("1.25x BW" = "black", "Base Flood" = "blue", "0.75x BW" = "orange", "0.5x BW" = "red"),  # fill = BackwaterCondition
    na.translate = FALSE) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black")  # Add axis lines
  )




#### STATISTICS COMPUTATION ####

# Linear Model (LM) and Analysis of Variance (ANOVA)

# Dependent variable options: Time_Q, Time_V, Time_FSS, Max_gAdydx, Max_dQVdx

# Independent variables: BedSlopeCondition, RoughnessCondition, FloodwaveShape (no MP!), FloodwaveIntensity, BackwaterCondition, ChannelShape

# Create another dataset for all but without multipulse scenarios and Q-est
summarized_gAS0_filtered <- summarized_gAS0 %>%
  filter(!str_detect(dataset, "u27|u28|u33|u34|u35|u36"))     

# Create another dataset for all but without multipulse scenarios and Q-est AND compound XS
summarized_gAS0_filtered <- summarized_gAS0 %>%
  filter(!str_detect(dataset, "u27|u28|u33|u34|u35|u36|g23"))  


# Set default values and prepare data
df_lm <- summarized_gAS0_filtered %>%
  mutate(
    BedSlopeCondition = ifelse(is.na(BedSlopeCondition), "Base (S0=0.00027)", BedSlopeCondition),
    RoughnessCondition = ifelse(is.na(RoughnessCondition), "Base (n=0.03)", RoughnessCondition),
    #QEstMethod = ifelse(is.na(QEstMethod), "Base Flood", QEstMethod), # If commenting out, remove from select() below too
    FloodwaveShape = ifelse(is.na(FloodwaveShape), "Base", FloodwaveShape),
    FloodwaveIntensity = ifelse(is.na(FloodwaveIntensity), "Base Flood", FloodwaveIntensity),
    BackwaterCondition = ifelse(is.na(BackwaterCondition), "Base Flood", BackwaterCondition),
    ChannelShape = ifelse(is.na(ChannelShape), "Rectangular/Prismatic", ChannelShape),
  ) %>%
  select(BedSlopeCondition, RoughnessCondition, FloodwaveShape, FloodwaveIntensity, 
         BackwaterCondition, ChannelShape, Time_V) %>%  # tested univariately: {Time_Q, Time_V, Fr_Max} as responses so far
  mutate(across(-Time_V, as.factor))  # tested univariately: {Time_Q, Time_V, Fr_Max} as responses so far

# Check out the data
anyNA(df_lm)  # should return FALSE

# Fit the model
fit <- lm(Time_V ~ ., data = df_lm)    # tested univariately: {Time_Q, Time_V, Fr_Max} as responses so far

# View the model summary
summary(fit)

# Perform ANOVA to see variance explained by each factor
anova(fit)

# Extract tidy summary of model
tidy_fit <- tidy(fit)

# Filter out intercept and plot coefficients
tidy_fit %>%
  filter(term != "(Intercept)", !is.na(estimate)) %>%
  ggplot(aes(x = reorder(term, estimate), y = estimate)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Linear Model Coefficients",
       x = "Driver Variables",
       y = "Effect on Time_V") +    # tested univariately: {Time_Q, Time_V, Fr_Max} as responses so far
  theme_minimal()

# Diagnostic plot
plot(fit, which = 1, main = "Residuals vs Fitted")



# Random Forest Variable Importance

# Set default values and prepare data
df_rf <- summarized_gAS0_filtered %>%
  mutate(
    BedSlopeCondition = ifelse(is.na(BedSlopeCondition), "Base (S0=0.00027)", BedSlopeCondition),
    RoughnessCondition = ifelse(is.na(RoughnessCondition), "Base (n=0.03)", RoughnessCondition),
    #QEstMethod = ifelse(is.na(QEstMethod), "Base Flood", QEstMethod),   # If commenting out, remove from select() below too
    FloodwaveShape = ifelse(is.na(FloodwaveShape), "Base", FloodwaveShape),
    FloodwaveIntensity = ifelse(is.na(FloodwaveIntensity), "Base Flood", FloodwaveIntensity),
    ChannelShape = ifelse(is.na(ChannelShape), "Rectangular/Prismatic", ChannelShape),
    BackwaterCondition = ifelse(is.na(BackwaterCondition), "Base Flood", BackwaterCondition)
  ) %>%
  select(BedSlopeCondition, RoughnessCondition, ChannelShape, FloodwaveShape,
         FloodwaveIntensity, BackwaterCondition, Time_V) %>%   # tested univariately: {Time_Q, Time_V, Fr_Max} as responses so far
  mutate(across(-Time_V, as.factor))    # tested univariately: {Time_Q, Time_V, Fr_Max} as responses so far

# Fit the random forest model
set.seed(123)  # reproducibility
rf_model <- randomForest(Time_V ~ ., data = df_rf, importance = TRUE)   # tested univariately: {Time_Q, Time_V, Fr_Max} as responses so far

# View model summary
print(rf_model)

# Show variable importance
importance(rf_model)
varImpPlot(rf_model)

# Variable importance plot
varImpPlot(rf_model,
           main = "Variable Importance (Random Forest)",
           col = "forestgreen")

# Clean ggplot version
# Create data frame from importance
importance_df <- as.data.frame(importance(rf_model))
importance_df$Variable <- rownames(importance_df)

# ggplot version with IncNodePurity (for regression trees)
ggplot(importance_df, aes(x = reorder(Variable, IncNodePurity), y = IncNodePurity)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Random Forest Variable Importance",
       x = "Driver Variables",
       y = "Increase in Node Purity") +
  theme_minimal()




# Multi-variate ANOVA
# Create the MANOVA model
manova_model <- manova(cbind(Time_Q, Time_V, Time_FSS, Max_gAdydx, Max_dQVdx) ~ 
                         BedSlopeCondition + 
                         RoughnessCondition + 
                         QEstMethod + 
                         FloodwaveShape + 
                         FloodwaveIntensity + 
                         BackwaterCondition + 
                         ChannelShape, 
                       data = summarized_gAS0)

# Summary with Pillai’s trace (robust to assumption violations)
summary(manova_model, test = "Pillai")
   # This says all but one are super significant...

# Follow-up univariate ANOVAs
summary.aov(manova_model)