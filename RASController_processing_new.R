# 10/21/24 
# Emma House
# R Script for processing RAS Controller output - STEP 1

# importing the required libraries  
#install.packages("readxl")
library(openxlsx)  
library(readxl)  
library(ggplot2)
library(dplyr)
library(scales)


#### READ IN DATA FROM HECRAS CONTROLLER ####

# To read from RASController, specify the path name  
path <- "C:/Users/ehouse/OneDrive - Tulane University/Desktop/HECRAS Controller Backup/HECRASController_output46.xlsm"

# getting data from sheets  
sheets <- openxlsx::getSheetNames(path)  

# filtering to keep only sheets that start with "RASResults_"
ras_sheets <- grep("^RASResults_", sheets, value = TRUE)

# reading data from all sheets into a list of data frames
data_list <- lapply(ras_sheets, openxlsx::read.xlsx, xlsxFile=path)  

# assigning names to the list for easy reference
names(data_list) <- ras_sheets

# printing the structure of the list to check its content
str(data_list)

# Access the first sheet to check it out 
df1 <- data_list[[1]]  

# Make it a time series -- create and fill in datetime column with the correct dates
datetime_data <- read_excel("C:/Users/ehouse/Downloads/DateTime.xlsx")
# Ensure DateTime column is properly formatted as POSIXct 
datetime_data <- datetime_data %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%d%b%Y %H%M"))
# Trim it if needed for shorter sim
#datetime_data <- datetime_data[c(1:14401),]
# Loop through each element of data_list and bind the DateTime column
for (i in seq_along(data_list)) {
  data_list[[i]] <- bind_cols(datetime_data, data_list[[i]])
}

# Delete rows with extremely large values at the end (or fix in RAS Controller to resolve issue before it starts)
# Remove rows where Profile > 34560
data_list <- lapply(data_list, function(df) df[df$Profile <= 34560, ])

# Access the first sheet to check it out 
df1 <- data_list[[1]]  


#### DEFINE FUNCTIONS ####

# Bed slope lookup table
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

# Define function to fill in length column with 304.8 m (1,000 ft) 
length.col <- function(df) {
  if ("Len..Chnl..(m)" %in% colnames(df)) {
    df$'Len..Chnl..(m)' <- 304.8  # If the "Len..Chnl..(m)" column exists, fill it with 304.8
  } else {
    df$'Len..Chnl..(m)' <- 304.8  # If the "Len..Chnl..(m)" column doesn't exist, create it and fill it with 304.8
  }
  return(df)  # Return the updated data frame
}

# Define function to create col and calculate depth difference (DS-US) 
diff.col <- function(df) {
  # Calculate depth difference
  df$'Depth.Diff(m)' <- df$'DS.Hyd..Depth.(m)' - df$'Hyd..Depth.(m)'
  return(df)  # Return the updated data frame
}


# Code all the momentum term calculations!!!  
# Define function to create cols and calculate dynamic momentum terms
dynamic.cols <- function(df) {
  # Ensure we have enough rows to calculate differences
  n <- nrow(df)
  if (n < 2) {
    warning("Not enough rows to calculate differences.")
    return(df)
  }
  # Calculate local acceleration dQ/dt
  dQ_US <- df$'Tot.Q.(cms)'[3:n] - df$'Tot.Q.(cms)'[2:(n-1)]
  dQ_DS <- df$'DS.Tot.Q.(cms)'[2:n] - df$'Tot.Q.(cms)'[1:(n-1)]
  dQdt <- 0.5 * (dQ_US / 60) + 0.5 * (dQ_DS / 60)
  df$'dQ/dt.(m3/s2)' <- c(dQdt, NA)  # Append NA to maintain length
  
  # Calculate convective acceleration dQV/dx 
  QV_US <- df$'Tot.Q.(cms)'[1:(n-1)] * df$'Avg..Vel..(m/s)'[1:(n-1)]
  QV_DS <- df$'DS.Tot.Q.(cms)'[2:n] * df$'DS.Avg..Vel..(m/s)'[2:n]
  dQVdx <- (QV_DS - QV_US) / df$'Len..Chnl..(m)'[1:(n-1)]
  df$'dQV/dx.(m3/s2)' <- c(dQVdx, NA)  # Append NA to maintain length
  
  return(df)  # Return the updated data frame
}


# Define function to create columns and calculate gA, S0, dydx, and Sw
supporting.cols <- function(df, df_name, slope_lookup) {
  n <- nrow(df)
  if (n < 3) {
    warning(paste("Not enough rows in", df_name, "to calculate differences."))
    return(df)
  }
  
  # Parse out the "gXX" from the name
  g_code <- sub(".*_(g[0-9]+)$", "\\1", df_name)
  
  # Look up the slope
  S0_val <- slope_lookup[[g_code]]
  if (is.null(S0_val)) {
    stop(paste("No slope value found for", g_code))
  }
  
  # Calculate gA
  # Choose gravity constant according to units
  gA <- (9.81 * df$'Flow.Area.(sqm)'[3:n] + 
           9.81 * df$'DS.Flow.Area.(sqm)'[2:(n-1)]) / 2
  df$'gA.(m3/s2)' <- c(NA, gA, NA)
  
  # Add slope
  df$'S0.(m/m)' <- S0_val
  
  # Calculate dy/dx
  dy_dx <- (df$'DS.Hyd..Depth.(m)'[2:(n-1)] - df$'Hyd..Depth.(m)'[3:n]) / 
    df$'Len..Chnl..(m)'[3:n]
  df$'dy/dx.(m/m)' <- c(NA, dy_dx, NA)
  
  # Calculate Sw
  Sw <- (df$'W.S..El..(m)' - df$'DS.W.S..El..(m)') / df$'Len..Chnl..(m)'
  df$'Sw.(m/m)' <- Sw
  
  return(df)
}


# Define function to create columns and calculate Kinematic, Diffusive, and Dynamic bulk Terms
# Maybe rename these later
bulk.cols <- function(df) {
  # Ensure we have enough rows to calculate differences
  n <- nrow(df)
  if (n < 2) {
    warning("Not enough rows to calculate differences.")
    return(df)
  }
  # Calculate Kinematic bulk term 
  kin <- df$'gA.(m3/s2)'[1:(n-1)] * (df$'S0.(m/m)'[1:(n-1)] - df$'Frctn..Slope.(-)'[2:n])
  df$'kin.(m3/s2)' <- c(kin, NA)
  
  # Calculate Diffusive bulk term 
  diff <- df$'gA.(m3/s2)'[2:n] * df$'dy/dx.(m/m)'[1:(n-1)]
  df$'diff.(m3/s2)' <- c(diff, NA)
  
  # Calculate Dynamic bulk term 
  dyn <- df$'dQ/dt.(m3/s2)'[1:n] + df$'dQV/dx.(m3/s2)'[1:n]  # IS indexing needed here? Am I overcomplicating things?
  df$'dyn.(m3/s2)' <- dyn
  
  return(df)  # Return the updated data frame
}


# Define function to create columns and calculate Kinematic terms: gASf and gAS0
kinematic.cols <- function(df) {
  # Ensure we have enough rows to calculate differences
  n <- nrow(df)
  if (n < 2) {
    warning("Not enough rows to calculate differences.")
    return(df)
  }
  
  # Calculate gAS0 
  gAS0 <- df$'gA.(m3/s2)'[1:(n-1)] * df$'S0.(m/m)'[1:(n-1)] 
  df$'gAS0' <- c(gAS0, NA)
  
  # Calculate gASf 
  gASf <- df$'gA.(m3/s2)'[1:(n-1)] * df$'Frctn..Slope.(-)'[2:(n-1)] 
  df$'gASf' <- c(gASf, NA)
  
  return(df)  # Return the updated data frame
}


# Define function to create columns and calculate Total mom, norm mom, and mom/all
momcheck.cols <- function(df) {
  # Calculate total momentum 
  Bulk.Mom <- df$'diff.(m3/s2)' + df$'dyn.(m3/s2)' - df$'kin.(m3/s2)'

  # Calculate total momentum with all terms 
  Sum.Mom <- abs(df$'dQ/dt.(m3/s2)') + abs(df$'dQV/dx.(m3/s2)') + abs(df$'diff.(m3/s2)') + abs(df$'gAS0')+ abs(df$'gASf')
  
  # Calculate Mom/all (this is our error score) 
  df$'Mom/all' <- abs(Bulk.Mom) / Sum.Mom
  
  return(df)  # Return the updated data frame
}



# Replace values in the first row with those in the second row (first row is max values)
replace_first_row <- function(df) {
  # Ensure we have at least two rows to perform the replacement
  if (nrow(df) < 2) {
    warning("Not enough rows to perform the replacement.")
    return(df)
  }
  # Replace values in the first row (from column 3 onwards) with those in the second row
  df[1, 3:ncol(df)] <- df[2, 3:ncol(df)]
  return(df)  # Return the updated data frame
}

# Function to normalize selected columns 0–100 and handle cases for inverse scaling
rescale_cols <- function(df, cols_to_rescale) {
  for (col_name in cols_to_rescale) {
    if (is.numeric(df[[col_name]])) {
      # Determine min and max of the column
      col_min <- min(df[[col_name]], na.rm = TRUE)
      col_max <- max(df[[col_name]], na.rm = TRUE)
      # Check if inversion is needed (for descending values)
      if (col_min < col_max) {
        # Standard rescaling from 0 to 100
        normalized_col <- 100 * (df[[col_name]] - col_min) / (col_max - col_min)
      } else {
        # Inverse scaling from 100 to 0
        normalized_col <- 100 * (1 - (df[[col_name]] - col_min) / (col_min - col_max))
      }
      # Create new column with "Norm_" prefix
      new_col_name <- paste0("Norm_", col_name)
      df[[new_col_name]] <- as.numeric(normalized_col)
    }
  }
  return(df)  # Return the updated data frame
}



#### RUN FUNCTIONS THROUGH DATA ####

# Loop through each sheet to apply the first few functions
for (sheet_name in names(data_list)) {
  df <- data_list[[sheet_name]]      # Extract the current data frame
  df <- length.col(df)               # Apply the function to fill in the 'length' column
  df <- diff.col(df)                 # Apply the function to fill in the depth diff column
  df <- dynamic.cols(df)             # Apply the function to fill in the dynamic variables columns
  data_list[[sheet_name]] <- df      # Save the modified data frame back into the list
}

# Access the first sheet to check the changes - it works! 
df1 <- data_list[[1]]
print(df1)
colnames(df1)

# Apply the new supporting cols function
data_list <- lapply(names(data_list), function(nm) {
  supporting.cols(data_list[[nm]], nm, g_bedslopevalue)
}) %>% setNames(names(data_list))


# Loop through each sheet to apply the next few functions
for (sheet_name in names(data_list)) {
  df <- data_list[[sheet_name]]      # Extract the current data frame
  df <- bulk.cols(df)                # Apply the function to fill in the bulk terms columns
  df <- kinematic.cols(df)           # Apply the function to fill in the kinematic terms columns
  data_list[[sheet_name]] <- df      # Save the modified data frame back into the list
}

# Adding momentum terms columns
for (sheet_name in names(data_list)) {
  df <- data_list[[sheet_name]]      # Extract the current data frame
  df <- momcheck.cols(df)            # Apply the function to fill in the momentum check columns
  data_list[[sheet_name]] <- df      # Save the modified data frame back into the list
}

# Check the range of the error score (should be very low...) 
summary(data_list[["RASResults_u01_g01"]][["Mom/all"]])   
     # Mean 0.3033 for u01_g02 pre-fixing slope thing, then 0.0065!! Yay!!

# Loop through each sheet in data_list and apply the first row replacement function
for (sheet_name in names(data_list)) {
  df <- data_list[[sheet_name]]         # Extract the current data frame
  df <- replace_first_row(df)           # Replace first row values with second row values for specified columns
  data_list[[sheet_name]] <- df         # Save the modified data frame back into the list
}

# Check the first sheet to see the changes - it works! 
df1 <- data_list[[1]]
print(df1)

# Define the columns you want to normalize/rescale
columns_to_rescale <- c("dQV/dx.(m3/s2)", "diff.(m3/s2)", "gAS0", "gASf",
                        "Sw.(m/m)", "Avg..Vel..(m/s)", "Tot.Q.(cms)", "W.S..El..(m)")  


# Loop through each sheet in data_list and apply the rescale function
for (sheet_name in names(data_list)) {
  df <- data_list[[sheet_name]]                 # Extract the current data frame
  df <- rescale_cols(df, columns_to_rescale)    # Rescale selected columns and add new columns
  data_list[[sheet_name]] <- df                 # Save the modified data frame back into the list
}

# Access the first sheet to check the changes - it works! 
df1 <- data_list[[1]]
print(df1)
colnames(df1)



#### Save Momentum Terms Data ####

# SAVE the data so we can reuse RAS Controller workbook
saveRDS(data_list, file="C:/Users/ehouse/Downloads/u01_roughness_2.RData")    # See spreadsheet for where we keep track of the permutations and their names


