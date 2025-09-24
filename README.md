# HysteresisSensitivity
Data repository for the Sensitivity manuscript

# Paper title: A Sensitivity Analysis Approach to Identifying Drivers of Streamflow Hysteresis
# Authors: Emma House, Ehab Meselhe, Marian Muste, Kyeongdong Kim, and Ibrahim Demir

# Included:
**Added_VBA_Functions.docx** VBA functions (Step 0) to run our sensitivity analysis through the RAS Controller. RAS Controller is a proprietary software that can be purchased online. 

**RASController_processing_new.R** R script (Step 1) to read in the data from the RAS Controller and calculate the momentum terms for the sensitivity scenarios.

**RASController_wrangling.R** R script (Step 2) to plot the calculated momentum terms for the sensitivity scenarios in batch/matrix form (like in the supplementary material of the manuscript).

**RASController_summary.R** R script (Step 3) to plot the summary plots for all sensitivity scenarios and batches (scatter plots and gradient plots), and run statistical analyses (ANOVA and RF).
