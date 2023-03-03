#---------------------------------------------------------------------
# This script assesses the magnitude of noise of the raw acceleration signals in rest across brands and various configurations: dynamic range and sampling frequency.
# - No-movement segments collected as part of the mechanical shaker experiments (except ms_bag experiment) were used as input
# - Measure of noise: standard deviation of the raw acceleration signal (x-, y-, and z-axis)
# - Aggregated per unique combination of brand, sampling frequency, and dynamic range as the median, mean and 95th percentile
rm(list=ls())
graphics.off()

# User input
shaker_experiments_folder = "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/Mechanical Shaker Machine" # Update to be your local directory
# shaker_experiments_folder = "~/data/VUMC/shaker_experiments"

# Specify file paths
datafile = paste0(shaker_experiments_folder, "/analyses/no_movement.RData") # Can be retrieved by running subset_data.R, setting: analysis <- "noise"

noise_output = extracted_data_path = paste0(shaker_experiments_folder, "/analyses/noise")
if (!dir.exists(noise_output)) {
  dir.create(noise_output)
}

#======================================================================================
# Load no-movement segment data
load(datafile)

# Visual inspection showed no apparent movement for: activPAL devices aP_490 and aP_254 (ms_lfcr), 
# and Axivity devices Ax_406 (ms_lfcr + ms_hfcr) and Ax_834 (ms_hfcr)
# These devices were therefore removed from analyses
deviates_lfcr <- c("aP_490", "aP_254", "Ax_406")
deviates_hfcr <- c("Ax_406", "Ax_834")
index_lfcr <- which(noise_data$label %in% deviates_lfcr & noise_data$experiment == "ms_lfcr")
noise_data <- noise_data[-index_lfcr,]
index_hfcr <- which(noise_data$label %in% deviates_hfcr & noise_data$experiment == "ms_hfcr")
noise_data <- noise_data[-index_hfcr,]

# Aggregate per unique combination of brand, dynamic range and sampling frequency
combinations <- unique(data$specifications[c("brand", "dynamic_range", "sampling_frequency")])
sds <- data.frame()
results_agg <- data.frame()

for(combination in 1:nrow(combinations)){
  index <- which(data$specifications$brand == combinations$brand[combination] & 
                   data$specifications$dynamic_range == combinations$dynamic_range[combination] &
                   data$specifications$sampling_frequency == combinations$sampling_frequency[combination])
  tmp <- data$specifications[index,]
  sub <- data$data[index]
  
  # Calculate the standard deviation for all three axes for each device
  sd_x <- c()
  sd_y <- c()
  sd_z <- c()
  for (device in 1:length(sub)) {
    df <- sub[[device]]
    sd_x <- c(sd_x, sd(df$x))
    sd_y <- c(sd_y, sd(df$y))
    sd_z <- c(sd_z, sd(df$z))
  }
  row <- cbind(sd_x, sd_y, sd_z)
  sds <- rbind(sds, row)
  # Create descriptive table by aggregating
  if(length(tmp) != 0) {
    results <- c(brand = unique(tmp$brand), n = nrow(tmp), dynamic_range = unique(tmp$dynamic_range), sampling_frequency = unique(tmp$sampling_frequency),
                 mean_x = mean(sd_x), mean_y = mean(sd_y), mean_z = mean(sd_z), 
                 median_x = median(sd_x), median_y = median(sd_y), median_z = median(sd_z), 
                 percentile_x = quantile(sd_x, .95), percentile_y = quantile(sd_y, .95), percentile_z = quantile(sd_z, .95))
    results_agg <- rbind(results_agg, results)
  }
}
noise_data <- cbind(data$specifications, sds)
noise_data$brand <- as.factor(noise_data$brand)
noise_data$sampling_frequency <- as.factor(as.integer(noise_data$sampling_frequency))
noise_data$dynamic_range <- as.factor(noise_data$dynamic_range)


save(noise_data, file = paste0(noise_output, "/noise_results.RData"))

colnames(results_agg) <- c("brand", "n", "dynamic_range", "sampling_frequency", "mean_x", "mean_y", "mean_z",
                           "median_x", "median_y", "median_z", "percentile95_x", "percentile95_y", "percentile95_z")
save(results_agg, file = paste0(noise_output, "/noise_aggregated_results.RData"))

# Statistical test differences 
res.man <- manova(cbind(sd_x, sd_y, sd_z) ~ brand + dynamic_range + sampling_frequency, data = noise_data)
summary(res.man)
summary.aov(res.man)
effectsize::eta_squared(res.man)

#boxplot <- boxplot(c(sd_x*1000, sd_y*1000, sd_z*1000)~brand, data = noise_data, ylab = "Standard deviation y-axis (mg)")

# Differences found between brands for the y-axis
jpeg(paste0(noise_output, "/boxplot_noise_brands.jpeg"))
boxplot <- boxplot(sd_y*1000~brand, data = noise_data, ylab = "Standard deviation y-axis (mg)")
dev.off()
# Post hoc test
model.y <- glm(sd_y ~ brand, data = noise_data)
emmRes.y <- emmeans::emmeans(model.y, comparison="pairwise", 
                             specs= ~ brand, adjust="sidak", data = noise_data) # Compute estimated marginal means for the desired fixed effects
contrast.y <- emmeans::contrast(emmRes.y, "pairwise") # Show pairwise contrasts for the fixed effects

# Visualize results
positions <- c("Actigraph", "Activpal", "Axivity", "GENEActiv", "MOX")

plot_noise_x <- ggplot2::ggplot(noise_data, ggplot2::aes(x = brand, y = sd_x)) +
  ggplot2::geom_boxplot(ggplot2::aes(color = sampling_frequency)) +
  ggplot2::theme_light() + ggplot2::scale_x_discrete(limits = positions)

plot_noise_y <- ggplot2::ggplot(noise_data, ggplot2::aes(x = brand, y = sd_y)) +
  ggplot2::geom_boxplot(ggplot2::aes(color = sampling_frequency)) +
  ggplot2::theme_light() + ggplot2::scale_x_discrete(limits = positions)

plot_noise_z <- ggplot2::ggplot(noise_data, ggplot2::aes(x = brand, y = sd_z)) +
  ggplot2::geom_boxplot(ggplot2::aes(color = sampling_frequency)) +
  ggplot2::theme_light() + ggplot2::scale_x_discrete(limits = positions)

gridExtra::grid.arrange(plot_noise_x, plot_noise_y, plot_noise_z, nrow=3) #arranges plots within grid

noise_plots <- gridExtra::arrangeGrob(plot_noise_x + ggplot2::theme(legend.position="top"), 
                                      plot_noise_y + ggplot2::theme(legend.position="none"), 
                                      plot_noise_z + ggplot2::theme(legend.position="none"), 
                                      nrow=3) # generates plot
plot(noise_plots) #print the plot
