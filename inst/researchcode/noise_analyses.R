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
# Load no movement data
load(datafile)

# Exclude two devices (aP_490 and aP_254) from the low sampling frequency experiment (ms_lfcr)
#deviates <- c("aP_490", "aP_254")
#index_deviates <- which(names(data$data) %in% deviates & data$specifications$experiment == "ms_lfcr")
#specifications <- data$specifications[-index_deviates,]
#data <- data$data
#data[index_deviates] <- NULL
#data <- list(data = data, specifications = specifications)
#rm(specifications, deviates, index_deviates)

# Calculate the standard deviation for all three axes for each device
sd_x <- c()
sd_y <- c()
sd_z <- c()
for (device in 1:length(data$data)) {
  df <- data$data[[device]]
  sd_x <- c(sd_x, sd(df$x))
  sd_y <- c(sd_y, sd(df$y))
  sd_z <- c(sd_z, sd(df$z))
}

noise_data <- cbind(data$specifications, sd_x, sd_y, sd_z)
#noise_data$brand <- as.factor(noise_data$brand)
#noise_data$experiment <- as.factor(noise_data$experiment)
noise_data$sampling_frequency <- round(as.integer(noise_data$sampling_frequency))
rm(device, df, sd_x, sd_y, sd_z)
save(noise_data, file = paste0(noise_output, "/noise.RData"))

#Create descriptive table by aggregating per brand
# Aggregate per unique combination of brand, dynamic range and sampling frequency
combinations <- unique(noise_data[c("brand", "dynamic_range", "sampling_frequency")])
results_agg <- data.frame()
for(combination in 1:nrow(combinations)){
  index <- which(noise_data$brand == combinations$brand[combination] & 
                   noise_data$dynamic_range == combinations$dynamic_range[combination] &
                   noise_data$sampling_frequency == combinations$sampling_frequency[combination])
  tmp <- noise_data[index,]
  if(length(tmp) != 0) {
    results <- c(brand = unique(tmp$brand), dynamic_range = unique(tmp$dynamic_range), sampling_frequency = unique(tmp$sampling_frequency),
                     mean_x = mean(tmp$sd_x), mean_y = mean(tmp$sd_y), mean_z = mean(tmp$sd_z), 
                     median_x = median(tmp$sd_x), median_y = median(tmp$sd_y), median_z = median(tmp$sd_z), 
                     percentile_x = quantile(tmp$sd_x, .95), percentile_y = quantile(tmp$sd_y, .95), percentile_z = quantile(tmp$sd_z, .95))
    results_agg <- rbind(results_agg, results)
  }
}
colnames(results_agg) <- c("brand", "dynamic_range", "sampling_frequency", "mean_x", "mean_y", "mean_z",
                           "median_x", "median_y", "median_z", "percentile95_x", "percentile95_y", "percentile95_z")
save(results_agg, file = paste0(noise_output, "/noise_aggregated_results.RData"))


# Plot noise for the brands with separate panels for the axes
positions <- c("Actigraph", "Activpal", "Axivity", "GENEActiv", "MOX")

plot_noise_x <- ggplot2::ggplot(noise_data, ggplot2::aes(x = brand, y = sd_x)) +
  ggplot2::geom_boxplot(ggplot2::aes(color = experiment)) +
  ggplot2::theme_light() + ggplot2::scale_x_discrete(limits = positions)
plot_noise_y <- ggplot2::ggplot(noise_data, ggplot2::aes(x = brand, y = sd_y)) +
  ggplot2::geom_boxplot(ggplot2::aes(color = experiment)) +
  ggplot2::theme_light() + ggplot2::scale_x_discrete(limits = positions)
plot_noise_z <- ggplot2::ggplot(noise_data, ggplot2::aes(x = brand, y = sd_z)) +
  ggplot2::geom_boxplot(ggplot2::aes(color = experiment)) +
  ggplot2::theme_light() + ggplot2::scale_x_discrete(limits = positions)

gridExtra::grid.arrange(plot_noise_x, plot_noise_y, plot_noise_z, nrow=3) #arranges plots within grid

noise_plots <- gridExtra::arrangeGrob(plot_noise_x + ggplot2::theme(legend.position="top"), 
                                      plot_noise_y + ggplot2::theme(legend.position="none"), 
                                      plot_noise_z + ggplot2::theme(legend.position="none"), 
                                      nrow=3) # generates plot
plot(noise_plots) #print the plot

ggplot2::ggsave(file=paste0(noise_output, "/boxplots_noise.jpeg"), noise_plots) #saves plot

### Statistical analyses: test for each brand if noise is different during the experiments (i.e. significant interaction-effect)
# x-axis
model.x <- lme4::lmer(sd_x ~ experiment * brand + (1 | label), data = noise_data)
summary(model.x)
car::Anova(model.x, type="II", test.statistic="F")
require(lmerTest)
test.x <- as(model.x, "merModLmerTest")
print(summary(test.x, ddf="Kenward-Roger"), correlation = FALSE)
# There are no differences in noise between experiments within brand
emmRes.x <- emmeans::emmeans(model.y, comparison="pairwise", 
                             specs= ~ brand*experiment, adjust="sidak", data = noise_data) # Compute estimated marginal means for the desired fixed effects
contrast.x <- emmeans::contrast(emmRes.x, "pairwise") # Show pairwise contrasts for the fixed effects
contrast_df.x <- as.data.frame(contrast.x)

focusOfInterest <- c()
check <- strsplit(contrast_df.x$contrast, " ")
for (row in 1:nrow(contrast_df.x)) {
  focusOfInterest <- c(focusOfInterest, (check[[row]][2] == check[[row]][5] 
                                         & check[[row]][1] !=   check[[row]][4])) #focus on rows where brand differs, but the experiment is the same
}
ct.interest.x <- contrast_df.x[focusOfInterest,]

# y-axis
model.y <- lme4::lmer(sd_y ~ experiment * brand + (1 | label), data = noise_data)
summary(model.y)
car::Anova(model.y, type="II", test.statistic="F")
require(lmerTest)
test.y <- as(model.y, "merModLmerTest")
print(summary(test.y, ddf="Kenward-Roger"), correlation = FALSE)

# Differences in noise between experiments are dependent on brand
emmRes.y <- emmeans::emmeans(model.y, comparison="pairwise", 
                             specs= ~ experiment*brand, adjust="sidak", data = noise_data) # Compute estimated marginal means for the desired fixed effects
contrast.y <- emmeans::contrast(emmRes.y, "pairwise") # Show pairwise contrasts for the fixed effects
contrast_df.y <- as.data.frame(contrast.y)
focusOfInterest <- c()
check <- strsplit(contrast_df.y$contrast, " ")
for (row in 1:nrow(contrast_df.y)) {
  focusOfInterest <- c(focusOfInterest, (check[[row]][2] == check[[row]][5] 
                                         & check[[row]][1] !=   check[[row]][4])) #focus on rows where brand is the same, but the experiment differs.
}
ct.interest.y <- contrast_df.y[s]
# There were no differences between experiments for the same brand

# z-axis
model.z <- lme4::lmer(sd_z ~ experiment * brand + (1 | label), data = noise_data)
summary(model.z)
car::Anova(model.z, type="II", test.statistic="F")
require(lmerTest)
test.z <- as(model.z, "merModLmerTest")
print(summary(test.z, ddf="Kenward-Roger"), correlation = FALSE)

# Differences in noise between experiments are dependent on brand
emmRes.z <- emmeans::emmeans(model.z, comparison="pairwise", 
                             specs= ~ experiment*brand, adjust="sidak", data = noise_data) # Compute estimated marginal means for the desired fixed effects
contrast.z <- emmeans::contrast(emmRes.z, "pairwise") # Show pairwise contrasts for the fixed effects
contrast_df.z <- as.data.frame(contrast.z)
focusOfInterest <- c()
check <- strsplit(contrast_df.z$contrast, " ")
for (row in 1:nrow(contrast_df.z)) {
  focusOfInterest <- c(focusOfInterest, (check[[row]][2] == check[[row]][5] 
                                         & check[[row]][1] !=   check[[row]][4])) #focus on rows where brand is the same, but the experiment differs.
}
ct.interest.z <- contrast_df.z[focusOfInterest,]
#noise in z-axis is significantly different between experiments for GENEActiv