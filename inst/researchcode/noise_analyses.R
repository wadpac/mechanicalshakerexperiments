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
deviates_lfcr <- c("aP_490", "aP_254") #, "Ax_406")
#deviates_hfcr <- c("Ax_406", "Ax_834")

index_lfcr <- which(data$specifications$label %in% deviates_lfcr & data$specifications$experiment == "ms_lfcr")
data$specifications <- data$specifications[-index_lfcr,]
data$data[index_lfcr] <- NULL
#index_hfcr <- which(data$specifications %in% deviates_hfcr & noise_data$experiment == "ms_hfcr")
#noise_data <- noise_data[-index_hfcr,]

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
}
noise_data <- cbind(data$specifications, sds)
noise_data$brand <- as.factor(noise_data$brand)
noise_data$sampling_frequency <- as.integer(noise_data$sampling_frequency)
noise_data$dynamic_range <- as.integer(noise_data$dynamic_range)
save(noise_data, file = paste0(noise_output, "/noise_results.RData"))

# Create descriptive table by aggregating
require(dplyr)
results_agg <- noise_data %>%
  group_by(brand, dynamic_range, sampling_frequency) %>%
  summarize(N = n(), mean_x = mean(sd_x), mean_y = mean(sd_y), mean_z = mean(sd_z), 
            median_x = median(sd_x), median_y = median(sd_y), median_z = median(sd_z), 
            percentile_x = quantile(sd_x, .95), percentile_y = quantile(sd_y, .95), percentile_z = quantile(sd_z, .95))
save(results_agg, file = paste0(noise_output, "/noise_aggregated_results.RData"))

# Statistical test differences MANOVA
# -Assumption: n in each cell > the number of outcome variables
noise_data %>%
group_by(brand, dynamic_range, sampling_frequency) %>%
  summarise(N = n())

# -Assumption: univariate or multivariate outliers
noise_data %>%
  group_by(brand, dynamic_range, sampling_frequency) %>%
  rstatix::identify_outliers(sd_x)
noise_data %>%
  group_by(brand, dynamic_range, sampling_frequency) %>%
  rstatix::identify_outliers(sd_y)
noise_data %>%
  group_by(brand, dynamic_range, sampling_frequency) %>%
  rstatix::identify_outliers(sd_z)

# -Assumption: multivariate normality
ggpubr::ggqqplot(noise_data, "sd_x", facet.by = "brand",
         ylab = "sd_x")
ggpubr::ggqqplot(noise_data, "sd_x", facet.by = "dynamic_range",
                 ylab = "sd_x")
ggpubr::ggqqplot(noise_data, "sd_x", facet.by = "sampling_frequency",
                 ylab = "sd_x")

ggpubr::ggqqplot(noise_data, "sd_y", facet.by = "brand",
                 ylab = "sd_y")
ggpubr::ggqqplot(noise_data, "sd_y", facet.by = "dynamic_range",
                 ylab = "sd_y")
ggpubr::ggqqplot(noise_data, "sd_y", facet.by = "sampling_frequency",
                 ylab = "sd_y")

ggpubr::ggqqplot(noise_data, "sd_z", facet.by = "brand",
                 ylab = "sd_z")
ggpubr::ggqqplot(noise_data, "sd_z", facet.by = "dynamic_range",
                 ylab = "sd_z")
ggpubr::ggqqplot(noise_data, "sd_z", facet.by = "sampling_frequency",
                 ylab = "sd_z")

noise_data %>%
  select(sd_x, sd_y, sd_z) %>%
  rstatix::mshapiro_test()

# -Assumption: multicollinearity; dependent (outcome) variables cannot be too correlated to each other < 0.90 (Tabachnick & Fidell, 2012)
noise_data %>% rstatix::cor_test(sd_x, sd_y, sd_z)

# -Assumption: linearity between all outcome variables for each group
# Create a scatterplot matrix by group
library(GGally)
results <- noise_data %>%
  select(sd_x, sd_y, sd_z, brand) %>%
  group_by(brand) %>%
  rstatix::doo(~ggpairs(.) + theme_bw(), result = "plots")
results$plots

results <- noise_data %>%
  select(sd_x, sd_y, sd_z, dynamic_range) %>%
  group_by(dynamic_range) %>%
  rstatix::doo(~ggpairs(.) + theme_bw(), result = "plots")
results$plots

results <- noise_data %>%
  select(sd_x, sd_y, sd_z, sampling_frequency) %>%
  group_by(sampling_frequency) %>%
  rstatix::doo(~ggpairs(.) + theme_bw(), result = "plots")
results$plots

# -Assumption: homogeneity of variances. The Levene’s test can be used to test the equality of variances between groups. Non-significant values of Levene’s test indicate equal variance between groups.
rstatix::box_m(noise_data[, c("sd_x", "sd_y", "sd_z")], noise_data$brand)
rstatix::box_m(noise_data[, c("sd_x", "sd_y", "sd_z")], noise_data$dynamic_range)
rstatix::box_m(noise_data[, c("sd_x", "sd_y", "sd_z")], noise_data$sampling_frequency)
# violated: using Pillai's statistics instead of Willks'

# -Assumption: homogeneity of variance-covariance matrices. The Box’s M Test can be used to check the equality of covariance between the groups. This is the equivalent of a multivariate homogeneity of variance. This test is considered as highly sensitive. Therefore, significance for this test is determined at alpha = 0.001.
noise_data %>% 
  tidyr::gather(key = "variable", value = "value", sd_x, sd_y, sd_z) %>%
  group_by(variable) %>%
  rstatix::levene_test(value ~ brand)

noise_data %>% 
  tidyr::gather(key = "variable", value = "value", sd_x, sd_y, sd_z) %>%
  group_by(variable) %>%
  rstatix::levene_test(value ~ as.factor(dynamic_range))

noise_data %>% 
  tidyr::gather(key = "variable", value = "value", sd_x, sd_y, sd_z) %>%
  group_by(variable) %>%
  rstatix::levene_test(value ~ as.factor(sampling_frequency))

# For brand (sd_y, sd_z) and sampling frequency the Levene’s test is non-significant (p > 0.05), so there was homogeneity of variances.
# For brand (sd_x) and dynamic range the Levene’s test is significant (p < 0.05), so there was no homogeneity of variances.
# Use different post hoc to test for differences between brands (sd_x) and dynamic ranges

#Build MANOVA mocel
model <- lm(cbind(sd_x, sd_y, sd_z) ~ brand + dynamic_range + sampling_frequency, data = noise_data)
car::Manova(model, test.statistic = "Pillai")

# Follow-up univariate models
summary.aov(model)
effectsize::eta_squared(model)

#Post-hoc test
posthoc_brand <- noise_data %>%
  tidyr::gather(key = "variables", value = "value", sd_x, sd_y, sd_z) %>%
  group_by(variables) %>%
  rstatix::games_howell_test(value ~ brand) %>%
  select(-estimate, -conf.low, -conf.high) # Remove details

posthoc_sf <- noise_data %>%
  tidyr::gather(key = "variables", value = "value", sd_x, sd_y, sd_z) %>%
  group_by(variables) %>%
  rstatix::games_howell_test(value ~ sampling_frequency) %>%
  select(-estimate, -conf.low, -conf.high) # Remove details


# Visualize results
positions <- c("Actigraph", "Activpal", "Axivity", "GENEActiv", "MOX")

plot_noise_x <- ggplot2::ggplot(noise_data, ggplot2::aes(x = brand, y = sd_x*1000)) +
  ggplot2::geom_boxplot(ggplot2::aes(color = as.factor(sampling_frequency))) +
  ggplot2::theme_light() + ggplot2::scale_x_discrete(limits = positions)

plot_noise_y <- ggplot2::ggplot(noise_data, ggplot2::aes(x = brand, y = sd_y*1000)) +
  ggplot2::geom_boxplot(ggplot2::aes(color = as.factor(sampling_frequency))) +
  ggplot2::theme_light() + ggplot2::scale_x_discrete(limits = positions)

plot_noise_z <- ggplot2::ggplot(noise_data, ggplot2::aes(x = brand, y = sd_z*1000)) +
  ggplot2::geom_boxplot(ggplot2::aes(color = as.factor(sampling_frequency))) +
  ggplot2::theme_light() + ggplot2::scale_x_discrete(limits = positions)

gridExtra::grid.arrange(plot_noise_x, plot_noise_y, plot_noise_z, nrow=3) #arranges plots within grid

noise_plots <- gridExtra::arrangeGrob(plot_noise_x + ggplot2::theme(legend.position="top"), 
                                      plot_noise_y + ggplot2::theme(legend.position="none"), 
                                      plot_noise_z + ggplot2::theme(legend.position="none"), 
                                      nrow=3) # generates plot
plot(noise_plots) #print the plot



require(dplyr)
results_agg_brand <- noise_data %>%
  group_by(brand) %>%
  summarize(N = n(), mean_x = mean(sd_x*1000), mean_y = mean(sd_y*1000), mean_z = mean(sd_z*1000))

results_agg_sf <- noise_data %>%
  group_by(sampling_frequency) %>%
  summarize(N = n(), mean_x = mean(sd_x*1000), mean_y = mean(sd_y*1000), mean_z = mean(sd_z*1000))
