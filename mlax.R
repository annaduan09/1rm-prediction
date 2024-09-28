#### Setup ####
library(tidyverse)
library(conflicted)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)

# read data
dat <- read_csv("MLAX_Squat_Velocity.csv") %>%
  distinct(Name, `Weight (lbs)`, .keep_all = TRUE) %>% # Remove sets with duplicate weights
  rename(name = Name,
         set_id = `Set ID`,
         weight = `Weight (lbs)`,
         reps = `Rep Count`,
         avg_velocity = `Avg Mean Velocity (m/s)`) %>%
  drop_na(weight, avg_velocity) %>%
  mutate(weight = as.numeric(weight),
         avg_velocity = as.numeric(avg_velocity)) %>%
  group_by(name) %>%
  filter(n() > 1) %>%  # Ensure valid_sets > 1 for each athlete
  ungroup()

#### Calculate max squat predictions ####
# max squat prediction function
calc_max <- function(data) {
  linear_model <- lm(weight ~ avg_velocity, data = data)
  
  max_pred <- predict(linear_model, newdata = data.frame(avg_velocity = 0.25))
  
  mse <- mean((data$weight - predict(linear_model, data))^2)
  r_squared <- summary(linear_model)$r.squared
  
  return(tibble(
    name = unique(data$name),       
    Max_Pred = max_pred,
    MSE = mse,
    R2 = r_squared,
    valid_sets = nrow(data)
  ))
}


# Calculate max squat predictions for all athletes
pred_max_squats <- dat %>%
  group_by(name) %>%
  group_modify(~ calc_max(.x)) %>%
  ungroup()


#### Plot the results ####
# Define the plotting function with the requested tweaks
plot_max_squat <- function(data, max_pred, athlete_name) {
  # Convert athlete_name to character to ensure proper handling
  athlete_name <- as.character(athlete_name)
  
  # Debugging: Print the athlete's name to confirm it's received correctly
  print(paste("Plotting for athlete:", athlete_name))
  
  # Fit the linear model
  linear_model <- lm(weight ~ avg_velocity, data = data)
  
  # Extract model coefficients
  intercept <- coef(linear_model)[1]
  slope <- coef(linear_model)[2]
  
  # Calculate the predicted weight at avg_velocity = 0.25 m/s
  predicted_weight <- max_pred
  
  # Define the x-axis range to include the predicted max
  x_max <- max(data$weight, predicted_weight) * 1.1  # Add 10% margin
  x_values <- seq(min(data$weight), x_max, length.out = 100)
  
  # Calculate corresponding avg_velocity values for the regression line
  regression_velocities <- (x_values - intercept) / slope
  
  # Create a data frame for the regression line
  regression_line <- data.frame(
    weight = x_values,
    avg_velocity = regression_velocities
  )
  
  # Create the plot
  ggplot(data, aes(x = weight, y = avg_velocity)) +
    geom_point(color = "gray20", size = 3) + 
    geom_line(data = regression_line, aes(x = weight, y = avg_velocity), color = "darkcyan") + 
    geom_hline(yintercept = 0.25, linetype = "dashed", color = "gray40") + 
    scale_x_continuous(limits = c(0, x_max)) + 
    annotate(
      "text", 
      x = predicted_weight * 0.8, 
      y = 0.25 + 0.02, 
      label = paste("Predicted Max:", round(predicted_weight, 1), "lbs"), 
      color = "black",
      size = 4
    ) +
    ggtitle(paste("Estimated Max Squat:", athlete_name)) + 
    labs(
      subtitle = "September 2024",
      x = "Weight (lbs)",
      y = "Average Velocity (m/s)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )
}


# Generate plots for each athlete
plots <- dat %>%
  group_by(name) %>%
  nest() %>% # Nest data by athlete
  left_join(pred_max_squats, by = "name") %>% # Correctly join by name
  mutate(plot = pmap(list(data, Max_Pred, name), ~ plot_max_squat(..1, ..2, ..3)))

# Display a specific plot (e.g., the first athlete)
print(plots$plot[[12]])

#### Export ####
write_csv(pred_max_squats, "predicted_max_squats.csv")
plots %>%
  mutate(filename = paste0("Max_Squat_Pred_", str_replace_all(name, " ", "_"), ".png")) %>%
  pwalk(function(plot, filename, ...) {
    ggsave(filename, plot = plot, width = 8, height = 6, dpi = 300)
    message("Saved plot to ", filename)
  })
