#II6

read_sample_from_file <- function(filename) {
  sample <- as.numeric(readLines(filename))
  return(sample[!is.na(sample)])
}

calculate_mean <- function(sample) {
  return(mean(sample))
}
calculate_confidence_interval <- function(sample, sigma, confidence_level = 0.95) {
  n <- length(sample)
  mean_sample <- calculate_mean(sample)
  
  z <- qnorm((1 + confidence_level) / 2)
  
  margin_of_error <- z * (sigma / sqrt(n))
  lower_bound <- mean_sample - margin_of_error
  upper_bound <- mean_sample + margin_of_error
  
  return(list(mean = mean_sample, sample_size = n, lower_bound = lower_bound, upper_bound = upper_bound))
}
sample <- read_sample_from_file('history.txt')
sigma <- 5
confidence_interval <- calculate_confidence_interval(sample, sigma)
cat("Sample Size:", confidence_interval$sample_size, "\n")
cat("Sample Mean:", round(confidence_interval$mean, 2), "\n")
cat("95% Confidence Interval: (", round(confidence_interval$lower_bound, 2), ", ", round(confidence_interval$upper_bound, 2), ")\n", sep = "")

#III4

read_sample_from_file <- function(filename) {
  sample <- as.numeric(readLines(filename))
  return(sample[!is.na(sample)])
}
calculate_mean <- function(sample) {
  return(mean(sample))
}
calculate_sd <- function(sample) {
  return(sd(sample))
}
calculate_confidence_interval <- function(sample, confidence_level) {
  n <- length(sample)
  mean_sample <- calculate_mean(sample)
  sd_sample <- calculate_sd(sample)
  z <- qnorm((1 + confidence_level) / 2)
  margin_of_error <- z * (sd_sample / sqrt(n))
  lower_bound <- mean_sample - margin_of_error
  upper_bound <- mean_sample + margin_of_error
  return(list(mean = mean_sample, sd = sd_sample, sample_size = n, lower_bound = lower_bound, upper_bound = upper_bound))
}
sample <- read_sample_from_file('history.txt')
confidence_interval_95 <- calculate_confidence_interval(sample, 0.95)
cat("95% Confidence Interval:\n")
cat("Sample Size:", confidence_interval_95$sample_size, "\n")
cat("Sample Mean:", round(confidence_interval_95$mean, 2), "\n")
cat("Sample Standard Deviation:", round(confidence_interval_95$sd, 2), "\n")
cat("95% Confidence Interval: (", round(confidence_interval_95$lower_bound, 2), ", ", round(confidence_interval_95$upper_bound, 2), ")\n", sep = "")
confidence_interval_99 <- calculate_confidence_interval(sample, 0.99)
cat("99% Confidence Interval:\n")
cat("Sample Size:", confidence_interval_99$sample_size, "\n")
cat("Sample Mean:", round(confidence_interval_99$mean, 2), "\n")
cat("Sample Standard Deviation:", round(confidence_interval_99$sd, 2), "\n")
cat("99% Confidence Interval: (", round(confidence_interval_99$lower_bound, 2), ", ", round(confidence_interval_99$upper_bound, 2), ")\n", sep = "")

#IV2

p0 <- 0.10  
n <- 150  
x <- 20 
phat <- x / n
z <- (phat - p0) / sqrt(p0 * (1 - p0) / n)
p_value <- 1 - pnorm(z)
alpha <- 0.05
cat("Proporția observată:", round(phat, 4), "\n")
cat("Statistica z:", round(z, 4), "\n")
cat("Valoarea p:", round(p_value, 4), "\n")
cat("Nivel de semnificație:", alpha, "\n")
if (p_value < alpha) {
  cat("Respingem ipoteza nulă: Procentul componentelor defecte este mai mare de 10%.\n")
} else {
  cat("Nu putem respinge ipoteza nulă: Nu există suficiente dovezi că procentul componentelor defecte este mai mare de 10%.\n")
}
