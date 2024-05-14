#1.2

ex1 = function(num_puncte) {
  parabola = function(x) {
    return(-2*x^2 + 5*x - 2)
  }
  
  x_coord = runif(num_puncte, 0, 2)
  y_coord = runif(num_puncte, 0, 2)
  
  puncte_sub_parabola = sum(y_coord <= parabola(x_coord))
  
  aria_estimata = (puncte_sub_parabola / num_puncte) * 4
  
  aria_exacta = integrate(parabola, 1/2, 2)$value
  
  eroare_relativa = abs(aria_estimata - aria_exacta) / aria_exacta
  
  print(paste("Aria estimata:", aria_estimata))
  print(paste("Aria exacta:", aria_exacta))
  print(paste("Eroarea relativa:", eroare_relativa))
}

#2.1
#a)
MC_integration = function(N) {
  sum = 0;
  for(i in 1:N) {
    u = runif(1, 0, pi);
    sum = sum + sin(u)*sin(u)
  }
  return(pi*sum/N);
}
abs_err =abs(MC_integration(20000)-pi/2)
rel_err =abs_err(pi/2)
print(abs_err)
print(rel_err)

#b)
MC_integration_b = function(N) {
  sum = 0;
  for(i in 1:N) {
    u = runif(1, 1, 4);
    sum = sum + exp(u); 
  }
  return(4*sum/N);
}
abs_err =abs(MC_integration_b(20000)-51.87987)
rel_err =abs_err/51.87987
print(abs_err)
print(rel_err)

#d)
MC_integration_b = function(N) {
  sum = 0;
  for(i in 1:N) {
    u = runif(1, 1, Inf);
    sum = sum + 1/(4*u*u-1); 
  }
  return(4*sum/N);
}
abs_err =abs(MC_integration_b(20000)-log(3/4))
rel_err =abs_err/log(3/4)
print(abs_err)
print(rel_err)

#2.2
MC_improved_integration = function(N) {
  sum = 0
  for (i in 1:N) {
    u = rexp(1, 3)
    sum = sum + exp(-2*u*u); 
  }
  return(sum/N)
}

MC_imprvd_integr_average = function(k, N) {
  estimates = numeric(k) 
  for (i in 1:k) {
    estimates[i] = MC_improved_integration(N)  
  }
  mean_estimate = mean(estimates)
  sd_estimate = sd(estimates)  
  print(paste("medie estimari", mean_estimate))
  print(paste("deviatie standard", sd_estimate))
}

MC_imprvd_integr_average(30, 50000)

#3.2
generate_service_time = function(lambda) {
  u = runif(1)
  return(-log(1 - u) / lambda)
}

generate_service_time_individual = function() {
  if (runif(1) < 3/4) {
    return(generate_service_time(lambda1))
  } else {
    return(generate_service_time(lambda2))
  }
}

estimate_average_service_time = function(N) {
  service_times = numeric(N)
  for (i in 1:N) {
    service_times[i] = generate_service_time_individual()
  }
  return(mean(service_times))
}

lambda1 = 4
lambda2 = 12

estimated_average_time = estimate_average_service_time(10000)
print(estimated_average_time)