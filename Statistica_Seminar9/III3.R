outliers_mean = function(sample){
  m = mean(sample)
  s = sd(sample)
  outliers = vector()
  j = 0
  for(i in 1:length(sample))
    if(sample[i] <m - 2*s | sample[i] > m + 2*s) {
      j = j + 1
      outliers[j] = sample[i]
    }
  return(outliers)
}

outliers_iqr = function(sample){
  q_1 = as.vector(quantile(sample))[2]
  q_3 = as.vector(quantile(sample))[4]
  iqr = q_3-q_1
  outliers = vector()
  j = 0
  for(i in 1:length(sample))
    if(sample[i] < q_1 - 1.5 * iqr | sample[i] > q_3 + 1.5 * iqr) {
      j = j + 1
      outliers[j] = sample[i]
    }
  return(outliers)
}

tablou = scan("sample2.txt")
outliers_mean(tablou)
outliers_iqr(tablou)
summary(tablou)