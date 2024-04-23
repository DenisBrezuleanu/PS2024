tablou = read.csv("life_expect.csv", header = T, sep = ',')

rate1 = tablou[['male']]
mean(rate1)
median(rate1)

rate2 = tablou[['female']]
mean(rate2)
median(rate2)