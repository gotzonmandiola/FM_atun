library(tidyverse)
setwd(dir = "C:/Users/Gotzon Mandiola/Desktop/datos")
data_wide = read.table(file = "ts_fm_intersected_wide.csv", header = T, sep = ",", dec = ".")
data = filter(data_wide, sp %in% c("skj", "yft")) #para eliminar datos de pesca "mix", he filtrado los otros, skj y yft
data = pivot_longer(data, cols = 9:dim(data)[2], names_to = "freq", values_to = "TS") 
data = mutate(data, frequency = str_sub(data$freq, start = 2, end = length(data$freq)))
data = mutate(data, frequency = as.numeric(data$frequency)) #al ponerlos como números, 
install.packages("caret")                                                            #los que estan en formato cientifico pasan a NA

unique(data$frequency)
fpdisparo = unique(table(data$ref)) #cantidad de frecuencias por cada ref
ndisparos = length(data$ref)/fpdisparo #cuantos disparos hay cada uno con toda la franja de frecuencias
f = data$frequency
TS = data$TS

#grupos  [1:21], [22:41], [42:61], [62:81], [82:93], [94:113], [114:133], [134:153], [154:173], [174:193], 
#[194:210], [211:230], [231:250], [251:270], [271:290], [291:318].

minimos = c(min(f[1:21]), min(f[22:41]), min(f[42:61]), min(f[62:81]), min(f[82:93]), min(f[94:113], na.rm = T), 
            min(f[114:133]), min(f[134:153]), min(f[154:173]), min(f[174:193]),
            min(f[194:210]), min(f[211:230]), min(f[231:250]), min(f[251:270], na.rm = T), 
            min(f[271:290]), min(f[291:318]))

nintervalos = c(length(f[1:21]), length(f[22:41]), length(f[42:61]), length(f[62:81]), length(f[82:93]), 
                length(f[94:113]), length(f[114:133]), length(f[134:153]), length(f[154:173]), 
                length(f[174:193]), length(f[194:210]), length(f[211:230]), length(f[231:250]), 
                length(f[251:270]), length(f[271:290]), length(f[291:318]))

data = mutate(data, f.10 = rep(c(c(rep(minimos[1], each = nintervalos[1])), c(rep(minimos[2], each = nintervalos[2])), 
  c(rep(minimos[3], each = nintervalos[3])), c(rep(minimos[4], each = nintervalos[4])),
  c(rep(minimos[5], each = nintervalos[5])), c(rep(minimos[6], each = nintervalos[6])),
  c(rep(minimos[7], each = nintervalos[7])), c(rep(minimos[8], each = nintervalos[8])),
  c(rep(minimos[9], each = nintervalos[9])), c(rep(minimos[10], each = nintervalos[10])),
  c(rep(minimos[11], each = nintervalos[11])), c(rep(minimos[12], each = nintervalos[12])),
  c(rep(minimos[13], each = nintervalos[13])), c(rep(minimos[14], each = nintervalos[14])),
  c(rep(minimos[15], each = nintervalos[15])), c(rep(minimos[16], each = nintervalos[16]))), times = ndisparos))

#hasta ahora he creado la columna con frecuencias cada 10.000 Hz, ahora crear otra con la mediana de TS
#para poner mediana hacerlo como la parte de arriba

new_data = data %>%
  group_by(sp, set, ref, f.10) %>%
  summarise(medianTS = median(TS, na.rm = T))

as.character(new_data$set)

glimpse(new_data)

new_data %>% 
  mutate(set = as.factor(set)) %>%
  ggplot(map = aes(x = f.10, y = medianTS, color = sp)) + 
  geom_point() + #para hacer la grafica
  facet_wrap(~set, nrow = 1) +
  theme_bw()

write.table(new_data, file = "data10000.csv", sep = ";", dec = ".", row.names = F, col.names = T) 

widenew_data = pivot_wider(new_data, names_from = f.10, values_from = medianTS)

write.table(widenew_data, file = "wide_data1000.csv", sep = ";", dec = ".", row.names = F, col.names = T)

