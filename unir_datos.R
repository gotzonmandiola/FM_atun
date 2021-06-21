# script para juntar los datos de broadband

library(tidyverse)

# load("BroadbandTS_all.RData")

# 1. juntamos los datos del lance 12

# datos70 = read.table(file = "datos/LANCE12_70_BroadbandTS.csv", header = T, 
#                      sep = "\t") 
# datos120 = read.table(file = "datos/LANCE12_120_BroadbandTS.csv", header = T, 
#                       sep = "\t") 
# datos200 = read.table(file = "datos/LANCE12_200_BroadbandTS.csv", header = T, 
                      # sep = "\t") 

#datuk kargetako, ikusi ia direkzinoie ondo dauen beti, eta tituloik dauken eta separadora zein dan.
datos38 = read.table(file = "datos/LANCE12_038_BroadbandTS.csv", header = T, 
                     sep = "\t") 
#datos38.1 barrixe sortzeko horrekin izenaz eta pivot.longer() bertikalin lotzeko, horizontalin jartzeko pivot.wider()
#daukeguzen frekuentzixa danak columna bihurtzeko ga esaten "freq" izenaz, eta honen balorik beste columna bat "TS" izenaz
datos38.l <- pivot_longer(data = datos38, cols = 7:dim(datos38)[2], names_to = "freq", values_to = "TS")

paste("datos/", lista[1], sep = "") #direktorixotik aukeratu nahioten artxibu aldatzen doie, horregaitxik,
                                    #lista bat eiot beien, eta listaku aukeratzeko esatzet.

lista <- dir("datos")               #honekin listi ze direktorioko artxibokin sortzi nahioten esan
str(lista)                          #
lista[2:7]                          #
length(lista)                       #


i = 1                               #
tini <- Sys.time()                  #hau bakarrik ordu esteko da (erabiligu jakitxeko comando bat 
                                    #computetan zemat denpora tardetan daben)
#Creamos un bucle para no tener que hacer la accion mil veces 
for (i in 1:length(lista)) {        #le decimos que la accion es desde el primero de la lista hasta el final
#primero decirle a que queremos que le haga el bucle
  narchivo <- lista[i]              #creamos narchivo con la lista que vamos creando
  rutaarchivo = paste("datos/", narchivo, sep = "") #creamos rutaarchivo porque la ruta tiene que ir cambiando 
                                                    #cada vez que la accion termine y vuelva a empezar,
                                                    #aqui, le decimos que la ruta va dependiendo de la lista (narchivo)
#ahora le decimos que queremos que le haga a los archivos 
  temp = read.table(file = rutaarchivo, header = T, sep = "\t") #que vaya leyendo archivos de la ruta
  temp <- temp %>%  #le anadimos cascada para que cada vez que empieze a leer le vaya ejecutando lo siguiente:
    pivot_longer(cols = 7:dim(temp)[2], names_to = "freq", values_to = "TS") %>% #que los vaya anadiendo verticalmente
    mutate(                                                 #para meterle dos columnas mas
      set = str_sub(narchivo, start = 6, end = 7),          #la pesca, que el nombre del archivo eran los numeros 6 y7 
      ctral.freq = str_sub(narchivo, start = 9, end = 11)   #la frecuencia central
    ) #le decimos que al primero le añada los nombre de columnas, pero a las demas no, para solo tenerlos arriba
  if (i == 1) { #
    write.table(x = temp, file = "data/BroadbandTS_all.csv", append = F, row.names = F,
                col.names = T, sep = ";", dec = ".")  } #append=F para que no borre lo que ya tenemos al principio
  else {
    write.table(x = temp, file = "data/BroadbandTS_all.csv", append = T, row.names = F,
                col.names = F, sep = ";", dec = ".") #append=T para que al pegar no vaya borrando lo que tenemos hasta ahora
  }
      #para ver como va el proceso le decimos que mientras compute nos vaya imprimiendo 
      #nos imprime el numero del bucle (i), el numero de la lista que va pegando, y cuanto tiempo tarda
  print(paste(i, length(lista), Sys.time() -tini, sep = " / ")) 
}

# Abrimos archivo y guardamos versiones reducidas
all <- read.table(file = "data/BroadbandTS_all.csv", header = T, sep = ";")
head(all)
str(all)
glimpse(all)

table(all$freq)

all <- all %>% 
  mutate(
    sp = case_when(
      set == 12 ~ "skj", 
      set == 23 ~ "skj", 
      set > 46 & set < 60 ~ "yft", 
      T ~ "mix"
    )
  )

table(all$sp)

# creamos una version reducida del archivo para poder hacer pruebas
all.redux <- all %>% 
  group_by(set, freq) %>% 
  sample_n(size = 100)

all.redux2 <- all %>% 
  group_by(set, freq) %>% 
  sample_n(size = 500)


table(all.redux2$ctral.freq)

write.table(all, file = "data/allTS_sp.csv", row.names = F, sep = ";", dec = ".", col.names = T)


write.table(all.redux, file = "data/all_redux.csv", row.names = F, sep = ";", dec = ".", col.names = T)


write.table(all.redux2, file = "data/all_redux2.csv", row.names = F, sep = ";", dec = ".", col.names = T)


all.redux %>% 
  mutate(
    freq = stringr::str_sub(freq, start = 2, 7), 
    freq = as.numeric(freq)
  ) %>% 
  group_by(set, freq) %>% 
  sample_frac(size = 0.01) %>% 
  ungroup %>% 
  mutate(set = as.factor(set)) %>% 
  ggplot(aes(x = freq/1000, y = TS, color = set)) +
  geom_point() +
  # geom_point(aes(x = freq, y = TS), color = "blue") +
  facet_wrap(~ set) +
  xlab("Frequency (kHz)") +
  ylab("TS (dB)") +
  geom_smooth(color = "black", method = "lm") +
  theme_bw()
ggsave(filename = "figs/scatterplot by sets.png", device = "png", width = 18, height = 8, units = "cm", dpi = "print")






#ejemplo de cascada
datos38.l %>% 
  mutate(
    set = 12, 
    ctral.freq = 38000
  ) %>% 
  group_by(set, freq) %>% 
  summarise(
    mean.TS = mean(TS, na.rm = T), 
    sd.TS = sd(TS)
  ) %>% 
  ggplot(aes(x = freq, y = mean.TS, color)) +
  geom_point()
  

str(datos)
