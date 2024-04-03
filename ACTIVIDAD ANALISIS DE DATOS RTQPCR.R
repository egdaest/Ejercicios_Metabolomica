
#Creador: Egda Estrada
#Análisis de datos de tiempo real

install.packages('pacman') #pacman sirve para llamar e instalar otros paquetes


library(pacman) #library es para ejecutar
p_load('readr', #para llamar a las bases de datos
       'dplyr') #para facilitar el manejo de datos

##############################################

#llamar la base de datos

datos_pcr <- read_csv(file = 'https://raw.githubusercontent.com/ManuelLaraMVZ/Transcript-mica/main/Genes.csv')

head(datos_pcr)
##########################################################

#Obtención de los genes referencia y de interés

actina <- datos_pcr%>% 
  slice(1)

actina

genes_interes <- datos_pcr %>% 
  slice(-1)

genes_interes

###############################################
# Promediar los controles y los tratamientos

promedio_actina <- actina %>% 
  mutate(Mea_Cx=(C1+C2+C3)/3)%>% 

promedio_actina
