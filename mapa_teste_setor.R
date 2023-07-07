library(flexdashboard)
library(plotly)
library(ggplot2)
library(sf)
library(readr)
library(janitor)
library(rgdal)
library(readxl)
library(writexl)
library(tidyverse)
library(geobr)


rm(list = ls())

base_rio_data = read_csv('setorescensitariosRio.csv') # %>% filter(objectid %in% c(6972, 7463))

# Juntando dados

rj = read_census_tract(code_tract = 33)

rj$code_tract = as.double(rj$code_tract)

base_geral = left_join(base_rio_data, rj, by = c("id" = "code_tract"))

ggplot()+
  geom_sf(data = base_geral, aes(geometry = geom))



populacaoSetor = read_excel('populacaoSetor.xls')


# Juntando as bases tratadas
base_geral_pop = left_join(base_geral, populacaoSetor, by = c("id" = "Cod_setor"))


# Criando o indicador de densidade
# Esse codigo precisa ser melhorado
base_geral_pop$densidade =base_geral_pop$V001 / base_geral_pop$st_lengthshape



# Mapa

mapa_densidade1 = ggplot() +
  geom_sf(data = base_geral_pop, aes(geometry = geom, fill = densidade)) +
  labs(title = "Mapa Teste", color = "Retorno de Linha", subtitle = 'Fonte: Data.rio') +
  theme_void() +
  theme(
    plot.title = element_text(family = "Arial", size = 18, face = "bold", hjust = 0.5),  # Title configuration
    plot.subtitle = element_text(family = "Arial", size = 14, face = "italic", hjust = 0.5),  # Subtitle configuration
    axis.title = element_text(family = "Arial", size = 12, face = "bold"),  # Axis title configuration
    axis.text = element_text(family = "Arial", size = 10),  # Axis text configuration
    legend.title = element_text(family = "Arial", size = 12, face = "bold"),  # Legend title configuration
    legend.text = element_text(family = "Arial", size = 10)
  )+
  scale_fill_gradientn(colors = c("green", "orange", "red"))

# Tornando ele interativo
mapa_densidade_int = ggplotly(mapa_densidade1) %>%
  layout(line = list(width = 0.4))
# Plotando o mapa

mapa_densidade_int