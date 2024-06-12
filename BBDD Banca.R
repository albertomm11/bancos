library(readxl)
library(tidyverse)
library(writexl)

file_name <- "BBDD_Banca.xlsx"
datos_path <- "datos/"
file_data <- paste0(datos_path,file_name)

1. #Filtrar tabla datos agregados ###----

trimestrales_df = read_excel(file_data, sheet = "Agregado", col_names = TRUE, trim_ws = TRUE) 

trimestrales_df <- trimestrales_df %>% mutate(fecha = as.Date(Periodo))

anuales_df <- read_excel(file_data, sheet = "Anuales", col_names = TRUE, trim_ws = TRUE)

anuales_df <- anuales_df %>% mutate (fecha = as.Date(fecha))

trim_filtrado_df <- trimestrales_df %>% 
                     select(fecha, Entidad, `Créditos ESP`,`Depósitos ESP`,`Crédito total`,`Depósitos`) %>% #columnas
                     filter(fecha=="2023-12-31") 
                     # pivot_wider(names_from = "Entidad",
                     #             values_from = variable)
  
anual_filtrado_df <- anuales_df %>% 
                     select(fecha, Entidad, `SBB`,`IS_TOT_CASH_COM_DVD`,
                            `BS_TIER1_COMMON_EQUITY`,`Gravamen`) %>% #columnas
                     filter(fecha>="2023-12-31")

merged_data <- full_join(trim_filtrado_df, anual_filtrado_df, by = c("fecha", "Entidad"))

write_xlsx(list(filtrado=merged_data),
           path = "filtrado.xlsx")



# 1. #Filtrar tabla datos agregados con variable dinámica###----
# 
# agregado_df = read_excel(file_data, sheet = "Agregado", col_names = TRUE, trim_ws = TRUE) 
# agregado_df <- agregado_df %>% mutate(Periodo=as.Date(Periodo))
# anuales_df <- read_excel(file_data, sheet = "Anuales", col_names = TRUE, trim_ws = TRUE)
# 
# variable = "Margen bruto"
# 
# filtrado_df <- agregado_df %>% 
#   select(Entidad, Periodo, !!variable) %>% 
#   # filter(!is.na(.data[[variable]])) %>% 
#   
#   pivot_wider(names_from = "Entidad",
#               values_from = variable)



#Para poner en formato LONG - inactivo ----
# sbb = read_excel(file_data, sheet = "SBB", col_names = TRUE, trim_ws = TRUE)
# 
# sbb_long <- sbb %>%  pivot_longer(cols = -fecha,
#                          names_to="Banco",
#                          values_to="sbb")
# 
# divs_cash = read_excel(file_data, sheet = "divs", col_names = TRUE, trim_ws = TRUE)
# 
# divs_long <- divs_cash %>%  pivot_longer(cols = -fecha,
#                                   names_to="Banco",
#                                   values_to="Div_cash")
# 
# sbb_long <- sbb_long %>%
#   mutate(fecha = paste0("31-12-", fecha))
# 
# divs_long <- divs_long %>%
#   mutate(fecha = paste0("31-12-", fecha))
# 
# merged_data <- left_join(sbb_long, divs_long, by = c("fecha", "Banco")) %>% 
#                  filter(fecha>="31-12-2013")
# 
# write_xlsx(list(Anuales=merged_data),            
#            path = "anuales.xlsx")
