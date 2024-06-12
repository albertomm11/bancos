library(readxl)
library(tidyverse)
library(writexl)

#0.Abrir ficheros y guardarlo como XlSX----

# Ruta a la carpeta de datos y fecha de los datos
datos_path <- "datos/"
fecha_datos <- "2022-12-31" #cambiar según corresponda
fecha_cols <- "dic22"

new_col_names <- c("codigo_entidad", #esta siempre igual
                   paste0("total_activo_",fecha_cols))

#ficheros necesarios en datos: datos_entidades.xlsx // pyg_consolidada.xlsx // balance_consolidado.xlsx // pyg_invididual.xlsx // balance_invididual.xlsx
#Características entidades
datos_entidades_df <- read_excel(paste0(datos_path, "datos_entidades.xlsx"), col_names=TRUE, trim_ws=TRUE)

#1.PYG CONSOLIDADA----
# Lee el archivo Excel, omitiendo las dos primeras filas y usando la primera fila como nombres de columna
pyg_consolidada_raw <- read_excel(paste0(datos_path, "pyg_consolidada.xlsx"), skip = 2, col_names = TRUE, trim_ws = TRUE)

# Elimina las columnas que contienen "Subgrupo"
pyg_consolidada_df <- pyg_consolidada_raw %>% select(-contains("Subgrupo"))

    new_col_names <- substr(names(pyg_consolidada_df)[-1], 1, 4) #extramos código de nombre entidad y lo hacemos nuevo nombre columnas
    new_col_names <- c("Variables", new_col_names) #Variables porque primera columna no tiene código
    names(pyg_consolidada_df) <- new_col_names #asignamos nuevos nombres (Variables y códigos)
    
##1.1 Elegir variables y renombrar columnas----
#Seleccionamos variables relevantes, las renombramos y transponemos el data frama

partidas_pyg <- c(1,5,7,10,11,47,53) #vector de filas a mantener

pyg_consolidada_df <- pyg_consolidada_df [partidas_pyg, ] %>% .[ ,-1] %>% t() %>% as.data.frame()   #filtramos las filas que queremos, quitamos la primera columna de nombres (para evitar perder los valores númericos al transponer)

# Convierte los nombres de fila a la primera columna (códigos entidad) y renombra las columnas
pyg_consolidada_df <- cbind(rownames(pyg_consolidada_df), pyg_consolidada_df) 

     new_col_names <- c("codigo_entidad", #esta siempre igual
                        "ing_intereses",
                        "gasto_intereses",
                        "margen_intereses",
                        "ing_comisiones",
                        "gasto_comisiones",
                        "rdo_pretax_actividades_continuadas",
                        "rdo_ejercicio")
     
     colnames(pyg_consolidada_df) <- new_col_names # Renombramos las columnas

#Convierte todas las columnas existentes a numeric (ignoring errors)
     pyg_consolidada_df <- pyg_consolidada_df %>%
       mutate(across(everything(), as.numeric, na.rm = TRUE))   # Convert columns to numeric
       
#Crear columna para etiquetar el origen
   pyg_consolidada_df <- pyg_consolidada_df %>%
     mutate(origen = "Consolidado") %>%
     mutate(fecha= as.Date(fecha_datos)) %>% 
     select(origen,fecha, everything())  #reordena las columnas con select
   

# Divide las columnas desde la cuarta en adelante por 1.000.000 
     pyg_consolidada_df[, 4:ncol(pyg_consolidada_df)] <- 
     pyg_consolidada_df[, 4:ncol(pyg_consolidada_df)] / 1000000


#2.PYG INDIVIDUAL----
# Lee el archivo Excel, omitiendo las dos primeras filas y usando la primera fila como nombres de columna
pyg_individual_df <- read_excel(paste0(datos_path, "pyg_individual.xlsx"), skip = 2, col_names = TRUE, trim_ws = TRUE)

new_col_names <- substr(names(pyg_individual_df)[-1], 1, 4) #extramos código de nombre entidad y lo hacemos nuevo nombre columnas
new_col_names <- c("Variables", new_col_names) #Variables porque primera columna no tiene código
names(pyg_individual_df) <- new_col_names #asignamos nuevos nombres (Variables y códigos)

##2.1 Elegir variables y renombrar columnas----
#Seleccionamos variables relevantes, las renombramos y transponemos el data frama

partidas_pyg <- c(1,5,7,9,10,45,49) #vector de filas a mantener

pyg_individual_df <- pyg_individual_df [partidas_pyg, ] %>% .[ ,-1] %>% t() %>% as.data.frame()   #filtramos las filas que queremos, quitamos la primera columna de nombres (para evitar perder los valores númericos al transponer)

# Convierte los nombres de fila a la primera columna
pyg_individual_df <- cbind(rownames(pyg_individual_df), pyg_individual_df) 

new_col_names <- c("codigo_entidad", #esta siempre igual
                   "ing_intereses",
                   "gasto_intereses",
                   "margen_intereses",
                   "ing_comisiones",
                   "gasto_comisiones",
                   "rdo_pretax_actividades_continuadas",
                   "rdo_ejercicio")

colnames(pyg_individual_df) <- new_col_names # Renombramos las columnas

#Convierte todas las columnas existentes a numeric (ignoring errors)
pyg_individual_df <- pyg_individual_df %>%
  mutate(across(everything(), as.numeric, na.rm = TRUE)) # Handles non-numeric values

#Crear columna para etiquetar el origen
pyg_individual_df <- pyg_individual_df %>%
  mutate(origen = "Individual") %>%
  mutate(fecha= as.Date(fecha_datos)) %>% 
  select(origen,fecha, everything())  #reordena las columnas con select


# Divide las columnas desde la tercera en adelante por 1.000.000 
  pyg_individual_df[, 4:ncol(pyg_individual_df)] <- 
  pyg_individual_df[, 4:ncol(pyg_individual_df)] / 1000000


#3.Combinar ambos----
filtered_pyg_df <- anti_join(pyg_individual_df, pyg_consolidada_df, by = "codigo_entidad")  #anti_join keeps rows in left dataframe that are not present in right dataframe based on the "Código entidad" column.

pyg_combinado_df <- rbind(pyg_consolidada_df,filtered_pyg_df) #combinanos df filtrado de invidual con consolidado 

#4.Balance----
##4.1.Balance CONSOLIDADO----
# Lee el archivo Excel, omitiendo las dos primeras filas y usando la primera fila como nombres de columna
balance_consolidado_raw <- read_excel(paste0(datos_path, "balance_consolidado.xlsx"), skip = 2, col_names = TRUE, trim_ws = TRUE)

# Elimina las columnas que contienen "Subgrupo"
balance_consolidado_df <- balance_consolidado_raw %>% select(-contains("Subgrupo"))

new_col_names <- substr(names(balance_consolidado_df)[-1], 1, 4) #extramos código de nombre entidad y lo hacemos nuevo nombre columnas
new_col_names <- c("Variables", new_col_names) #Variables porque primera columna no tiene código
names(balance_consolidado_df) <- new_col_names #asignamos nuevos nombres (Variables y códigos)

##4.1.1 Elegir variables y renombrar columnas----
#Seleccionamos variables relevantes, las renombramos y transponemos el data frama

partidas_balance <- c(66) #vector de filas a mantener

balance_consolidado_df <- balance_consolidado_df [partidas_balance, ] %>% .[ ,-1] %>% t() %>% as.data.frame()   #filtramos las filas que queremos, quitamos la primera columna de nombres (para evitar perder los valores númericos al transponer)

# Convierte los nombres de fila a la primera columna
balance_consolidado_df <- cbind(rownames(balance_consolidado_df), balance_consolidado_df) 

new_col_names <- c("codigo_entidad", #esta siempre igual
                   paste0("total_activo_",fecha_cols))
                  
colnames(balance_consolidado_df) <- new_col_names # Renombramos las columnas

#Convierte todas las columnas existentes a numeric (ignoring errors)
balance_consolidado_df <- balance_consolidado_df %>%
  mutate(across(everything(), as.numeric, na.rm = TRUE))  # Handles non-numeric values

#Crear columna para etiquetar el origen
balance_consolidado_df <- balance_consolidado_df %>%
  mutate(origen = "Consolidado") %>%
  mutate(fecha= as.Date(fecha_datos)) %>% 
  select(origen,fecha, everything())  #reordena las columnas con select


# Divide las columnas desde la cuarta en adelante por 1.000.000 
balance_consolidado_df[, 4:ncol(balance_consolidado_df)] <- 
  balance_consolidado_df[, 4:ncol(balance_consolidado_df)] / 1000000

#4.2.Balance INDIVIDUAL----
# Lee el archivo Excel, omitiendo las dos primeras filas y usando la primera fila como nombres de columna
balance_individual_raw <- read_excel(paste0(datos_path, "balance_individual.xlsx"), skip = 2, col_names = TRUE, trim_ws = TRUE)

# Elimina las columnas que contienen "Subgrupo"
balance_individual_df <- balance_individual_raw %>% select(-contains("Subgrupo"))

new_col_names <- substr(names(balance_individual_df)[-1], 1, 4) #extramos código de nombre entidad y lo hacemos nuevo nombre columnas
new_col_names <- c("Variables", new_col_names) #Variables porque primera columna no tiene código
names(balance_individual_df) <- new_col_names #asignamos nuevos nombres (Variables y códigos)

##4.2.1 Elegir variables y renombrar columnas----
#Seleccionamos variables relevantes, las renombramos y transponemos el data frama

partidas_balance <- c(66) #vector de filas a mantener

balance_individual_df <- balance_individual_df [partidas_balance, ] %>% .[ ,-1] %>% t() %>% as.data.frame()   #filtramos las filas que queremos, quitamos la primera columna de nombres (para evitar perder los valores númericos al transponer)

# Convierte los nombres de fila a la primera columna
balance_individual_df <- cbind(rownames(balance_individual_df), balance_individual_df) 

new_col_names <- c("codigo_entidad", #esta siempre igual
                   paste0("total_activo_",fecha_cols))

colnames(balance_individual_df) <- new_col_names # Renombramos las columnas

#Convierte todas las columnas existentes a numeric (ignoring errors)
balance_individual_df <- balance_individual_df %>%
  mutate(across(everything(), as.numeric, na.rm = TRUE))  # Handles non-numeric values

#Crear columna para etiquetar el origen
balance_individual_df <- balance_individual_df %>%
  mutate(origen = "Individual") %>%
  mutate(fecha= as.Date(fecha_datos)) %>% 
  select(origen,fecha, everything())  #reordena las columnas con select

# Divide las columnas desde la tercera en adelante por 1.000.000 
balance_individual_df[, 4:ncol(balance_individual_df)] <- 
  balance_individual_df[, 4:ncol(balance_individual_df)] / 1000000

##4.2.2.Combinar ambos----
filtered_balance_df <- anti_join(balance_individual_df, balance_consolidado_df, by = "codigo_entidad")  #anti_join keeps rows in left dataframe that are not present in right dataframe based on the "Código entidad" column.
balance_combinado_df <- rbind(balance_consolidado_df,filtered_balance_df)

#5. Combinar ambos y con datos entidades----  

#limpiar entorno
# Data frames to keep (replace with your actual names)
data_frames_to_keep <- c("pyg_combinado_df", "balance_combinado_df", "datos_entidades_df","fecha_cols")

# Remove all objects except those in data_frames_to_keep
objects_to_remove <- ls()[!ls() %in% data_frames_to_keep] #ls()lista los objetos del entorno --> selecciona objetos que NO estén en "data frames to keep"
rm(list = objects_to_remove)

combined_df <- inner_join(pyg_combinado_df, balance_combinado_df, 
                          by = c("origen", "fecha", "codigo_entidad")) %>% 
               inner_join(.,datos_entidades_df,by ="codigo_entidad") #el punto utiliza el resultado de la operación anterior
tabla_final_df <- combined_df %>% 
                  mutate(comisiones_netas=ing_comisiones-gasto_comisiones,
                         base_imponible=margen_intereses+comisiones_netas) %>% 
                  select(codigo_entidad,origen,fecha,Entidad,
                        "SI/LSI","Sujeto obligado gravamen temporal",
                        "Filial/SE/Filial extranjera",
                        paste0("total_activo_",fecha_cols),
                        margen_intereses,
                        comisiones_netas,
                        base_imponible,
                        rdo_pretax_actividades_continuadas,
                        rdo_ejercicio) 

write_xlsx(list(principal=tabla_final_df),
           path = "datos_dic_22.xlsx")

