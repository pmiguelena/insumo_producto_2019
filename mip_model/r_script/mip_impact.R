### MIP MODEL 2019
### MATRIZ IO INDUSTRIA X INDUSTRIA REALIZADA EN BASE A LOS COU 2019
### DOCUMENTO METODOLOGICO EN PROCESO
## PROF GABRIEL MICHELENA VERSION JUNIO 2022

rm(list = ls()) # limpiar la memoria

# opciones iniciales
options(warn = 1)
options(scipen = 999)
options(max.print = 1000000)
options(java.parameters = "-Xmx2000m")
options(digits = 3)

# funcion concatenar
"%+%" <- function(x,y) paste(x, y, sep = "")

# Establecer directorio de trabajo

# Directorios principales
main     <- getwd() %+% '/' 
main     <- gsub('r_script/', '', main)
data_in  <- main %+% 'data_in/'
data_out <- main %+% 'data_out/'
script   <- main %+% 'r_script/'


# cargo las librerias que necesito para trabajar
source(script  %+%  'librerias.R')

#-----------------------------------------------------------------------------------------------#
# CARGA DE DATOS
#-----------------------------------------------------------------------------------------------#

# Establecer directorio de trabajo
tbl_act <- read_xlsx(data_in %+% "mip_2019.xlsx", sheet = "desc")
q_act <- nrow(tbl_act)

str(tbl_act)
class(tbl_act)
mode(tbl_act)
tbl_act[1:2,]

# SAM 2018 millones de pesos corrientes
tbl_mip <-  as.data.frame(read_xlsx(data_in %+% "mip_2019.xlsx", sheet = "mat_pb"))
n_cell <- nrow(tbl_mip)  # nro de filas menos el total
rownames(tbl_mip) <- (tbl_mip[,1])
tbl_mip <-  (tbl_mip[,-1])   # le elimino la primer columna

tbl_mip[1:2,1:3]

dim(tbl_mip)
class(tbl_mip) # data.frame

# la convierto a matriz
Z <- as.matrix(tbl_mip[1:(q_act), 1:(q_act)]) # convertir el data.frame en una matriz
Z[1:10,1:3]

#-----------------------------------------------------------------------------------------------#
# ALGUNAS VARIABLES DE INTERES 
#-----------------------------------------------------------------------------------------------#
VBP <-  tbl_mip[c("vbp"), 1:q_act]
VA <- tbl_mip[c("vab"), 1:q_act]
VAsobreVBP <- VA / VBP

# aca vamos a tomar las intermedias
M_int <- tbl_mip[c("impo"), 1:q_act]
MsobreVBP <- M_int / VBP
MsobreVBP

# primero levanto el vector de importaciones totales (para estimar absor)
impo <- read_xlsx(data_in %+% "mip_2019.xlsx", sheet = "aux_vectors")
impo <- as.matrix(impo[,'m']) 
impo

# primero levanto el vector de importaciones totales (para estimar absor)
expo <- read_xlsx(data_in %+% "mip_2019.xlsx", sheet = "aux_vectors")
expo <- as.matrix(expo[,'ex']) 
expo


# primero levanto el vector de recaudacion
reca <- read_xlsx(data_in %+% "mip_2019.xlsx", sheet = "aux_vectors")
reca <- as.matrix(reca[,c('tax')]) 
reca

# levanto el vector de demanda total
dem <-  read_xlsx(data_in %+% "mip_2019.xlsx", sheet = "aux_vectors")
dem <- as.matrix(dem[,c('ch','cp','ex','inv','ve')]) 
dem

 

#==============================================================================#
# INVIRTIENDO LA MATRIZ DE COEF 
#==============================================================================#
# vector de totales
tot <- tbl_mip[c("vbp"), 1:q_act]
tot <- as.matrix(tot)  

# armo una matriz diagonal
iden <- diag(q_act)

# matriz inversa de totales
inv_tot <- 1/tot

#pregunta a R que elementos del vector con infinito y en esos casos pone cero
inv_tot[is.infinite(inv_tot)] <- 0  

# armo la matriz diagonal
diag_tot <- diag(c(inv_tot)) 

# matriz de coeficientes
A <- Z %*% diag_tot  # ahora si hago la multiplicación
A[is.nan(A)] <- 0
colnames(A) <- colnames(Z)

# 3: finalmente invierto la matriz
inter <- iden - A
B <- solve(iden - A) # finalmente obtengo la inversa sam
B
 
#==============================================================================#
# SIMULACIONES ===> DEMANDA
#==============================================================================#
# recordatorio => la sam y la cou estan expresadas en miles de pesos (2019)
# el TCN implicito del bdp es 48.25
#==================================
## shocklist
shock.list <- read_xlsx(data_in %+% "mip_2019.xlsx", sheet = "shock_list")
shock.list


#==================================
#==================================
# ARMO LOS SHOCKS
  
#  ahora armo el vector
f.chg <-  as.matrix((shock.list[,2])) 

# calculo ratio fchange con respecto al PBI
sum(f.chg)/sum(VA)

# primero creo un vector de oferta domestica
domsup <- (t(as.matrix(VBP) )- expo)/ (t(as.matrix(VBP)) - expo + impo )

# X multiplico por contenido nacional
f.chg.nac <- f.chg * domsup 

# agrego nombre a las filas 
rownames(f.chg.nac) <- rownames(A)
#==================================
# RESUELVO

# multiplico por la inversa SAM
x.chg <- B %*% as.matrix(f.chg.nac)
sum(x.chg)/sum(f.chg)

# tengo que reducir la dimension de x.chg para poder estimar el resto
x.chg <-  x.chg[1:nrow(A),]

# calculo el vector de cambio en el emplo
#=================================================
# primero levanto el vector => miles de puestos
labor <- read_xlsx(data_in %+% "mip_2019.xlsx", sheet = "imo")
labor

# estimo el coeficiente labor-output
# puestos por miles de $ de producto
LsobreVBP <- t(t(labor[,2])%*% diag(c(1/VBP)))
LsobreVBP

# estimo el cambio en el empleo : Elasticidad = 0.7
lab.chg <- t( t(LsobreVBP) %*% diag(c(x.chg)))  
lab.chg
sum(lab.chg)



# calculo el vector de cambio en las importaciones
#=================================================
# estimo el cambio en las impos intermedias + finales
impo.chg <- diag(c(MsobreVBP)) %*% x.chg + f.chg * (1-domsup) 
colnames(impo.chg) <- 'impo'
impo.chg

# en millones de dolares
sum(impo.chg)/(1000*48.25)


# calculo el vector de cambio en la recaudacion
#=================================================
# estimo el coeficiente reca-output
# paso importante, las importaciones pagan iva e indirectos
# tengo que corregir el ratio
 
# armo tres vectores
# tax iva
TAXsobreVBP <- t(t(reca)%*% diag(c(1/VBP)))
TAXsobreVBP
 

tax.chg <- t( t(TAXsobreVBP) %*% diag(c(x.chg)))
tax.chg

# ahora si junto todo en un solo vector
reca.chg <- tax.chg
colnames(reca.chg) <-  c('tax')
 
# calculo el vector de cambio en el valor agregado
#=================================================
va.chg <- diag(c(VAsobreVBP)) %*% x.chg
colnames(va.chg) <- 'vab'
va.chg

#==============================================================================#
# TABLAS RESULTADOS
#==============================================================================#
# TABLA GENERAL = CHG ABSOLUTO
#================= 
# armo tabla resultados generales
gral_res <- cbind(x.chg,va.chg,reca.chg,impo.chg)
gral_base <- cbind(t(VBP),t(VA),reca,impo) 

# agrego resultados y armo tabla final
# en millones de dolares
scale2 <- function(x, na.rm = FALSE) (x /(1000*48.25))  # lo expreso en dolares

gral_base <-  as_tibble(gral_base)   %>%
              summarise_all(sum)  %>%
              gather(var,base,1:4) %>%
              select(-var)

gral_res <-  as_tibble(gral_res)   %>%
             summarise_all(sum) %>% 
             gather(var,abs.chg,1:4) %>%
             bind_cols(gral_base) %>%
             mutate_at(vars(base,abs.chg), scale2) %>% 
             mutate(pc.chg = 100*abs.chg/base) %>% 
             mutate(var = c('produccion','valor_agregado','recaudacion',
                            'importaciones'))  %>%
             select(var,base,abs.chg,pc.chg) %>%
             mutate(order = seq(1:n()))  
       
gral_res
  

# grafico 
ggplot(data=gral_res, aes(reorder(var, order),abs.chg, fill = var)) +
  geom_bar(stat="identity") +
  theme(legend.position = "none") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(x=" ", y="millones USD") +
  ggtitle("Cambio absoluto (millones USD)")
ggsave(data_out %+% 'gral_res_abs.png')


# TABLA GENERAL = CHG PORCENTUAL
#================= 

# grafico 
ggplot(data=gral_res, aes(reorder(var, order),pc.chg, fill = var)) +
  geom_bar(stat="identity") +
  theme(legend.position = "none") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_y_continuous(labels = comma) +
  labs(x=" ", y="%") +
  ggtitle("Cambio porcentual (En %)")
ggsave(data_out %+% 'gral_res_pc.png')


# exporto a excel
  write.xlsx(as.data.frame(gral_res), data_out %+% 
               "resultados.xlsx",
               sheetName = 'general' ) 
 

#====================================================
# TABLA SECTORIAL
#====================================================
# creo una tabla final y exporto los resultados 
sec_res <- cbind(x.chg, va.chg, rowSums(lab.chg))

# agrego totales
# le agrego una  una fila con los totales 
sec_res <- rbind(sec_res, colSums(sec_res))
# corrijo el nombre
rownames(sec_res)[NROW(sec_res)] <- 'TOTAL'
colnames(sec_res) <- c('chg.vbp', 'chg.vab', 'chg.lab')

# imprimo la tabla
sec_res


#====================================================
# GRAFICO 1: cambio en el empleo top 10 sectores
#====================================================
# ordeno de mayor a menor y dejo top ten
empleo <- cbind(as.data.frame(lab.chg), tbl_act)  %>%
  arrange(-lab.chg) %>%
  mutate(id = seq(1:n())) %>%
  mutate(desc_agg = ifelse(id >=10, "Resto", desc_agg)) %>%
  group_by(desc_agg)   %>%
  summarise_if(is.numeric, funs(sum))  %>%
  ungroup()  %>%
  dplyr::select(desc_agg, total) %>%
  arrange(-total) 

# grafico  
ggplot(data=empleo, aes(reorder(desc_agg, total),total, fill = desc_agg)) +
  geom_bar(stat="identity") +
  coord_flip() +
  theme(legend.position = "none") + 
  scale_y_continuous(labels = comma) +
  labs(x=" ", y="puestos") +
  ggtitle("Cambio en el Empleo (puestos)")
ggsave(data_out %+% "empleo_res.png")


# GRAFICO 2: cambio en el vbp top 10 sectores
#================================
# ordeno de mayor a menor y dejo top ten
output <- cbind(x.chg, tbl_act)  %>%
  arrange(-x.chg) %>%
  mutate(id = seq(x.chg)) %>%
  mutate(desc_agg = ifelse(id >=10, "Resto", desc_agg)) %>%
  group_by(desc_agg)   %>%
  summarise(x.chg = sum(x.chg))  %>%
  mutate_at(vars(x.chg), scale2)    %>%  # convierto a dolares
  ungroup() %>%
  arrange(-x.chg)


# grafico  
ggplot(data=output, aes(reorder(desc_agg, x.chg),x.chg, fill = desc_agg)) +
  geom_bar(stat="identity") +
  coord_flip() +
  theme(legend.position = "none") + 
  scale_y_continuous(labels = comma) +
  labs(x=" ", y="millones USD") +
  ggtitle("Cambio en VBP (millones de USD)")
ggsave(data_out %+% "output_res.png")

 
# GRAFICO 3: cambio en el vab top 10 sectores
#================================
# ordeno de mayor a menor y dejo top ten
valueadd <- cbind(va.chg, tbl_act)  %>%
  arrange(-vab) %>%
  mutate(id = seq(vab)) %>%
  mutate(desc_agg = ifelse(id >=10, "Resto", desc_agg)) %>%
  group_by(desc_agg)   %>%
  summarise(vab = sum(vab))  %>%
  mutate_at(vars(vab), scale2)    %>%  # convierto a dolares
  ungroup()


# grafico 
ggplot(data=valueadd, aes(reorder(desc_agg, vab),vab, fill = desc_agg)) +
  geom_bar(stat="identity") +
  coord_flip() +
  theme(legend.position = "none") + 
  scale_y_continuous(labels = comma) +
  labs(x=" ", y="millones USD") +
  ggtitle("Cambio en VAB (millones de USD)")

ggsave(data_out %+% "valueadd_res.png")
 


