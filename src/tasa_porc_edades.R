pacman::p_load(tidyverse, tidyquant)



db_sis <- read_csv("~/Downloads/sisver_public.csv")
fa <- db_sis%>% 
  filter(tipacien=="HOSPITALIZADO") %>% 
  filter(resdefin!="NEGATIVO" & resdefin!="NO ADECUADO" & resdefin!="RECHAZADA") %>% 
  mutate(fechreg=ymd(fechreg), 
         rango=if_else(edad<=4, 1, 
               if_else(edad>4 & edad<=14, 2, 
               if_else(edad>14 & edad<=59, 3, 
               if_else(edad>59 & edad <= 84, 4, 5)))),
         rango_fac=factor(x = rango, labels = c("0 - 4", "5 - 14", 
                                                "15 - 59", "60 - 84", "Más de 85"))) %>% 
  filter(fechreg>="2020-11-01" & fechreg<max(fechreg)-2) %>% 
  group_by(fechreg, rango_fac) %>% 
  summarise(total=n()) %>% 
  group_by(rango_fac) %>% 
  mutate(maximo=max(total)) %>% 
  ungroup() %>% 
  mutate(porc=total/maximo)


fa %>%  
  ggplot( ) +
  # geom_line(aes(x = fechreg, y = total, color = rango), size = 1.1, alpha = .2) +
  # geom_point(aes(x = fechreg, y = total, color = rango),size = 1.1, alpha = .3) +
  geom_ma(aes(x = fechreg, y = porc, color = rango_fac), size = 0.9, n = 7,
          linetype = 1) +
  scale_color_manual(values=c("#045a8d", "#2b8cbe", "#74a9cf", "#78c679", "#006d2c"))+
  labs(title="Tendencia de hospitalizaciones por edades",
       subtitle = "En CDMX, porcentaje con respecto al pico máximo", 
       color="edades",
       caption="Fuente: Covid-19 SINAVE Ciudad de México")+
  ggthemes::theme_fivethirtyeight()

ggsave(filename  = "~/Downloads/porcentaje_edades.png", width = 9,height = 5,units = "in")

#74a9cf
#2b8cbe
#045a8d

pob <- read_csv("~/Downloads/INEGI_exporta_5_1_2022_16_53_30.csv") %>% 
       na.omit() %>% 
       janitor::clean_names() %>% 
       gather(rango, total, de_0_a_4_a_os:x85_a_os_y_m_s) %>% 
       select(rango, total) 

pob[1, 1] <- "0 - 4"
pob[2:3, 1] <- "5 - 14"
pob[4:12, 1] <- "15 - 59"
pob[13:17, 1] <- "60 - 84"
pob[18, 1] <- "Más de 85"


pob <- pob %>% 
  group_by(rango) %>% 
  summarise(pob=sum(total, na.rm=T)) %>% 
  mutate(rango_fac=if_else(rango=="0 - 4", 1,
                   if_else(rango=="5 - 14", 2, 
                   if_else(rango=="15 - 59", 3, 
                   if_else(rango=="60 - 84", 4,5)))), 
         rango_fac=factor(x = rango, labels = c("0 - 4", "5 - 14", 
                                                "15 - 59", "60 - 84", "Más de 85"))) %>% 
        select(-rango)

rate <- fa %>% 
      left_join(pob) %>% 
      mutate(tasa=round((total/pob)*100000, 2)) %>% 
  filter(fechreg>="2021-09-01") 




rate %>%  
  ggplot( ) +
  # geom_line(aes(x = fechreg, y = total, color = rango), size = 1.1, alpha = .2) +
  # geom_point(aes(x = fechreg, y = total, color = rango),size = 1.1, alpha = .3) +
  geom_ma(aes(x = fechreg, y = tasa, color = rango_fac), size = 0.9, n = 7,
          linetype = 1) +
  scale_color_manual(values=c("#045a8d", "#2b8cbe", "#74a9cf", "#78c679", "#006d2c"))+
  labs(title="Tendencia de hospitalizaciones por edades",
       subtitle = "En CDMX, tasa por 100mil habitantes", 
       color="edades", 
       caption="Fuente: Covid-19 SINAVE Ciudad de México \nCenso 2020 (INEGI)")+
  ggthemes::theme_fivethirtyeight()
ggsave(filename  = "~/Downloads/tasa_edades.png", width = 9,height = 5,units = "in")
