

pacman::p_load(tidyverse, foreign, 
               janitor, readxl,
               ggrepel, tidyquant)



db_sis <- read_csv("~/Downloads/sisver_public.csv")
fa <- db_sis%>% 
          filter(tipacien=="HOSPITALIZADO") %>% 
          filter(resdefin!="NEGATIVO" & resdefin!="NO ADECUADO" & resdefin!="RECHAZADA") %>% 
          mutate(fechreg=ymd(fechreg)) %>% 
          filter(fechreg>="2021-10-15" & fechreg<max(fechreg)-2) %>% 
          group_by(fechreg, vacunado) %>% 
          summarise(total=n()) %>% 
          filter(!is.na(vacunado))


fa %>%  
  ggplot( ) +
  # geom_line(aes(x = fechreg, y = total, color = vacunado), size = 1.1, alpha = .2) +
  # geom_point(aes(x = fechreg, y = total, color = vacunado),size = 1.1, alpha = .3) +
  geom_ma(aes(x = fechreg, y = total, color = vacunado), size = 1.1, n = 7,
          linetype = 1) +
  scale_color_manual(values=c("#FF2700", "#77AB43"))+
  labs(title="Evolución de hospitalizaciones COVID-19",
       subtitle = "En CDMX")+
  scale_x_date(date_breaks= "2 weeks", date_labels = "%d/%b") +
  ggthemes::theme_fivethirtyeight()

ggsave(filename  = "~/Downloads/hosp.png", width = 9,height = 5,units = "in")



# casos y hosp ------------------------------------------------------------

hosp <- db_sis%>% 
  filter(resdefin!="NEGATIVO" & 
           resdefin!="NO ADECUADO" & 
           resdefin!="RECHAZADA") %>% 
  mutate(fecingre=ymd(fecingre)) %>% 
  filter(fecingre>="2021-01-01" ) %>% 
  filter(tipacien=="HOSPITALIZADO") %>% 
  group_by(fecingre) %>% 
  summarise(total_hosp=n()) %>% 
  mutate(maximo_hosp=max(total_hosp)) %>% 
  mutate(porc_hosp=total_hosp/maximo_hosp) %>% 
  select(fecha=fecingre, Hospitalizaciones=porc_hosp)

casos <- db_sis%>% 
  filter(resdefin!="NEGATIVO" & 
           resdefin!="NO ADECUADO" & 
           resdefin!="RECHAZADA") %>% 
  mutate(fecinisi=ymd(fecinisi)) %>% 
  filter(fecinisi>="2021-01-01" ) %>% 
  group_by(fecinisi) %>% 
  summarise(total_casos=n()) %>% 
  mutate(maximo_casos=max(total_casos)) %>% 
  mutate(porc_casos=total_casos/maximo_casos) %>% 
  select(fecha=fecinisi, Casos=porc_casos)

fa <- left_join(casos, hosp) %>% 
      gather(tipo, pct, Casos:Hospitalizaciones)






fa %>%  
  filter(fecha>="2021-09-01" & fecha<=max(fecha)-4) %>% 
  ggplot( ) +
  # geom_line(aes(x = fecha, y = pct, color = tipo), size = 1.1, alpha = .2) +
  # geom_point(aes(x = fecha, y = pct, color = tipo),size = 1.1, alpha = .3) +
  geom_ma(aes(x = fecha, y = pct, color = tipo), size = 0.9, n = 7,
          linetype = 1) +
  scale_color_manual(values=c("#77AB43","#FF2700" ))+
  labs(title="Tendencia de hospitalizaciones por edades",
       subtitle = "En CDMX, porcentaje con respecto al pico máximo", 
       color="",
       caption="Fuente: Covid-19 SINAVE-CDMX")+
  ggthemes::theme_fivethirtyeight()


# a ver  ------------------------------------------------------------------

fe <- db_sis%>% 
  filter(fechreg>="2021-01-01") %>% 
  filter(tipacien=="HOSPITALIZADO") %>% 
  filter(resdefin!="NEGATIVO" & 
           resdefin!="NO ADECUADO" & 
           resdefin!="RECHAZADA") %>% 
 group_by(fechreg) %>% 
 summarise(total=n())


fe %>%  
  ggplot( ) +
  geom_line(aes(x = fechreg, y = total), size = 1.1, alpha = .2) +
  geom_point(aes(x = fechreg, y = total),size = 1.1, alpha = .3) +
  geom_ma(aes(x = fechreg, y = total), size = 0.9, n = 7,
          linetype = 1)
