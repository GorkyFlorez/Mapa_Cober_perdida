library(raster)
library(tidyverse)
library(ggplot2)
library(sf)
library(ggplot2)
library(cowplot)
library(ggspatial)


Inambari = st_read("SHP/Inambari.shp")  %>% st_as_sf()
Inamba   <- st_transform(Inambari ,
                         crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Defores = stack("Raster/Parte1.tif")
Defores_Puer   <- crop(Defores, Inamba)                           #   
Defores_Puer   <- Defores_Puer<- mask(Defores_Puer, Inamba)

plot(Defores_Puer)

Defores_tbl  <-  rasterToPoints(Defores_Puer)
Defores_df   <-  data.frame(Defores_tbl)
colnames(Defores_df) = c("x", "y", "Año")

Defores_df_2012= Defores_df%>%
  subset(Año<= 19 & Año> 0)  %>%
  mutate(Años = 2000 +Año)

Defores2 = stack("Raster/Parte2.tif")
Defores_Puer2   <- crop(Defores2, Inamba)                           #   
Defores_Puer2   <- Defores_Puer2<- mask(Defores_Puer2, Inamba)

plot(Defores_Puer2)

Defores_tbl2  <-  rasterToPoints(Defores_Puer2)
Defores_df2   <-  data.frame(Defores_tbl2)
colnames(Defores_df2) = c("x", "y", "Año")

Defores_df_20122= Defores_df2%>%
  subset(Año<= 19 & Año> 0)  %>%
  mutate(Años = 2000 +Año)


col=c( '#8ecae6', '#023e8a', '#03045e', '#184e77', '#40916c', '#80b918',
            '#55a630','#aacc00','#d4d700','#eeef20','#ffff3f','#ff9e00','#ff9100','#ff6d00','#e36414'
            ,'#9a031e')

Mapa = ggplot()  +
  geom_sf(data= Inamba, fill="gray90", color="black", size=0.8)+
  geom_raster(data = Defores_df_2012 ,aes(x,y, fill = Años) , show.legend = F)+
  scale_fill_gradientn(colours = col)+
  new_scale_fill()+
  geom_raster(data = Defores_df_20122 ,aes(x,y, fill = Años) )+
  scale_fill_gradientn(colours = col,
                       name="Periodo",
                       breaks = c(2001, 2002,2003,2004,2005,2006,2007,
                                  2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015,
                                  2016, 2017, 2018, 2019))+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  coord_sf(xlim = c(-70.8,-69.44416), ylim = c(-13.39782 ,-12.58043)) +
  theme_bw()+
  theme(legend.position = c(0.9, 0.24),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect( color = "white", fill = NA, size = 0.5),
        
        
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=7, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.3,"cm"), #ancho de cuadrados de referencia 
  )+
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")



tbl = rasterToPoints(Defores_Puer, spatial = F)
tbl = as_tibble(tbl)
tbl = setNames(tbl, c("x", "y", "year"))
tbl = filter(tbl, year > 0)

summ = tbl %>%
  group_by(year)%>%
  summarise(count =n())%>%
  ungroup ()%>%
  mutate( year= 2000 + year)

summ = mutate(summ, meters =count *900, has = meters /10000)
summ = dplyr::select(summ, year,has)

tbl1 = rasterToPoints(Defores_Puer2, spatial = F)
tbl1 = as_tibble(tbl1)
tbl1 = setNames(tbl1, c("x", "y", "year"))
tbl1 = filter(tbl1, year > 0)

summ1 = tbl1 %>%
  group_by(year)%>%
  summarise(count =n())%>%
  ungroup ()%>%
  mutate( year= 2000 + year)

summ1 = mutate(summ1, meters =count *900, has = meters /10000)
summ1 = dplyr::select(summ1, year,has)

Combi = full_join(summ1, summ , by = c("year", "has"))

summ$has = summ$has + summ1$has
  
  
library(hrbrthemes)
library(gcookbook)
library(tidyverse)

# current verison
packageVersion("hrbrthemes")
## [1] '0.8.6'
update_geom_font_defaults(font_rc_light)

Estadis = ggplot(data = summ, aes(x=year, y=has, fill=year)) +
  geom_col(show.legend = F) +
  scale_fill_gradientn(colours = col)+
  scale_y_comma(limits=c(0,10000)) +
  coord_flip() +
  labs(x="Periodo 2000 - 2019 (años)",
       y="Hectareas (ha)",
       caption="") + 
  theme_bw()+
  geom_text(aes(label=paste0(round(has,1), "ha"), hjust=0, nudge_y=20) ,family="serif", size=2)+
  theme(plot.background = element_rect(fill = "white"),
        plot.subtitle = element_text(face = "italic", family="serif"),
        plot.caption = element_text(size = 8, hjust = 0.95, family="serif", face = "italic"),
        
        axis.text.x  = element_text(face="bold", color="black", size=8,
                                    family="serif"),
        axis.text.y  = element_text(face="bold", color="black",
                                    family="serif",size=8),
        axis.title = element_text(face="bold", color="black"),
        panel.border = element_rect( color = "white", fill = NA, size = 0.5)
        )+
  scale_x_continuous(breaks = c(2001:2019)) 

Estadis


summ
summ$has =round(summ$has,2)
# lets count proportions
summ$fraction = summ$has/sum(summ$has)
summ

# Compute the cumulative proportions (top of each rectangle)
summ$ymax = cumsum(summ$fraction)
summ
# Compute the bottom of each rectangle
summ$ymin = c(0, head(summ$ymax, n=-1))
summ
#compute label position
summ$labelPosition= (summ$ymax+summ$ymin)/2
summ
#get data label
summ$label= paste0(summ$Categoria ,"\n Valor=", summ$has)
summ
library(ggrepel)
Ha_total = sum(summ$has)

summ$Porcentaje =  summ$has*100/Ha_total
summ$Porcentaje =round(summ$Porcentaje,2)

Pastel=ggplot(summ,aes(ymax=ymax,ymin=ymin,xmax=4, xmin=2, fill=year ))+
  geom_rect(alpha=0.8)+
  coord_polar(theta="y")+
  xlim(c(0,4))+
  theme_void()+
  geom_text(aes(y=labelPosition,label=paste0(round(Porcentaje,2), "%")),x=3, 
            color="black", family="serif",angle=90,
            size=2.5)+
  theme(legend.position = "none")+
  scale_fill_gradientn(colours = col)

Pastel



Fin=ggdraw() +
  coord_equal(xlim = c(0, 30), ylim = c(0, 22), expand = FALSE) +
  draw_plot(Mapa , width = 29, height = 22,x = 0.5, y = 0)+
  
  draw_plot(Estadis , width = 10, height = 10,x = 0, y = 10)+
  draw_plot(Pastel , width = 10, height = 10,x = 0, y = 0)+
  
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "black", fill = NA, size = 1))+
  annotate(geom = "text", x = -75, y = -17, hjust = 0, vjust = 1, angle=45,
           label = "Gorky Florez Castillo            Gorky Florez Castillo        Gorky Florez Castillo",
           size = 7, family="serif", color = "grey20",
           alpha=0.2)

ggsave(plot = Fin ,"Mapa/Mapa de perdida.png", units = "cm", 
       width = 30,height = 22, dpi = 1200) 



