library(raster)
library(tidyverse)
library(ggplot2)
library(sf)
library(ggplot2)
library(cowplot)

Peru1  <- getData('GADM', country='Peru', level=1) %>% st_as_sf()
Peru2  <- getData('GADM', country='Peru', level=2) %>% st_as_sf()
MDD  =  subset(Peru2 , NAME_1 == "Madre de Dios")

SurAmerica = st_read("SHP/SurAmerica.geojson")  %>% st_as_sf()
SurAmeric  <- st_transform(SurAmerica  ,
                           crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Inambari = st_read("SHP/Inambari.shp")  %>% st_as_sf()
Inamba   <- st_transform(Inambari ,
                          crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Inambari_Cober  = st_read("SHP/Inambari_Cober.shp")  %>% st_as_sf()
Inambari_Cobe   <- st_transform(Inambari_Cober ,
                              crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
1000000
Ha_total = sum(Inambari_Cobe$Shape_Area/10000)

df2  = dplyr::select(Inambari_Cobe, CobVeg2013 , Shape_Area)
df2$Shape_Area =df2$Shape_Area/10000
df2$Porcentaje =  (df2$Shape_Area/10000)*100/Ha_total

Resu_area = df2 %>%
  as_tibble %>%
  group_by(CobVeg2013)%>%
  summarize (
    Cantidad =n(),
    area =sum(Shape_Area))


library(ggnewscale)
library(ggspatial)
Mapa= ggplot() + 
  geom_sf(data= Inamba, fill=NA, color="black", size=0.8)+
  geom_sf(data = Inambari_Cobe, aes(fill = CobVeg2013), color = "white", size = 0.1)+
  scale_fill_viridis_d(option = "inferno", name="Cobertura vegetal")+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  
  new_scale_fill()+
  geom_raster(data = Defores_df_2012 ,aes(x,y, fill = Año), 
              show.legend = F)+
  scale_fill_gradientn(colours = "red")+
  coord_sf(xlim = c(-70.7,-69.44416), ylim = c(-13.39782 ,-12.58043)) +
  theme_bw()+
  theme(legend.position = c(0.12, 0.22),
        
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

legend <- get_legend(Mapa)

Mapa1= Mapa +  theme(legend.position = "nene")
Resu_area$area

summ <- Resu_area %>%
  mutate(CobVeg2013 = fct_reorder(CobVeg2013 , Resu_area$area, .desc = TRUE))

round(Resu_area$area,2)

Stat = Mapa +scale_fill_viridis_d(option = "inferno", name="Cobertura \nHectárea (ha)",
                           labels = c("21909.43", "66520.15", "23741.28","1548.17", 
                                      "542.37", "4266.89", "40584.85", "14750.93", "45993.57",
                                      "82254.46", "263.22", "179894.72", "22384.13" ,"20831.76",
                                      "307.90", "11923.60", "4435.38"))

ggplot() + 
  geom_sf(data= Inamba, fill=NA, color="black", size=0.8)+
  geom_sf(data = Inambari_Cobe, aes(fill = CobVeg2013), color = "white", size = 0.1)+
  scale_fill_viridis_d(option = "inferno", name="Cobertura \nHectárea (ha)",
                       labels = c("21909.43", "66520.15", "23741.28","1548.17", 
                                  "542.37", "4266.89", "40584.85", "14750.93", "45993.57",
                                  "82254.46", "263.22", "179894.72", "22384.13" ,"20831.76",
                                  "307.90", "11923.60", "4435.38"),
                       guide = guide_legend( direction = "horizontal",
                                             keyheight = unit(0.3, units = "cm"), 
                                             keywidth=unit(0.001, units = "cm"), 
                                             label.position = "bottom", title.position = 'top', nrow=1))+
  
  coord_sf(xlim = c(-70.7,-69.44416), ylim = c(-13.39782 ,-12.58043)) +
  theme_classic()+
  theme(legend.position = c(0.15, 0.15),
        legend.text=element_text(size=7, family="serif", angle=90),
        axis.text.x  = element_text(face="bold", color="black", size=8,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=8),
        axis.title = element_text(face="bold", color="black"),
        
        plot.title = element_text(size = 16, hjust = 0.5, family="serif", face = "italic"),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = "italic", family="serif"),
        plot.caption = element_text(size = 10, hjust = 0.95, family="serif", face = "italic"),
        
        
        legend.background = element_rect(fill = "white"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.3,"cm"), #ancho de cuadrados de referencia 
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))


  
  
legend_Stat <- get_legend(Stat)

Fin=ggdraw() +
  coord_equal(xlim = c(0, 30), ylim = c(0, 22), expand = FALSE) +
  draw_plot(Mapa1 , width = 29, height = 22,x = 0.5, y = 0)+
  
  draw_plot(legend , width = 8, height = 8,x = 4.5, y = 15.5)+
  draw_plot(legend_Stat , width = 8, height = 8,x = 3.5, y = 9.6)+
  
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "black", fill = NA, size = 1))+
  annotate(geom = "text", x = -75, y = -17, hjust = 0, vjust = 1, angle=45,
           label = "Gorky Florez Castillo            Gorky Florez Castillo        Gorky Florez Castillo",
           size = 7, family="serif", color = "grey20",
           alpha=0.2)

ggsave(plot = Fin ,"Mapa/Mapa de Cobertura.png", units = "cm", 
       width = 30,height = 22, dpi = 1200) 







