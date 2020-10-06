library(tidyverse)
# geom_sf(data=taz_mrs, size=.15, color= "dark gray", fill= "dark gray", alpha= .25)+
#   geom_sf(data=taz_na, size=.15, color= "#6b5069", fill= "#946f91", alpha= .5)+
#   geom_sf(data= taz_filtered, size= .15, color= "#b84f00", fill= "#d95f02", alpha= .5)+
#   geom_sf(data= taz_filtered_dif, size= .15, color= "#44947c", fill= "#5fcfad", alpha= .35)+


legend_entries <- data.frame(source= c("pct_change_filter", "no_pct_calc", "abs_change_filter"), 
                             x= c(1,1,2), 
                             y= c(1,2,1.5),
                             size= c(3,3,3))
cols <- c("% Change Filter"= "#b84f00", "No % Change Calculated"= "#6b5069", "Absolute Change Filter"= "#44947c")
fills <- c("% Change Filter"= "#d95f02", "No % Change Calculated"= "#946f91", "Absolute Change Filter"= "#5fcfad")
alpha <- c("% Change Filter"= ".5", "No % Change Calculated"= ".5", "Absolute Change Filter"= ".3")

ggplot(legend_entries, aes(x=x, y=y))+
  geom_point(aes(size= size, color= source, fill= source), shape= 20, alpha= .5, show.legend = FALSE)+
  scale_color_manual(values= cols)+
  scale_fill_manual(values= fills)+
  scale_alpha_manual(values= alpha)+
  scale_size_continuous(range=c(0,500))+
  theme_minimal()


library(ggforce)
cols <- c("% Change Filter"= "#b84f00", "No % Change Calculated"= "#6b5069", "Absolute Change Filter"= "#44947c")
fills <- c("% Change Filter"= "#d95f02", "No % Change Calculated"= "#946f91", "Absolute Change Filter"= "#5fcfad")
alpha <- c("% Change Filter"= .5, "No % Change Calculated"= .5, "Absolute Change Filter"= .3)

df.venn <- data.frame(x = c(0, 0.866, -0.866),
                      y = c(1, -0.5, -0.5),
                      labels = c("% Change Filter", 'No % Change Calculated', 'Absolute Change Filter'))
ggplot(df.venn, aes(x0 = x, y0 = y, r = 1.5, fill = labels, color= labels, alpha=labels)) +
  geom_circle( size = 1) +
  scale_color_manual(values= cols)+
  scale_fill_manual(values= fills)+
  scale_alpha_manual(values= alpha)+
  coord_fixed() +
  theme_void()


 df_sq_ven <- data.frame( x1=c(.5,0,1) ,
                          x2= c(1.5,2,3),
                          y1=c(1,2,0) ,
                          y2=c(3,5,4) ,
                          labels = c("No % Change Calculated", '% Change Filter', 'Absolute Change Filter'))
 ggplot(df_sq_ven, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill = labels, color= labels, alpha=labels)) +
   geom_rect( size = 1) +
   scale_color_manual(values= cols)+
   scale_fill_manual(values= fills)+
   scale_alpha_manual(values= alpha)+
   coord_fixed() +
   theme_void()
 