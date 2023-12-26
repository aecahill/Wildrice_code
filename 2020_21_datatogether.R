#need file river which is the matrix of species
#need file riversites 
#need file riverrich

river<-read.table("C:/Users/aecsk/Documents/GitHub/Wildrice_code/river_taxa.txt",header=TRUE)
riversites<-read.table("C:/Users/aecsk/Documents/GitHub/Wildrice_code/river_sites.txt",header=TRUE)
riverrich<-read.table("C:/Users/aecsk/Documents/GitHub/Wildrice_code/river_rich.txt",header=TRUE)

#load vegan
library(vegan)
library(wesanderson)

library(cowplot)
library(ggplot2)

#compute NMDS
rivernmds<-metaMDS(river)

#plot NMDS in base R
#ordiplot(asus3nmds,type="n")
#ordiellipse(asus3nmds,groups=sites$Sea,draw="polygon",col="grey90",label=F)
#orditorp(asus3nmds,display="species",col="black",air=0.01)
#orditorp(asus3nmds,display="sites",col="red",air=0.01)

#analysis of similarity for sites and regions
anosim(river,riversites$Month)
anosim(river,riversites$Enviro)
anosim(river,riversites$Rice)

#compute PERMANOVA with a space and time interaction
adonis(formula=river~riversites$Month*riversites$Rice)



#moving plot to ggplot

data.scores <- as.data.frame(scores(rivernmds)$sites)
datascores<-cbind(data.scores,riversites)
head(datascores)
species.scores <- as.data.frame(scores(rivernmds, "species"))
species.scores$species <- rownames(species.scores)
head(species.scores)



#make hulls, one for each sea

grp.a <- data.scores[datascores$Month == "June", ][chull(datascores[datascores$Month == 
                                                                       "June", c("NMDS1", "NMDS2")]), ]
grp.b <- data.scores[datascores$Month == "August", ][chull(datascores[datascores$Month == 
                                                                      "August", c("NMDS1", "NMDS2")]), ]
grp.c <- data.scores[datascores$Month == "October", ][chull(datascores[datascores$Month == 
                                                                         "October", c("NMDS1", "NMDS2")]), ]
hull.data <- rbind(grp.a, grp.b,grp.c) #turn the hulls into a single dataframe
hull.month<-c("June","June","June","June","June","June","June","August","August","August","August","August","August","October","October","October","October","October","October","October","October") #add column for groups (these are based on this data only)
hull.data<-cbind(hull.data,hull.month) #attach group names to hull dataframe

#plot in ggplot

palwes<-c("#F21A00","#EBCC2A","#3B9AB2")

morphotime<-ggplot() +
  geom_point(data=datascores,aes(x=NMDS1,y=NMDS2,colour=Month),size=3) + # add the point markers
  scale_colour_manual(values = palwes) +
  coord_equal() +
  theme_bw()+
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())+ 
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,group=hull.month),alpha=0.15) #add polygon based on the hulls calculated

#ACROSS SITES

grp.a <- data.scores[datascores$Enviro == "a_riverbank", ][chull(datascores[datascores$Enviro == 
                                                                  "a_riverbank", c("NMDS1", "NMDS2")]), ]
grp.b <- data.scores[datascores$Enviro == "b_channel_wildrice", ][chull(datascores[datascores$Enviro == 
                                                                  "b_channel_wildrice", c("NMDS1", "NMDS2")]), ]
grp.c <- data.scores[datascores$Enviro == "c_wildricebed_left_dock", ][chull(datascores[datascores$Enviro == 
                                                                  "c_wildricebed_left_dock", c("NMDS1", "NMDS2")]), ]
grp.d <- data.scores[datascores$Enviro == "d_wildricebed_right_dock", ][chull(datascores[datascores$Enviro == 
                                                                  "d_wildricebed_right_dock", c("NMDS1", "NMDS2")]), ]
grp.e <- data.scores[datascores$Enviro == "e_bank_near_honeysuckle", ][chull(datascores[datascores$Enviro == 
                                                                  "e_bank_near_honeysuckle", c("NMDS1", "NMDS2")]), ]
grp.f <- data.scores[datascores$Enviro == "f_bank_near_tallgrass", ][chull(datascores[datascores$Enviro == 
                                                                  "f_bank_near_tallgrass", c("NMDS1", "NMDS2")]), ]
grp.g <- data.scores[datascores$Enviro == "g_bank_near_grassmud", ][chull(datascores[datascores$Enviro == 
                                                                  "g_bank_near_grassmud", c("NMDS1", "NMDS2")]), ]
grp.h <- data.scores[datascores$Enviro == "h_upriver_wildricebed", ][chull(datascores[datascores$Enviro == 
                                                                                       "h_upriver_wildricebed", c("NMDS1", "NMDS2")]), ]
grp.i <- data.scores[datascores$Enviro == "i_riverbank_field", ][chull(datascores[datascores$Enviro == 
                                                                                       "i_riverbank_field", c("NMDS1", "NMDS2")]), ]
grp.j <- data.scores[datascores$Enviro == "j_wildricebed_bridge", ][chull(datascores[datascores$Enviro == 
                                                                                       "j_wildricebed_bridge", c("NMDS1", "NMDS2")]), ]

hull.data <- rbind(grp.a, grp.b, grp.c, grp.d,grp.e, grp.f, grp.g, grp.h,grp.i,grp.j) #turn the hulls into a single dataframe
hull.sample<-c("A","A","A","A","A","B","B","B","B","B","C","C","C","C","D","D","D","D","D","E","E","E","E","E","E","F","F","F","F","G","G","G","G","G","H","H","H","H","I","I","I","I","I","I","J","J","J","J") #add column for groups (these are based on this data only)
hull.data<-cbind(hull.data,hull.sample) #attach group names to hull dataframe

#plot in ggplot

morphosite<-ggplot() +
  geom_point(data=datascores,aes(x=NMDS1,y=NMDS2,colour=Enviro),size=3) + # add the point markers
  scale_colour_manual(values=rev(wes_palette("Zissou1", n = 10, type="continuous"))) +
  coord_equal() +
  theme_bw()+
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())+ 
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,group=hull.sample),alpha=0.10) #add polygon based on the hulls calculated


#By rice
grp.a <- data.scores[datascores$Rice == "Y", ][chull(datascores[datascores$Rice == 
                                                                      "Y", c("NMDS1", "NMDS2")]), ]
grp.b <- data.scores[datascores$Rice == "N", ][chull(datascores[datascores$Rice == 
                                                                        "N", c("NMDS1", "NMDS2")]), ]
hull.data <- rbind(grp.a, grp.b) #turn the hulls into a single dataframe
hull.rice<-c("Y","Y","Y","Y","Y","Y","Y","N","N","N","N","N","N","N") #add column for groups (these are based on this data only)
hull.data<-cbind(hull.data,hull.rice) #attach group names to hull dataframe

morphorice<-ggplot() +
  geom_point(data=datascores,aes(x=NMDS1,y=NMDS2,colour=Rice),size=3) + # add the point markers
  scale_colour_manual(values=rev(wes_palette("Zissou1", n = 2, type="continuous"))) +
  coord_equal() +
  theme_bw()+
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())+ 
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,group=hull.rice),alpha=0.10) #add polygon based on the hulls calculated

plot_grid(morphosite,morphotime,morphorice,labels=c("A","B","C"),ncol=1)


#diversity statistics

riverdiv<-cbind(diversity(river,index="simpson"),riversites) #calculate simpsons index, bind to site information

colnames(riverdiv)<-c("Simpsons","Enviro","Month","Site","Replicate","Rice") #rename columns


#summary(aov(marshdiv$Simpsons~marshdiv$Month)) #anova among regions
#summary(aov(marshdiv$Simpsons~marshdiv$Site)) #anova among regions

summary(aov(riverdiv$Simpsons~riverdiv$Rice+riverdiv$Month*riverdiv$Enviro)) #two-way ANOVA

riverdivJune<-riverdiv[riverdiv$Month == "June", ]
riverdivAugust<-riverdiv[riverdiv$Month == "August", ]
riverdivOctober<-riverdiv[riverdiv$Month == "October", ]


divJune<-ggplot(riverdivJune,aes(x=Enviro,y=Simpsons,fill=Enviro))+
  geom_boxplot()+ 
  geom_jitter(alpha=0.5)+
  scale_fill_manual(values=rev(wes_palette("Zissou1", n = 10, type="continuous"))) +
  ylim(0,1)+
  theme_bw()+
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=1),
    axis.title.x = element_blank(), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

divAugust<-ggplot(riverdivAugust,aes(x=Enviro,y=Simpsons,fill=Enviro))+
  geom_boxplot()+ 
  geom_jitter(alpha=0.5)+
  scale_fill_manual(values=rev(wes_palette("Zissou1", n = 10, type="continuous"))) +
  ylim(0,1)+
  theme_bw()+
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=1),
    axis.title.x = element_blank(), # remove x-axis labels
        axis.title.y = element_blank(), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

divOctober<-ggplot(riverdivOctober,aes(x=Enviro,y=Simpsons,fill=Enviro))+
  geom_boxplot()+ 
  geom_jitter(alpha=0.5)+
  scale_fill_manual(values=rev(wes_palette("Zissou1", n = 10, type="continuous"))) +
  ylim(0,1)+
  theme_bw()+
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=1),
    axis.title.x = element_blank(), # remove x-axis labels
        axis.title.y = element_blank(), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())


plot_grid(divJune,divAugust,divOctober,ncol=3)


#richness stats

summary(aov(riverrich$Richness~riverdiv$Rice+riverrich$Month*riverrich$Enviro)) #two-way ANOVA

riverrichJune<-riverrich[riverrich$Month == "June", ]
riverrichAugust<-riverrich[riverrich$Month == "August", ]
riverrichOctober<-riverrich[riverrich$Month == "October", ]


richJune<-ggplot(riverrichJune,aes(x=Enviro,y=Richness,fill=Enviro))+
  geom_boxplot()+ 
  geom_jitter(alpha=0.5)+
  scale_fill_manual(values=rev(wes_palette("Zissou1", n = 10, type="continuous"))) +
  ylim(0,3)+
  theme_bw()+
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=1),
        axis.title.x = element_blank(), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

richAugust<-ggplot(riverrichAugust,aes(x=Enviro,y=Richness,fill=Enviro))+
  geom_boxplot()+ 
  geom_jitter(alpha=0.5)+
  scale_fill_manual(values=rev(wes_palette("Zissou1", n = 10, type="continuous"))) +
  ylim(0,3)+
  theme_bw()+
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=1),
        axis.title.x = element_blank(), # remove x-axis labels
        axis.title.y = element_blank(), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

richOctober<-ggplot(riverrichOctober,aes(x=Enviro,y=Richness,fill=Enviro))+
  geom_boxplot()+ 
  geom_jitter(alpha=0.5)+
  scale_fill_manual(values=rev(wes_palette("Zissou1", n = 10, type="continuous"))) +
  ylim(0,3)+
  theme_bw()+
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=1),
        axis.title.x = element_blank(), # remove x-axis labels
        axis.title.y = element_blank(), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())


plot_grid(richJune,richAugust,richOctober,ncol=3)
