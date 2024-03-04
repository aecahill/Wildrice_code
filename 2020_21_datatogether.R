#need file river which is the matrix of species
#need file riversites 
#need file riverrich

river<-read.table("C:/Users/aecsk/Documents/GitHub/Wildrice_code/river_taxa.txt",header=TRUE)
riversites<-read.table("C:/Users/aecsk/Documents/GitHub/Wildrice_code/river_sites.txt",header=TRUE)
riverrich<-read.table("C:/Users/aecsk/Documents/GitHub/Wildrice_code/river_rich.txt",header=TRUE)

#NOTE MARCH 4 2024: SITE AND RICE ARE COLLINEAR! which, obviously, bc site is either rice or no rice, always
#So let's not consider site. It's not important. What we want is rice and no rice. 

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
#anosim(river,riversites$Enviro)
anosim(river,riversites$Rice)

#compute PERMANOVA with a space and time interaction
adonis2(formula=river~riversites$Month*riversites$Rice+riversites$Enviro)



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

plot_grid(morphotime,morphorice,labels=c("A","B"),ncol=1)



#diversity statistics

riverdiv<-cbind(diversity(river,index="simpson"),riversites) #calculate simpsons index, bind to site information

colnames(riverdiv)<-c("Simpsons","Enviro","Month","Site","Replicate","Rice") #rename columns

summary(aov(riverdiv$Simpsons~riverdiv$Rice*riverdiv$Month+riverdiv$Enviro)) #two-way ANOVA



diversityfig<-ggplot(riverdiv,aes(x=Month,y=Simpsons,fill=Rice))+
  geom_boxplot()+ 
  geom_point(alpha=0.6, position=position_jitterdodge(0.2))+
  scale_fill_manual(values=rev(wes_palette("Zissou1", n = 2, type="continuous"))) +
  scale_color_manual(values=rev(wes_palette("Zissou1", n = 2, type="continuous"))) +
  ylim(0,1)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=1),
    axis.title.x = element_blank(), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())


#richness stats

summary(aov(riverrich$Richness~riverrich$Rice*riverrich$Month+riverrich$Enviro)) #two-way ANOVA

#ricerich.lm <- lm(formula = Richness ~ Rice*Month,
                 #data = riverrich)

#car::Anova(ricerich.lm, type = 3)

richnessfig<-ggplot(riverrich,aes(x=Month,y=Richness,fill=Rice))+
  geom_boxplot()+ 
  geom_point(alpha=0.6, position=position_jitterdodge(0.2))+
  scale_fill_manual(values=rev(wes_palette("Zissou1", n = 2, type="continuous"))) +
  scale_color_manual(values=rev(wes_palette("Zissou1", n = 2, type="continuous"))) +
  ylim(0,3)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=1),
        axis.title.x = element_blank(), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

plot_grid(richnessfig,diversityfig,labels=c("A","B"),ncol=1)
