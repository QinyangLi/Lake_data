
# packages #### 
pacman::p_load(readr, doBy, tidyr,ggplot2,stringr,cowplot,dplyr,magrittr,ggsci, readxl, viridis,swemaps, cowplot, gridExtra, PerformanceAnalytics, dendextend,ggdendro,reshape2,grid,gplots )

# merge chemistry and chlorophyta data ####
load("../RData_files/chem1941_2019.RData")
load("../RData_files/chloro1941_2019.RData")
chemerge <- chem1941_2019 %>% dplyr::rename(id=MD.MVMId)
chlmerge<- chloro1941_2019 %>% dplyr::rename(id=MD.MVM_Id)    


# plotting trend over the year ####
View(chlmerge)

trend_data <- chlmerge %>% dplyr::select(Provtagningsår, Provdatum, TaxonId, Taxonnamn, Täthet_.celler.l., Biovolym_.mm3.l.)
head(trend_data)

trend <- trend_data %>% group_by(Provtagningsår, TaxonId, Taxonnamn) %>% 
  summarise(mean_density = mean(Täthet_.celler.l., na.rm=T),
            mean_biovol = mean(Biovolym_.mm3.l., na.rm=T))  
  
View(trend)

ggplot(trend, aes(x=Provtagningsår, y=log(mean_biovol))) +
  geom_point()+
  stat_smooth(method = loess) +
  theme_bw()

ggplot(trend, aes(x=Provtagningsår, y=log(mean_biovol))) +
  geom_point()+
  stat_smooth(method = lm) +
  theme_bw()+
  xlim(1995,2020)+
  ggpubr::stat_regline_equation(label.x = 2005, label.y = 3)+
  ggpubr::stat_cor(label.x = 2005, label.y = 2)
  
library(ggpubr)
ggplot(trend, aes(x=Provtagningsår, y=log(mean_density))) +
  geom_point()+ 
  xlim(2003,2020)+
  stat_smooth(method = lm)+
  theme_classic()+
  ggpubr::stat_regline_equation(label.x = 2005, label.y = 20)+
  ggpubr::stat_cor(label.x = 2005, label.y = 19)

ggplot(trend, aes(x=Provtagningsår, y=mean_density)) +
  geom_point()+ 
  xlim(2003,2020)+
  stat_smooth(method = lm)+
  theme_classic()+
  ggpubr::stat_regline_equation(label.x = 2005, label.y = 2.0e+07)+
  ggpubr::stat_cor(label.x = 2005, label.y = 1.9e+07)  

# remove all the "_" in colnames in chlmerge #########
names(chlmerge) <- gsub("_","",names(chlmerge))   

# change date formate in chemerge to yyyy-mm-dd####
chemerge$Provdatum <- as.character(as.Date(chemerge$Provdatum, "%d/%m/%Y"))

# unique identifier:  MVMID + year + month + day: ####
chemerge$mergeid<-paste(chemerge$id,chemerge$Provtagningsår,chemerge$Provtagningsmånad,chemerge$Provtagningsdag,sep="_")
chlmerge$mergeid<-paste(chlmerge$id,chlmerge$Provtagningsår,chlmerge$Provtagningsmånad,chlmerge$Provtagningsdag,sep="_")

# remove underscores in variables except taxonnamn in chlmerge ####
chlmerge <- chlmerge %>% mutate(across(Stationsnamn:Kommun, gsub, pattern="_", replacement=""))
# alternatively:
# chlmerge <- chlmerge %>% mutate(across(Stationsnamn:Kommun,~ gsub("_","",.))) 
# "~" is a shortcut for function(gsub){}, and "."is a shortcut for Stationsnamn:Kommun

# since chemistry and chlorophyta not sampled necessarily on the same day, if we do a full merge, lakes with both chemistry
# and phytoplankton data but not done in the same day will be deleted (e.g. Erken), we therefore do a 
# partial merge: #### 
chloro_chem_merged<-merge(chemerge, chlmerge,by="mergeid", all.x = T, all.y = T) # argument:  all.x = T & all.y = T for partial merge
dim(chloro_chem_merged) # 545432    180

# coalesce repeated column names ####
# use dplyr::coalesce to merge e.g. Statiosnamn.x and Stationsnamn.y into one column
# dplyr::coalesce finds the first non-missing value 
dat <- chloro_chem_merged %>% mutate(lake_name = coalesce(Stationsnamn.x, Stationsnamn.y), 
                                     id = coalesce(as.factor(id.x), as.factor(id.y)),
                                     station_coor_n_x= coalesce(as.integer(StationskoordinatN.X.x),as.integer(StationskoordinatN.X.y)),
                                     station_coor_e_y= coalesce(as.integer(StationskoordinatE.Y.x), as.integer(StationskoordinatE.Y.y)),
                                     sample_coor_n_x = coalesce(as.integer(ProvplatskoordinatN.X.x), as.integer(ProvplatskoordinatN.X.y)),
                                     sample_coor_e_y = coalesce(as.integer(ProvplatskoordinatE.Y.x), as.integer(ProvplatskoordinatE.Y.y)),
                                     program = coalesce(Program.x,Program.y),
                                     delprogram = coalesce(Delprogram.x,Delprogram.y),
                                     project = coalesce(Projekt.x,Projekt.y),
                                     county = coalesce(Län.x,Län.y),
                                     municipality = coalesce(Kommun.x,Kommun.y),
                                     provid = coalesce(ProvId.x, ProvId.y),
                                     sample_date = coalesce(Provdatum.x, Provdatum.y),
                                     year = coalesce(Provtagningsår.x, Provtagningsår.y),
                                     month = coalesce(Provtagningsmånad.x, Provtagningsmånad.y),
                                     day = coalesce(Provtagningsdag.x, Provtagningsdag.y),
                                     coordinate_sys = coalesce(as.character(Koordinatsystem.x),as.character(Koordinatsystem.y)),
                                     sampling_medium = coalesce(Provtagningsmedium.x, Provtagningsmedium.y),
                                     research_type = coalesce(Undersökningstyp.x,Undersökningstyp.y),
                                     comment = coalesce(Provkommentar.x,Provkommentar.y),
                                     mscdc2 = coalesce(MSCDC2.x,MSCDC2.y),
                                     mscdc3 = coalesce(MSCDC3.x,MSCDC3.y),
                                     euid = coalesce(EUid.x,EUid.y),
                                     min_depth_chloro = Minprovdjup.m..y,
                                     max_depth_chloro = Maxprovdjup.m..y,
                                     min_depth_chem =  Minprovdjup.m..x,
                                     max_depth_chem =  Maxprovdjup.m..x)

# remove redundant variables: ####
remove<-c("Stationsnamn.x", "Stationsnamn.y","id.x","id.y","Program.x","Program.y","Delprogram.x","Delprogram.y","Projekt.x","Projekt.y","Län.x","Län.y",
          "Kommun.x","Kommun.y","ProvId.x","ProvId.y","Provdatum.x","Provdatum.y","Provtagningsår.x","Provtagningsår.y","Provtagningsmånad.x",
          "Provtagningsmånad.y","Provtagningsdag.x","Provtagningsdag.y","Koordinatsystem.x","Koordinatsystem.y","Provtagningsmedium.x","Provtagningsmedium.y",
          "Undersökningstyp.x","Undersökningstyp.y","Provkommentar.x","Provkommentar.y","StationskoordinatN.X.x","StationskoordinatN.X.y",
          "StationskoordinatE.Y.x", "StationskoordinatE.Y.y","ProvplatskoordinatN.X.x", "ProvplatskoordinatN.X.y",
          "ProvplatskoordinatE.Y.x", "ProvplatskoordinatE.Y.y","MSCDC2.x","MSCDC2.y","MSCDC3.x","MSCDC3.y","EUid.x","EUid.y",
          "Minprovdjup.m..x","Minprovdjup.m..y","Maxprovdjup.m..x", "Maxprovdjup.m..y")
length(remove) # check how many redundant variables to be removed: 50
length(which(names(dat) %in% remove)) # check how many of those variables are in dat: 50
remove[!(remove %in% names(dat))]
remove_ids<-which(names(dat) %in% remove) # shows the column orders where redundant ids are to be removed
dat <- dat[, -(remove_ids)] # remove the variables
dim(dat) # 545432 157

# relocate columns dplyr::relocate ####
chloro_chem <- dat %>% relocate(c(lake_name:max_depth_chem), .before = AbsF254..5cm.) %>% relocate(c(research_type:mscdc3), .after = max_depth_chem)

# Fill up empty cells with NAs ####
chloro_chem2 <- chloro_chem %>% mutate_all(na_if,"") 

# change all the colnames to lower case ####
colnames(chloro_chem2) %<>% tolower()
unique(chloro_chem2$taxonnamn)
# Add algae phenotype data ####
load("../RData_files/algaepheno.RData")
algaepheno <- algaepheno %>% rename(taxonnamn = species)

# check species name differences between algaepheno and chloro_chem2: ####
setdiff(algaepheno$taxonnamn, lake_full$taxonnamn) # "Pandorina_sp"   "Tetradesmus_dimorphus"

# change "Pandorina_sp" and "Tetradesmus_dimorphus" to "Pandorina" and "Tetradesmus_dimorphus_" respectively in algaepheno
algaepheno[algaepheno == "Pandorina_sp"] <- "Pandorina"
algaepheno[algaepheno == "Tetradesmus_dimorphus"] <- "Tetradesmus_dimorphus_"

# join the full dataset lake_full ####
mother_lake <- left_join(chloro_chem2, algaepheno, by="taxonnamn")
dim(mother_lake) # 545432 217  

# check duplicated lakes ####
# duplicated_lake <- lake_full[duplicated(lake_full), ] # takes some time cr. 2min
# View(duplicated_lake) # turns out those apparent duplication are due to sampling multiple times in each day, so keep them

# save the data locally####
# save(mother_lake, file="../RData_files/mother_lake.RData")
# write_excel_csv(mother_lake, "../Fulldata/mother_lake.csv" )

# save the data to shared Algae/Data folder ####
# write_excel_csv(mother_lake, "../../../../Algae/Data/Lake_slu/LakeData_full_dataset/mother_lake.csv") # export to shared dropbox folder Algae/lake_slu
# save(mother_lake, file="../../../../Algae/Data/Lake_slu/LakeData_full_dataset/mother_lake.RData")

