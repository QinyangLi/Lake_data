########### Original code by Teodor Alling

## NOTE: missing Alk.Acid, added by Qinyang 20220323####

################## Packages used:
library(plyr)

getOption("encoding") # originally it's 'native.enc'
options(encoding = "ISO-8859-1") # Change encoding to scandinavian language

################## Read .csv files:
c1 <- read.csv('../Raw_data/chem2_1941-1961_(1946).csv',header=TRUE)
c2 <- read.csv('../Raw_data/chem2_1962-1984.csv',header=TRUE)
c3 <- read.csv('../Raw_data/chem2_1985-1999.csv',header=TRUE)
c4 <- read.csv('../Raw_data/chem2_2000-2010.csv',header=TRUE)
c5 <- read.csv('../Raw_data/chem2_2011-2019.csv',header=TRUE)

################## First we need to join the five files together:
Reduce(intersect, list(colnames(c5),colnames(c4),colnames(c3),colnames(c2),colnames(c1))) 
#This shows us that the five dataframes share 32 variables. (Where the names match exactly, at least...) 
#Let's see how many unique variables there are in all dataframes combined:
vars<-c(colnames(c5),colnames(c4),colnames(c3),colnames(c2),colnames(c1))
length(unique(vars)) #It's 210 unique columns among the five dfs.
unique(vars)
# combining all files into one named 'rawchem'
clist<-list(c1,c2,c3,c4,c5)
rawchem<-rbind.fill(clist)
# ?rbind.fill
# str(rawchem)
# View(rawchem)

#Lots of duplication and unwanted columns in rawchem, needs cleanning
chem<-rawchem # Make copy and start trimming the copy instead of rawchem.
chemvars<-sort(colnames(chem[,26:210])) # The variable names only, sorted alphabetically.
#View(chemvars)
#head(chemvars)
chemvars<-c(colnames(chem[,1:25]),chemvars) # All the columns with only the chemical names sorted alphabetically.
chem<-chem[chemvars] #Updated chem data frame, now in a better order.
k<-chem
View(colnames(k))

# Qinyang: the following steps are to standardize units in mg/l or mekv/l 
# Qinyang: mg/L = mEq/L (mekv/L) × equivalent weight (e.g. 
# equivalent weight Ca2+ = (Atomic weight of Ca)/2 = 40.078/2 = 20.039 )
# mEq/L: milliequivalents per liter

# See slu website for unit conversion references:
# https://miljodata.slu.se/MVM/DataContents/UnitConversion

#Aluminium Aluminum:
k$Al_mgL<-ifelse(is.na(k$Al.µg.l.),k$Al.mg.l.,k$Al.µg.l./1000)
#Remove: "Al.µg.l."              "Al.mg.l."

#Aluminium inorganic Aluminum inorganic:
k$AlI_mgL<-ifelse(is.na(k$AlINAJ.µg.l.),k$AlINAJ.mg.l.,k$AlINAJ.µg.l./1000)
#Remove: "AlINAJ.µg.l."          "AlINAJ.mg.l."

#Alkalinity Alkalinity:
k$Alk_comb0<-ifelse(is.na(k$Alk..mekv.l.),k$Alk..mmol.l.,k$Alk..mekv.l.) #combine mekvL and mmolL
k$Alk_mekvL<-ifelse(is.na(k$Alk_comb0),k$Alk..mgHCO3.l./61.0168,k$Alk_comb0) #combine above with mgHCO3L
#Remove: Alk..mekv.l."          "Alk..mgHCO3.l."          "Alk..mmol.l."        "Alk_comb0"

# Qinyang addition: 
# Alk.Acid.mekv.l.
k$Alk_Acid_mekvL<-ifelse(is.na(k$Alk.Acid.mmol.l.),k$Alk.Acid.mekv.l.,k$Alk.Acid.mmol.l.) #combine mekvL and mmolL

#Arsenik Arsenic: 
k$As_mgL<-ifelse(is.na(k$As.µg.l.),k$As.mg.l.,k$As.µg.l./1000)
#Remove: "As.µg.l."              "As.mg.l."

#Arsenik filtrerat Arsenic filtered:
k$As_filt_mgL<-ifelse(is.na(k$AsF.µg.l.),k$AsF.mg.l.,k$AsF.µg.l./1000)
#Remove: "AsF.µg.l."             "AsF.mg.l."

#Bod7 Biochemical oxygen demand 7 days:
k$bod7_mgL<-ifelse(is.na(k$BOD7.mg.l.),k$BOD7.mg.lO2.,k$BOD7.mg.l.)
#Remove: "BOD7.mg.l."            "BOD7.mg.lO2."

#Calcium:
k$Ca_mekvL<-ifelse(is.na(k$Ca.mekv.l.),k$Ca.mg.l./20.039,k$Ca.mekv.l.) 
#Remove: "Ca.mekv.l."            "Ca.mg.l."

#Cadmium:
k$Cd_mgL<-ifelse(is.na(k$Cd.µg.l.),k$Cd.mg.l.,k$Cd.µg.l./1000)
#Remove: "Cd.µg.l."              "Cd.mg.l."

#Cadmium filtered:
k$CdF_mgL<-ifelse(is.na(k$CdF.µg.l.),k$CdF.mg.l.,k$CdF.µg.l./1000)
#Remove: "CdF.µg.l."            "CdF.mg.l."

#Chloride:
k$Cl_mekvL<-ifelse(is.na(k$Cl.mekv.l.),k$Cl.mg.l./35.4,k$Cl.mekv.l.)
#Remove: "Cl.mekv.l."            "Cl.mg.l."

#Cobalt :
k$Co_mgL<-ifelse(is.na(k$Co.µg.l.),k$Co.mg.l.,k$Co.µg.l./1000)
#Remove: "Co.µg.l."              "Co.mg.l."

#Kemisk syreförbrukning (permanganat) mg/L Chemical oxygen demand (permanganate):
k$CODMn_mgL<-ifelse(is.na(k$CODMn.mg.l.),k$CODMn.mg.lO2.,k$CODMn.mg.l.)
#Remove: "CODMn.mg.l."           "CODMn.mg.lO2."

#Krom Chromium:
k$Cr_mgL<-ifelse(is.na(k$Cr.µg.l.),k$Cr.mg.l.,k$Cr.µg.l./1000)
#Remove: "Cr.µg.l."              "Cr.mg.l."

#Krom filtered Chromium filtered:
k$CrF_mgL<-ifelse(is.na(k$CrF.µg.l.),k$CrF.mg.l.,k$CrF.µg.l./1000)
#Remove: "CrF.µg.l."             "CrF.mg.l."

#Copper:
k$Cu_mgL<-ifelse(is.na(k$Cu.µg.l.),k$Cu.mg.l.,k$Cu.µg.l./1000)
#Remove: "Cu.µg.l."              "Cu.mg.l."

#Copper filtered:
k$CuF_mgL<-ifelse(is.na(k$CuF.µg.l.),k$CuF.mg.l.,k$CuF.µg.l./1000)
#Remove: "CuF.µg.l."             "CuF.mg.l."

#DOC Dissolved organic carbon:
k$DOC_mgL<-ifelse(is.na(k$DOC.mg.l.),k$DOC.mg.lC.,k$DOC.mg.l.)
#Remove: "DOC.mg.l."             "DOC.mg.lC."

#Fluoride:
k$F_mekvL<-ifelse(is.na(k$F.mekv.l.),k$F.mg.l./18.998,k$F.mekv.l.)
#Remove: "F.mekv.l."             "F.mg.l."

#Iron:
k$Fe_mgL<-ifelse(is.na(k$Fe.µg.l.),k$Fe.mg.l.,k$Fe.µg.l./1000)
#Remove: "Fe.µg.l."              "Fe.mg.l."

#Iron filtered:
k$FeF_mgL<-ifelse(is.na(k$FeF.µg.l.),k$FeF.mg.l.,k$FeF.µg.l./1000)
#Remove: "FeF.µg.l."             "FeF.mg.l."

#Mercury:
k$Hg_mgL0<-ifelse(is.na(k$Hg.µg.l.),k$Hg.mg.l.,k$Hg.µg.l./1000)
k$Hg_mgL<-ifelse(is.na(k$Hg.ng.l.),k$Hg_mgL0,k$Hg.ng.l./1000000)
#Remove: "Hg.µg.l."              "Hg.mg.l."              "Hg.ng.l."           "Hg_mgL0"

#Mercury filtered:
k$HgF_ugL<-ifelse(is.na(k$HgF.µg.l.),k$HgF.ng.l./1000,k$HgF.µg.l.)
#Remove: "HgF.µg.l."             "HgF.ng.l."

#Potassium:
k$K_mekvL<-ifelse(is.na(k$K.mekv.l.),k$K.mg.l./39,k$K.mekv.l.)
#Remove: "K.mekv.l."             "K.mg.l."

#Klorofyll Chlorophyll:
k$kfyll_ugL0<-ifelse(is.na(k$Kfyll.µg.l.),k$Kfyll.µg.l..1,k$Kfyll.µg.l.)
k$Kfyll_ugL<-ifelse(is.na(k$Kfyll.mg.m3.),k$kfyll_ugL0,k$Kfyll.mg.m3./0.000001)
#Remove:  "Kfyll.µg.l."           "Kfyll.µg.l..1"         "Kfyll.mg.m3."        "kfyll_ugL0"

#Kjeldahlvkäve Kjeld Nitrogen:
k$KjelN_nanocomb<-ifelse(is.na(k$Kjeld..N.µg.l.),k$Kjeld..N.µg.lN.,k$Kjeld..N.µg.l.)
k$KjeldN_mgL<-ifelse(is.na(k$Kjeld..N.mg.l.),k$KjelN_nanocomb/1000,k$Kjeld..N.mg.l.)
#Remove: "Kjeld..N.µg.l."        "Kjeld..N.µg.lN."       "Kjeld..N.mg.l."           "KjelN_nanocomb"

#Konduktivitet vid 20C Conductivity at 20C:
k$kond20_mS_m<-ifelse(is.na(k$Kond20.mS.m.),k$Kond20.µS.cm./10,k$Kond20.mS.m.)
#Remove:  "Kond20.µS.cm."         "Kond20.mS.m."

#Konduktivitet vid 25C Conductivity at 25C:
k$kond25_mS_m<-ifelse(is.na(k$Kond25.mS.m.),k$Kond25.µS.cm./10,k$Kond25.mS.m.)
#Remove: "Kond25.µS.cm."         "Kond25.mS.m."

#Magnesium:
k$Mg_mekvL<-ifelse(is.na(k$Mg.mekv.l.),k$Mg.mg.l./12.15,k$Mg.mekv.l.)
#Remove: "Mg.mekv.l."            "Mg.mg.l."

#Mangan Manganese:
k$Mn_mgL<-ifelse(is.na(k$Mn.µg.l.),k$Mn.mg.l.,k$Mn.µg.l./1000)
#Remove: "Mn.µg.l."              "Mn.mg.l."

#Mangan filtrerad Manganese filtered:
k$MnF_mgL<-ifelse(is.na(k$MnF.µg.l.),k$MnF.mg.l.,k$Mn.µg.l./1000)
#Remove: "MnF.µg.l."             "MnF.mg.l." 

#Molybden Molybdenum:
k$Mo_mgL<-ifelse(is.na(k$Mo.µg.l.),k$Mo.mg.l.,k$Mo.µg.l./1000)
#Remove: "Mo.µg.l."              "Mo.mg.l."

#Sodium:
k$Na_mekvL<-ifelse(is.na(k$Na.mekv.l.),k$Na.mg.l./23,k$Na.mekv.l.)
#Remove: "Na.mekv.l."            "Na.mg.l."

#Ammonium:
k$NH4_ugL0<-ifelse(is.na(k$NH4.N.µg.l.),k$NH4.N.µg.lN.,k$NH4.N.µg.l.)
k$NH4_mgL<-ifelse(is.na(k$NH4_ugL0),k$NH4.N.mg.l.,k$NH4_ugL0/1000)
#Remove: "NH4.N.µg.l."       "NH4.N.µg.lN."       "NH4.N.mg.l."       "NH4.N.mg.lN."      "k$NH4_ugL0"

#Nickel:
k$Ni_mgL<-ifelse(is.na(k$Ni.µg.l.),k$Ni.mg.l.,k$Ni.µg.l./1000)
#Remove: "Ni.µg.l."              "Ni.mg.l."

#Nickel filtered:
k$NiF_mgL<-ifelse(is.na(k$NiF.µg.l.),k$NiF.mg.l.,k$NFi.µg.l./1000)
#Remove:  "NiF.µg.l."             "NiF.mg.l."

#Nitrit+Nitratkväve Nitrite + Nitrate:
k$NO2_NO3_micro0<-ifelse(is.na(k$NO2.NO3.N.µg.l.),k$NO2.NO3.N.µg.lN.,k$NO2.NO3.N.µg.l.)
k$NO2_NO3_mgL0<-ifelse(is.na(k$NO2.NO3.N.mg.l.),k$NO2.NO3.N.mg.lN.,k$NO2.NO3.N.mg.l.)
k$NO2_NO3_mgL<-ifelse(is.na(k$NO2_NO3_micro0),k$NO2_NO3_mgL0,k$NO2_NO3_micro0/1000)
#Remove: "NO2.NO3.N.µg.l." "NO2.NO3.N.µg.lN." "NO2.NO3.N.mg.l." "NO2.NO3.N.mg.lN." "NO2_NO3_micro0" "NO2_NO3_mgL0"

#Nitrate:
k$NO3_mgL<-ifelse(is.na(k$NO3.N.µg.l.),k$NO3.N.µg.l,k$NO3.N.µg.l./1000)
#Remove:  "NO3.N.µg.l."           "NO3.N.mg.l."

#Lead:
k$Pb_mgL<-ifelse(is.na(k$Pb.µg.l.),k$Pb.mg.l.,k$Pb.µg.l./1000)
#Remove: "Pb.µg.l."              "Pb.mg.l."

#Bly filtrerat Lead filtered:
k$PbF_mgL<-ifelse(is.na(k$PbF.µg.l.),k$PbF.mg.l.,k$PbF.µg.l./1000)
#Remove: "PbF.µg.l."             "PbF.mg.l."

#Fosfatfosfor Phosphate phosphorus:
k$PO4_micro0<-ifelse(is.na(k$PO4.P.µg.l.),k$PO4.P.µg.lP.,k$PO4.P.µg.l.)
k$PO4_mgL0<-ifelse(is.na(k$PO4.P.mg.l.),k$PO4.P.mg.lP.,k$PO4.P.mg.l.)
k$PO4_mgL<-ifelse(is.na(k$PO4_micro0),k$PO4_mgL0,k$PO4_micro0/1000)
#Remove: "PO4.P.µg.l."  "PO4.P.µg.lP."   "PO4.P.mg.l."           "PO4.P.mg.lP." "PO4_micro0" "PO4_mgL0"

#Silicon:
k$Si_mgL<-ifelse(is.na(k$Si.µg.l.),k$Si.mg.l.,k$Si.µg.l./1000)
#Remove: "Si.µg.l."              "Si.mg.l."

#Sulfate:
k$SO4_mgL0<-ifelse(is.na(k$SO4.mg.l.),k$SO4.mg.lS.,k$SO4.mg.l.)
k$SO4_mekvL<-ifelse(is.na(k$SO4.mekv.l.),k$SO4_mgL0/(96.06/2),k$SO4.mekv.l.)
#Remove: "SO4.mekv.l."       "SO4.mg.l."             "SO4.mg.lS."       "k$SO4_mgL0"

#Syrgashalt Oxygen content:
k$syrgas_mg_L<-ifelse(is.na(k$Syrgashalt.mg.l.),k$Syrgashalt.mg.lO2.,k$Syrgashalt.mg.l.)
#Remove: "Syrgashalt.mg.l."      "Syrgashalt.mg.lO2."

#TOC Total organic carbon:
k$TOC_mgL<-ifelse(is.na(k$TOC.mg.l.),k$TOC.mg.lC.,k$TOC.mg.l.)
#Remove: "TOC.mg.l."             "TOC.mg.lC."

#Tot N Total Nitrogen:
k$totN_mgL0<-ifelse(is.na(k$Tot.N.mg.l.),k$Tot.N.mg.lN.,k$Tot.N.mg.l.)
k$totN_micro0<-ifelse(is.na(k$Tot.N.µg.l.),k$Tot.N.µg.lN.,k$Tot.N.µg.l.)
k$totN_mgL<-ifelse(is.na(k$totN_micro0),k$totN_mgL0,k$totN_micro0/1000)
#Remove: "Tot.N.µg.l."   "Tot.N.µg.lN."   "Tot.N.mg.l."     "Tot.N.mg.lN."  "totN_mgL0"   "totN_micro0"

#Totalkväve Persulfat-metoden Total nitrogen Persulfate method:
k$totNps_mgL0<-ifelse(is.na(k$Tot.Nps.mg.l.),k$Tot.Nps.mg.lN.,k$Tot.Nps.mg.l.)
k$totNps_micro0<-ifelse(is.na(k$Tot.Nps.µg.l.),k$Tot.Nps.µg.lN.,k$Tot.Nps.µg.l.)
k$totNps_mgL<-ifelse(is.na(k$totNps_micro0),k$totNps_mgL0,k$totNps_micro0/1000)
#Remove: "Tot.Nps.µg.l."   "Tot.Nps.µg.lN."   "Tot.Nps.mg.l."   "Tot.Nps.mg.lN."    "totNps_mgL0"   "totNps_micro0"

#Totalkväve (summa-metoden, dvs Kjeldahlkväve+NO2+NO3) Total nitrogen (Kjeld nitrogen + no2 + no3)
k$TotNsum_micro0<-ifelse(is.na(k$Tot.Nsumma.µg.l.),k$Tot.Nsumma.µg.lN,k$Tot.Nsumma.µg.l.)
k$TotNsum_mgL<-ifelse(is.na(k$TotNsum_micro0),k$Tot.Nsumma.mg.l.,k$TotNsum_micro0/1000) 
#Remove: "Tot.Nsumma.µg.l."    "Tot.Nsumma.µg.lN."     "Tot.Nsumma.mg.l." "TotNsum_micro0"

#Totalkväve TNb-metoden Total nitrogen TNb method
k$Tot_NTNb_micro0<-ifelse(is.na(k$Tot.NTNb.µg.l.),k$Tot.NTNb.µg.lN.,k$Tot.NTNb.µg.l.)
k$Tot_NTNb_mgL<-ifelse(is.na(k$Tot_NTNb_micro0),k$Tot.NTNb.mg.l.,k$Tot_NTNb_micro0/1000)
#Remove: "Tot.NTNb.µg.l."        "Tot.NTNb.µg.lN."       "Tot.NTNb.mg.l." "Tot_NTNb_micro0"

#Total fosfor Total phosphorus:
k$TotP_micro0<-ifelse(is.na(k$Tot.P.µg.l.),k$Tot.P.µg.lP.,k$Tot.P.µg.l.)
k$TotP_mgL0<-ifelse(is.na(k$Tot.P.mg.l.),k$Tot.P.mg.lP.,k$Tot.P.mg.l.)
k$TotP_mgL<-ifelse(is.na(k$TotP_micro0),k$TotP_mgL0,k$TotP_micro0/1000)
#Remove: "Tot.P.µg.l."     "Tot.P.µg.lP."   "Tot.P.mg.l."   "Tot.P.mg.lP." "TotP_micro0"  "TotP_mgL0"

#Filtrerad totalfosfor Total phosphorus filtered:
k$TotPF_ugL<-ifelse(is.na(k$Tot.PF.µg.l.),k$Tot.PF.µg.lP.,k$Tot.PF.µg.l.)
#Remove: "Tot.PF.µg.l."          "Tot.PF.µg.lP."

#Vanadin Vanadium:
k$V_mgL<-ifelse(is.na(k$V.mg.l.),k$V.µg.l./1000,k$V.mg.l.)
#Remove:  "V.µg.l."               "V.mg.l."

#Zink:
k$Zn_mgL<-ifelse(is.na(k$Zn.mg.l.),k$Zn.µg.l./1000,k$Zn.mg.l.)
#Remove:  "Zn.µg.l."              "Zn.mg.l."

#Remove the now redundant and the temporary variables that I created above:
removefromk<-c("Al.µg.l.","Al.mg.l.","AlINAJ.µg.l.","AlINAJ.mg.l.","Alk..mekv.l.","Alk..mgHCO3.l.",
               "Alk..mmol.l.","Alk_comb0","Alk.Acid.mekv.l.", "Alk.Acid.mmol.l.", "As.µg.l.","As.mg.l.","AsF.µg.l.","AsF.mg.l.","BOD7.mg.l.",
               "BOD7.mg.lO2.","Ca.mekv.l.","Ca.mg.l.","Cd.µg.l.","Cd.mg.l.","CdF.µg.l.","CdF.mg.l.",
               "Cl.mekv.l.","Cl.mg.l.","Co.µg.l.","Co.mg.l.","CODMn.mg.l.","CODMn.mg.lO2.","Cr.µg.l.",
               "Cr.mg.l.","CrF.µg.l.","CrF.mg.l.","Cu.µg.l.","Cu.mg.l.","CuF.µg.l.","CuF.mg.l.",
               "DOC.mg.l.","DOC.mg.lC.","F.mekv.l.","F.mg.l.","Fe.µg.l.","Fe.mg.l.","FeF.µg.l.",
               "FeF.mg.l.","Hg.µg.l.","Hg.mg.l.","Hg.ng.l.","Hg_mgL0","HgF.µg.l.","HgF.ng.l.",
               "K.mekv.l.","K.mg.l.","Kfyll.µg.l.","Kfyll.µg.l..1","Kfyll.mg.m3.","kfyll_ugL0",
               "Kjeld..N.µg.l.","Kjeld..N.µg.lN.","Kjeld..N.mg.l.","KjelN_nanocomb","Kond20.µS.cm.",
               "Kond20.mS.m.","Kond25.µS.cm.","Kond25.mS.m.","Mg.mekv.l.","Mg.mg.l.","Mn.µg.l.",
               "Mn.mg.l.","MnF.µg.l.","MnF.mg.l.","Mo.µg.l.","Mo.mg.l.","Na.mekv.l.","Na.mg.l.",
               "NH4.N.µg.l.","NH4.N.µg.lN.","NH4.N.mg.l.","NH4.N.mg.lN.","NH4_ugL0","Ni.µg.l.",
               "Ni.mg.l.","NiF.µg.l.","NiF.mg.l.","NO2.NO3.N.µg.l.","NO2.NO3.N.µg.lN.",
               "NO2.NO3.N.mg.l.","NO2.NO3.N.mg.lN.","NO2_NO3_micro0","NO2_NO3_mgL0","NO3.N.µg.l.",
               "NO3.N.mg.l.","Pb.µg.l.","Pb.mg.l.","PbF.µg.l.","PbF.mg.l.","PO4.P.µg.l.",
               "PO4.P.µg.lP.","PO4.P.mg.l.","PO4.P.mg.lP.","PO4_micro0","PO4_mgL0","Si.µg.l.",
               "Si.mg.l.","SO4.mekv.l.","SO4.mg.l.","SO4.mg.lS.","SO4_mgL0","Syrgashalt.mg.l.",
               "Syrgashalt.mg.lO2.","TOC.mg.l.","TOC.mg.lC.","Tot.N.µg.l.","Tot.N.µg.lN.",
               "Tot.N.mg.l.","Tot.N.mg.lN.","totN_mgL0","totN_micro0","Tot.Nps.µg.l.","Tot.Nps.µg.lN.",
               "Tot.Nps.mg.l.","Tot.Nps.mg.lN.","totNps_mgL0","totNps_micro0","Tot.Nsumma.µg.l.",
               "Tot.Nsumma.µg.lN.","Tot.Nsumma.mg.l.","TotNsum_micro0","Tot.NTNb.µg.l.",
               "Tot.NTNb.µg.lN.","Tot.NTNb.mg.l.","Tot_NTNb_micro0","Tot.P.µg.l.","Tot.P.µg.lP.",
               "Tot.P.mg.l.","Tot.P.mg.lP.","TotP_micro0","TotP_mgL0","Tot.PF.µg.l.","Tot.PF.µg.lP.",
               "V.µg.l.","V.mg.l.","Zn.µg.l.","Zn.mg.l.")
removefromk_ids<-which(names(k) %in% removefromk)
length(removefromk) #143
length(removefromk_ids) #143, good.
removefromk[!(removefromk %in% names(k))] #Checks which names to remove that do not exist in k. None!
k<-k[,-(removefromk_ids)]
dim(k)# 138 variables which is the number of variables in k now. Great!

chem1941_2019 <- k
dim(k)
save(chem1941_2019, file = "../RData_files/chem1941_2019.RData")


# write.csv(k,"Dropbox (MEEL)/QinyangLi/PhD/LakeData/LakeData_Qinyang/processed_files/chem1941-2019.csv")
# 
# processed_chem <- read.csv("Dropbox (MEEL)/QinyangLi/PhD/LakeData/LakeData_Miljödata/Processedfiles/chem1941-present.csv", header = TRUE)
# head(processed_chem)
