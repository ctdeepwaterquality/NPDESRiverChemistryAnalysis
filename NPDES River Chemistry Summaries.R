# Use this to make sure your code is being written to the right folder.
getwd()

# Need to install the packages you want and reference them in the library
# before you can work with them.

install.packages("tidyverse")
install.packages("respirometry")
install.packages("writexl")

library(tidyverse)
library(respirometry)
library(writexl)

# Load in data
River_Tox_Data <- read.csv("CleanedR_NPDES.csv")

# Find Means; if you want a different parameter,
# change it in the 'summarise' function. 
# I have it set to print 127 rows; otherwise, it lets you preview 10 and
# then has a bit about how to print 117 more rows.

# DO
River_Tox_Data |>
  group_by(Site) |>
  summarise(Mean_DO = mean(DO, na.rm = TRUE)) |>
  print(n=127)

# pH (This has to be done a little differently.
# I learned that you can't directly average pH data because of the logarithmic
# nature of pH data. Doing it with the way I did it above gave me a pH of 
# 15.8 for one value. That's how I knew something wasn't quite right...)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Mean_pH = mean_pH(`Chem pH`, na.rm = TRUE)) |>
  print(n=127)

# Gonna experiment with doing all of them at once to speed things up a bit.
# Starting with mean conducivity and salinity simulaneously, as proof of concept.

River_Tox_Data |>
  group_by(Site) |>
  summarise(Mean_Cond = mean(Conductivity, na.rm = TRUE),
            Mean_Sal = mean(Salinity, na.rm = TRUE)) |>
  print(n=127)

# That worked, now let's do more:
River_Tox_Data |>
  group_by(Site) |>
  summarise(Mean_Alk = mean(Alkalinity, na.rm = TRUE),
            Mean_Hard = mean(Hardness, na.rm = TRUE),
            Mean_Chl = mean(Chloride, na.rm = TRUE),
            Mean_TSS = mean(TSS, na.rm = TRUE),
            Mean_NH4 = mean(Ammonia, na.rm = TRUE),
            Mean_NO3 = mean(Nitrate, na.rm = TRUE),
            Mean_NO2 = mean(Nitrite, na.rm = TRUE))|>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Mean_TKN = mean(TKN, na.rm = TRUE),
            Mean_PO4 = mean(Phosphate, na.rm = TRUE),
            Mean_P = mean(Phosphorus, na.rm = TRUE),
            Mean_SO4 = mean(Sulfate, na.rm = TRUE),
            Mean_BOD5 = mean(BOD5, na.rm = TRUE),
            Mean_COD = mean(COD, na.rm = TRUE),
            Mean_DOC = mean(DOC, na.rm = TRUE)) |>
  print(n=127)


River_Tox_Data |>
  group_by(Site) |>
  summarise(Mean_TOC = mean(TOC, na.rm = TRUE),
            Mean_Fl = mean(Fluoride, na.rm = TRUE),
            Mean_CNT = mean(`CN-T`, na.rm = TRUE),
            Mean_TRC = mean(TRC, na.rm = TRUE),
            Mean_TDS = mean(TDS, na.rm = TRUE),
            Mean_MBAS = mean(MBAS, na.rm = TRUE),
            Mean_OandG = mean(`Oand G`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Mean_Form = mean(Formaldehyde, na.rm = TRUE),
            Mean_CdD = mean(`Cd- Diss.`, na.rm = TRUE),
            Mean_CdT = mean(`Cd-Tot.`, na.rm = TRUE),
            Mean_Cr6 = mean(`Cr- +6`, na.rm = TRUE),
            Mean_CrD = mean(`Cr- Diss`, na.rm = TRUE),
            Mean_CrT = mean(`Cr- Tot`, na.rm = TRUE),
            Mean_CuD = mean(`Cu- Diss`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Mean_Cu_T = mean(`Cu-Tot`, na.rm = TRUE),
            Mean_Pb_D = mean(`Pb- Diss.`, na.rm = TRUE),
            Mean_Pb_T = mean(`Pb- Tot`, na.rm = TRUE),
            Mean_Ni_D = mean(`Ni- Diss`, na.rm = TRUE),
            Mean_Ni_T = mean(`Ni-Tot`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Mean_Zn_D = mean(`Zn- Diss`, na.rm = TRUE),
            Mean_Zn_T = mean(`Zn-Tot`, na.rm = TRUE),
            Mean_Fe_D = mean(`Fe- Diss`, na.rm = TRUE),
            Mean_Fe_T = mean(`Fe- Tot`, na.rm = TRUE),
            Mean_Mn_D = mean(`Mn-Diss`, na.rm = TRUE),
            Mean_Mn_T = mean(`Mn- Tot`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Mean_Al_D = mean(`Al- Diss`, na.rm = TRUE),
            Mean_Al_T = mean(`Al- Tot`, na.rm = TRUE),
            Mean_Tin = mean(`Sn-Tot`, na.rm = TRUE),
            Mean_Ag_D = mean(`Ag- Diss`, na.rm = TRUE),
            Mean_Ag_T = mean(`Ag- Tot`, na.rm = TRUE),
            Mean_Mo_D = mean(`Mo-Diss`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Mean_Mo_T = mean(`Mo-Tot`, na.rm = TRUE),
            Mean_Ca = mean(Ca, na.rm = TRUE),
            Mean_Mg = mean(`Mg-Tot`, na.rm = TRUE),
            Mean_Hg = mean(`Hg-Tot`, na.rm = TRUE),
            Mean_Ti = mean(`Ti-Tot`, na.rm = TRUE),
            Mean_V = mean(`V-Tot`, na.rm = TRUE),
            Mean_B = mean(Boron, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Mean_Co = mean(`Co-Tot`, na.rm = TRUE),
            Mean_Au = mean(`Au-Tot`, na.rm = TRUE),
            Mean_Na = mean(`Na-T`, na.rm = TRUE),
            Mean_Ba = mean(`Ba-T`, na.rm = TRUE),
            Mean_Sb = mean(`Sb-Tot`, na.rm = TRUE),
            Mean_As = mean(`As-Tot`, na.rm = TRUE),
            Mean_Be = mean(`Be-Tot`, na.rm = TRUE),
            Mean_Se = mean(`Se-Tot`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Mean_Tl = mean(Tl, na.rm = TRUE),
            Mean_Hyd = mean(Hydrazine, na.rm = TRUE),
            Mean_1c2n = mean(`1-chloro-2-nitrobenzene`, na.rm = TRUE),
            Mean_Ace = mean(Acetone, na.rm = TRUE),
            Mean_AcM = mean(Acrylamide, na.rm = TRUE),
            Mean_AcN = mean(Acrylonitrile, na.rm = TRUE),
            Mean_Ani = mean(Aniline, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Mean_Az = mean(Azobenzene, na.rm = TRUE),
            Mean_Bnz = mean(Benzene, na.rm = TRUE),
            Mean_Bze = mean(Benzidine, na.rm = TRUE),
            Mean_BEHP = mean(`Bis-2-ethyl hexyl phthlate`, na.rm = TRUE),
            Mean_BD = mean(bromodichloromethane, na.rm = TRUE),
            Mean_Bm = mean(Bromoform, na.rm = TRUE),
            Mean_B13 = mean(`Butadiene 1,3`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Mean_But = mean(Butanol, na.rm = TRUE),
            Mean_BuI = mean(`Butanol, iso`, na.rm = TRUE),
            Mean_Cze = mean(Carbazole, na.rm = TRUE),
            Mean_CDS = mean(`Carbon disulfide`, na.rm = TRUE),
            Mean_CA2 = mean(`Chloroaniline, 2-`, na.rm = TRUE),
            Mean_CA4 = mean(`Chloroaniline,4-`, na.rm = TRUE),
            Mean_Cbz = mean(chlorobenzene, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Mean_Cfm = mean(Chloroform, na.rm = TRUE),
            Mean_Cme = mean(chloromethane, na.rm = TRUE),
            Mean_Cp2 = mean(`Chlorophenol-2`, na.rm = TRUE),
            Mean_DCn = mean(Dichloran, na.rm = TRUE),
            Mean_D14 = mean(`Dichlorobenzene 1,4`, na.rm = TRUE),
            Mean_D12 = mean(`Dichlorobenzene, 1,2`, na.rm = TRUE),
            Mean_Dbd = mean(Dichlorobenzidine, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Mean_D11 = mean(`Dichloroethane 1,1`, na.rm = TRUE),
            Mean_D12 = mean(`Dichloroethane 1,2`, na.rm = TRUE),
            Mean_Cis = mean(`dichloroethylene, cis`, na.rm = TRUE),
            Mean_Trn = mean(`Dichloroethylene, trans`, na.rm = TRUE),
            Mean_D13 = mean(`Dicholorbenzene, 1,3`, na.rm = TRUE),
            Mean_DiP = mean(`Diisononyl phthlate`, na.rm = TRUE),
            Mean_D33 = mean(`Dimethylbenzidine 3,3,`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Mean_Dio = mean(Dioxane, na.rm = TRUE),
            Mean_Ebz = mean(Ethylbenzene, na.rm = TRUE),
            Mean_I = mean(Isopropylbenzene, na.rm = TRUE),
            Mean_MK = mean(MEK, na.rm = TRUE),
            Mean_Mhl = mean(Methanol, na.rm = TRUE),
            Mean_MhA = mean(Methylacrylate, na.rm = TRUE),
            Mean_MCl = mean(`methylene Cl`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Mean_MTBE = mean(MTBE, na.rm = TRUE),
            Mean_Nap = mean(Napthalene, na.rm = TRUE),
            Mean_Np4 = mean(`Nitrophenol, 4-`, na.rm = TRUE),
            Mean_pcp = mean(pentachlophenol, na.rm = TRUE),
            Mean_PT = mean(phenanthrene, na.rm = TRUE),
            Mean_Phe = mean(phenols, na.rm = TRUE),
            Mean_Pp2 = mean(`Propanol, 2-`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Mean_Sty = mean(Styrene, na.rm = TRUE),
            Mean_Thf = mean(Tetrahydrofuran, na.rm = TRUE),
            Mean_TCa = mean(Tetrachloroethane, na.rm = TRUE),
            Mean_TCe = mean(Tetrachloroethylene, na.rm = TRUE),
            Mean_Tld = mean(`toludine, m`, na.rm = TRUE),
            Mean_Tle = mean(Toluene, na.rm = TRUE),
            Mean_111 = mean(`Trichloroethane 1,1,1`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Mean_Tce = mean(Trichloroethene, na.rm = TRUE),
            Mean_Tcp = mean(`Trichlorophenol 2,4,6`, na.rm = TRUE),
            Mean_VC = mean(`Vinyl chloride`, na.rm = TRUE),
            Mean_X = mean(Xylene, na.rm = TRUE),
            Mean_ETPH = mean(ETPH, na.rm = TRUE),
            Mean_TPH = mean(TPH, na.rm = TRUE),
            Mean_DEET = mean(DEET, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Mean_Car = mean(Carbaryl, na.rm = TRUE),
            Mean_BPA = mean(`Bisphenol A`, na.rm = TRUE),
            Mean_Caf = mean(Caffiene, na.rm = TRUE),
            Mean_Cop = mean(Copranastol, na.rm = TRUE),
            Mean_Cho = mean(Cholesterol, na.rm = TRUE),
            Mean_DNO = mean(`Di-n-octylphthlalte`, na.rm = TRUE),
            Mean_Non = mean(Nonylphenol, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Mean_VOC = mean(VOCs, na.rm = TRUE),
            Mean_AOX = mean(AOX, na.rm = TRUE),
            Mean_Fur = mean(Furfural, na.rm = TRUE),
            Mean_ON = mean(`Organic Nitrogen`, na.rm = TRUE),
            Mean_PCB = mean(PCBs, na.rm = TRUE),
            Mean_CA3 = mean(`Chloroaniline, 3-`, na.rm = TRUE),
            Mean_Dip = mean(Diphenamid, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Mean_Dib = mean(Dibenzofuran, na.rm = TRUE),
            Mean_Epi = mean(Epichlorohydran, na.rm = TRUE),
            Mean_Bro = mean(Bromine, na.rm = TRUE),
            Mean_Tur = mean(Turbidity, na.rm = TRUE)) |>
  print(n=127)

# At first, Ammonia and Nitrate columns (and others) were showing up as NA.
# I changed them to numeric by running this code snippet on each parameter:

River_Tox_Data$TSS <- as.numeric(as.character(River_Tox_Data$TSS))
sapply(River_Tox_Data$Ammonia, class)

River_Tox_Data$Ammonia <- as.numeric(as.character(River_Tox_Data$Ammonia))
sapply(River_Tox_Data$Ammonia, class)

River_Tox_Data$Nitrate <- as.numeric(as.character(River_Tox_Data$Nitrate))
sapply(River_Tox_Data$Nitrate, class)

River_Tox_Data$`Cr- Diss` <- as.numeric(as.character(River_Tox_Data$`Cr- Diss`))
sapply(River_Tox_Data$`Cr- Diss`, class)

River_Tox_Data$`Pb- Diss.` <- as.numeric(as.character(River_Tox_Data$`Pb- Diss.`))
sapply(River_Tox_Data$`Pb- Diss.`, class)

# Find mins; this function WILL produce a warning 
# if there are missing values (which there definitely are). 
# It won't actually prevent the code from running though.

River_Tox_Data |> 
  group_by(Site) |>
  summarise(Min_DO = min(DO, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |> 
  group_by(Site) |>
  summarise(Min_pH = min(`Chem pH`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Min_Cond = min(Conductivity, na.rm = TRUE),
            Min_Sal = min(Salinity, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Min_Alk = min(Alkalinity, na.rm = TRUE),
            Min_Hard = min(Hardness, na.rm = TRUE),
            Min_Chl = min(Chloride, na.rm = TRUE),
            Min_TSS = min(TSS, na.rm = TRUE),
            Min_NH4 = min(Ammonia, na.rm = TRUE),
            Min_NO3 = min(Nitrate, na.rm = TRUE),
            Min_NO2 = min(Nitrite, na.rm = TRUE))|>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Min_TKN = min(TKN, na.rm = TRUE),
            Min_PO4 = min(Phosphate, na.rm = TRUE),
            Min_P = min(Phosphorus, na.rm = TRUE),
            Min_SO4 = min(Sulfate, na.rm = TRUE),
            Min_BOD5 = min(BOD5, na.rm = TRUE),
            Min_COD = min(COD, na.rm = TRUE),
            Min_DOC = min(DOC, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Min_TOC = min(TOC, na.rm = TRUE),
            Min_Fl = min(Fluoride, na.rm = TRUE),
            Min_CNT = min(`CN-T`, na.rm = TRUE),
            Min_TRC = min(TRC, na.rm = TRUE),
            Min_TDS = min(TDS, na.rm = TRUE),
            Min_MBAS = min(MBAS, na.rm = TRUE),
            Min_OandG = min(`Oand G`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Min_Form = min(Formaldehyde, na.rm = TRUE),
            Min_CdD = min(`Cd- Diss.`, na.rm = TRUE),
            Min_CdT = min(`Cd-Tot.`, na.rm = TRUE),
            Min_Cr6 = min(`Cr- +6`, na.rm = TRUE),
            Min_CrD = min(`Cr- Diss`, na.rm = TRUE),
            Min_CrT = min(`Cr- Tot`, na.rm = TRUE),
            Min_CuD = min(`Cu- Diss`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Min_Cu_T = min(`Cu-Tot`, na.rm = TRUE),
            Min_Pb_D = min(`Pb- Diss.`, na.rm = TRUE),
            Min_Pb_T = min(`Pb- Tot`, na.rm = TRUE),
            Min_Ni_D = min(`Ni- Diss`, na.rm = TRUE),
            Min_Ni_T = min(`Ni-Tot`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Min_Zn_D = min(`Zn- Diss`, na.rm = TRUE),
            Min_Zn_T = min(`Zn-Tot`, na.rm = TRUE),
            Min_Fe_D = min(`Fe- Diss`, na.rm = TRUE),
            Min_Fe_T = min(`Fe- Tot`, na.rm = TRUE),
            Min_Mn_D = min(`Mn-Diss`, na.rm = TRUE),
            Min_Mn_T = min(`Mn- Tot`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Min_Al_D = min(`Al- Diss`, na.rm = TRUE),
            Min_Al_T = min(`Al- Tot`, na.rm = TRUE),
            Min_Tin = min(`Sn-Tot`, na.rm = TRUE),
            Min_Ag_D = min(`Ag- Diss`, na.rm = TRUE),
            Min_Ag_T = min(`Ag- Tot`, na.rm = TRUE),
            Min_Mo_D = min(`Mo-Diss`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Min_Mo_T = min(`Mo-Tot`, na.rm = TRUE),
            Min_Ca = min(Ca, na.rm = TRUE),
            Min_Mg = min(`Mg-Tot`, na.rm = TRUE)) |>
  print(n=127)

# Total Mercury and Titanium: Returns very small / weird values,
# displayed in scientific notation (or inaccurate) when in a larger tibble.
# Isolating just these parameter seems to work!

River_Tox_Data |>
  group_by(Site) |>
  summarise(Min_Hg = min(`Hg-Tot`, na.rm = TRUE),
            Min_Ti = min(`Ti-Tot`, na.rm = TRUE)) |>
print(n=127)  

River_Tox_Data |>
  group_by(Site) |>
  summarise(Min_V = min(`V-Tot`, na.rm = TRUE),
            Min_B = min(Boron, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Min_Co = min(`Co-Tot`, na.rm = TRUE),
            Min_Au = min(`Au-Tot`, na.rm = TRUE),
            Min_Na = min(`Na-T`, na.rm = TRUE),
            Min_Ba = min(`Ba-T`, na.rm = TRUE),
            Min_Sb = min(`Sb-Tot`, na.rm = TRUE),
            Min_As = min(`As-Tot`, na.rm = TRUE),
            Min_Be = min(`Be-Tot`, na.rm = TRUE),
            Min_Se = min(`Se-Tot`, na.rm = TRUE)) |>
  print(n=127)

# This had another weird value that might in Sci Not. Isolating it to be safe.

River_Tox_Data |>
  group_by(Site) |>
  summarise(Min_Sb = min(`Sb-Tot`, na.rm = TRUE)) |>
  print(n=127) 

River_Tox_Data |>
  group_by(Site) |>
  summarise(Min_Tl = min(Tl, na.rm = TRUE),
            Min_Hyd = min(Hydrazine, na.rm = TRUE),
            Min_1c2n = min(`1-chloro-2-nitrobenzene`, na.rm = TRUE),
            Min_Ace = min(Acetone, na.rm = TRUE),
            Min_AcM = min(Acrylamide, na.rm = TRUE),
            Min_AcN = min(Acrylonitrile, na.rm = TRUE),
            Min_Ani = min(Aniline, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Min_Az = min(Azobenzene, na.rm = TRUE),
            Min_Bnz = min(Benzene, na.rm = TRUE),
            Min_Bze = min(Benzidine, na.rm = TRUE),
            Min_BEHP = min(`Bis-2-ethyl hexyl phthlate`, na.rm = TRUE),
            Min_BD = min(bromodichloromethane, na.rm = TRUE),
            Min_Bm = min(Bromoform, na.rm = TRUE),
            Min_B13 = min(`Butadiene 1,3`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Min_But = min(Butanol, na.rm = TRUE),
            Min_BuI = min(`Butanol, iso`, na.rm = TRUE),
            Min_Cze = min(Carbazole, na.rm = TRUE),
            Min_CDS = min(`Carbon disulfide`, na.rm = TRUE),
            Min_CA2 = min(`Chloroaniline, 2-`, na.rm = TRUE),
            Min_CA4 = min(`Chloroaniline,4-`, na.rm = TRUE),
            Min_Cbz = min(chlorobenzene, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Min_Cfm = min(Chloroform, na.rm = TRUE),
            Min_Cme = min(chloromethane, na.rm = TRUE),
            Min_Cp2 = min(`Chlorophenol-2`, na.rm = TRUE),
            Min_DCn = min(Dichloran, na.rm = TRUE),
            Min_D14 = min(`Dichlorobenzene 1,4`, na.rm = TRUE),
            Min_D12 = min(`Dichlorobenzene, 1,2`, na.rm = TRUE),
            Min_Dbd = min(Dichlorobenzidine, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Min_D11 = min(`Dichloroethane 1,1`, na.rm = TRUE),
            Min_D12 = min(`Dichloroethane 1,2`, na.rm = TRUE),
            Min_Cis = min(`dichloroethylene, cis`, na.rm = TRUE),
            Min_Trn = min(`Dichloroethylene, trans`, na.rm = TRUE),
            Min_D13 = min(`Dicholorbenzene, 1,3`, na.rm = TRUE),
            Min_DiP = min(`Diisononyl phthlate`, na.rm = TRUE),
            Min_D33 = min(`Dimethylbenzidine 3,3,`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Min_Dio = min(Dioxane, na.rm = TRUE),
            Min_Ebz = min(Ethylbenzene, na.rm = TRUE),
            Min_I = min(Isopropylbenzene, na.rm = TRUE),
            Min_MK = min(MEK, na.rm = TRUE),
            Min_Mhl = min(Methanol, na.rm = TRUE),
            Min_MhA = min(Methylacrylate, na.rm = TRUE),
            Min_MCl = min(`methylene Cl`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Min_MTBE = min(MTBE, na.rm = TRUE),
            Min_Nap = min(Napthalene, na.rm = TRUE),
            Min_Np4 = min(`Nitrophenol, 4-`, na.rm = TRUE),
            Min_pcp = min(pentachlophenol, na.rm = TRUE),
            Min_PT = min(phenanthrene, na.rm = TRUE),
            Min_Phe = min(phenols, na.rm = TRUE),
            Min_Pp2 = min(`Propanol, 2-`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Min_Sty = min(Styrene, na.rm = TRUE),
            Min_Thf = min(Tetrahydrofuran, na.rm = TRUE),
            Min_TCa = min(Tetrachloroethane, na.rm = TRUE),
            Min_TCe = min(Tetrachloroethylene, na.rm = TRUE),
            Min_Tld = min(`toludine, m`, na.rm = TRUE),
            Min_Tle = min(Toluene, na.rm = TRUE),
            Min_111 = min(`Trichloroethane 1,1,1`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Min_Tce = min(Trichloroethene, na.rm = TRUE),
            Min_Tcp = min(`Trichlorophenol 2,4,6`, na.rm = TRUE),
            Min_VC = min(`Vinyl chloride`, na.rm = TRUE),
            Min_X = min(Xylene, na.rm = TRUE),
            Min_ETPH = min(ETPH, na.rm = TRUE),
            Min_TPH = min(TPH, na.rm = TRUE),
            Min_DEET = min(DEET, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Min_Car = min(Carbaryl, na.rm = TRUE),
            Min_BPA = min(`Bisphenol A`, na.rm = TRUE),
            Min_Caf = min(Caffiene, na.rm = TRUE),
            Min_Cop = min(Copranastol, na.rm = TRUE),
            Min_Cho = min(Cholesterol, na.rm = TRUE),
            Min_DNO = min(`Di-n-octylphthlalte`, na.rm = TRUE),
            Min_Non = min(Nonylphenol, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Min_VOC = min(VOCs, na.rm = TRUE),
            Min_AOX = min(AOX, na.rm = TRUE),
            Min_Fur = min(Furfural, na.rm = TRUE),
            Min_ON = min(`Organic Nitrogen`, na.rm = TRUE),
            Min_PCB = min(PCBs, na.rm = TRUE),
            Min_CA3 = min(`Chloroaniline, 3-`, na.rm = TRUE),
            Min_Dip = min(Diphenamid, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Min_Dib = min(Dibenzofuran, na.rm = TRUE),
            Min_Epi = min(Epichlorohydran, na.rm = TRUE),
            Min_Bro = min(Bromine, na.rm = TRUE),
            Min_Tur = min(Turbidity, na.rm = TRUE)) |>
  print(n=127)

# Find max's; again, this function will produce a warning.
# That just means there's no data in certain rows.

River_Tox_Data |> 
  group_by(Site) |>
  summarise(Max_DO = max(DO, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |> 
  group_by(Site) |>
  summarise(Max_pH = max(`Chem pH`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Max_Cond = max(Conductivity, na.rm = TRUE),
            Max_Sal = max(Salinity, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Max_Alk = max(Alkalinity, na.rm = TRUE),
            Max_Hard = max(Hardness, na.rm = TRUE),
            Max_Chl = max(Chloride, na.rm = TRUE),
            Max_TSS = max(TSS, na.rm = TRUE),
            Max_NH4 = max(Ammonia, na.rm = TRUE),
            Max_NO3 = max(Nitrate, na.rm = TRUE),
            Max_NO2 = max(Nitrite, na.rm = TRUE))|>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Max_TKN = max(TKN, na.rm = TRUE),
            Max_PO4 = max(Phosphate, na.rm = TRUE),
            Max_P = max(Phosphorus, na.rm = TRUE),
            Max_SO4 = max(Sulfate, na.rm = TRUE),
            Max_BOD5 = max(BOD5, na.rm = TRUE),
            Max_COD = max(COD, na.rm = TRUE),
            Max_DOC = max(DOC, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Max_TOC = max(TOC, na.rm = TRUE),
            Max_Fl = max(Fluoride, na.rm = TRUE),
            Max_CNT = max(`CN-T`, na.rm = TRUE),
            Max_TRC = max(TRC, na.rm = TRUE),
            Max_TDS = max(TDS, na.rm = TRUE),
            Max_MBAS = max(MBAS, na.rm = TRUE),
            Max_OandG = max(`Oand G`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Max_Form = max(Formaldehyde, na.rm = TRUE),
            Max_Cd_D = max(`Cd- Diss.`, na.rm = TRUE),
            Max_Cd_T = max(`Cd-Tot.`, na.rm = TRUE),
            Max_Cr_6 = max(`Cr- +6`, na.rm = TRUE),
            Max_Cr_D = max(`Cr- Diss`, na.rm = TRUE),
            Max_Cr_T = max(`Cr- Tot`, na.rm = TRUE),
            Max_Cu_D = max(`Cu- Diss`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Max_Cu_T = max(`Cu-Tot`, na.rm = TRUE),
            Max_Pb_D = max(`Pb- Diss.`, na.rm = TRUE),
            Max_Pb_T = max(`Pb- Tot`, na.rm = TRUE),
            Max_Ni_D = max(`Ni- Diss`, na.rm = TRUE),
            Max_Ni_T = max(`Ni-Tot`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Max_Zn_D = max(`Zn- Diss`, na.rm = TRUE),
            Max_Zn_T = max(`Zn-Tot`, na.rm = TRUE),
            Max_Fe_D = max(`Fe- Diss`, na.rm = TRUE),
            Max_Fe_T = max(`Fe- Tot`, na.rm = TRUE),
            Max_Mn_D = max(`Mn-Diss`, na.rm = TRUE),
            Max_Mn_T = max(`Mn- Tot`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Max_Al_D = max(`Al- Diss`, na.rm = TRUE),
            Max_Al_T = max(`Al- Tot`, na.rm = TRUE),
            Max_Tin = max(`Sn-Tot`, na.rm = TRUE),
            Max_Ag_D = max(`Ag- Diss`, na.rm = TRUE),
            Max_Ag_T = max(`Ag- Tot`, na.rm = TRUE),
            Max_Mo_D = max(`Mo-Diss`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Max_Mo_T = max(`Mo-Tot`, na.rm = TRUE),
            Max_Ca = max(Ca, na.rm = TRUE),
            Max_Mg = max(`Mg-Tot`, na.rm = TRUE),
            Max_Hg = max(`Hg-Tot`, na.rm = TRUE),
            Max_Ti = max(`Ti-Tot`, na.rm = TRUE),
            Max_V = max(`V-Tot`, na.rm = TRUE),
            Max_B = max(Boron, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Max_Co = max(`Co-Tot`, na.rm = TRUE),
            Max_Au = max(`Au-Tot`, na.rm = TRUE),
            Max_Na = max(`Na-T`, na.rm = TRUE),
            Max_Ba = max(`Ba-T`, na.rm = TRUE),
            Max_Sb = max(`Sb-Tot`, na.rm = TRUE),
            Max_As = max(`As-Tot`, na.rm = TRUE),
            Max_Be = max(`Be-Tot`, na.rm = TRUE),
            Max_Se = max(`Se-Tot`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Max_Tl = max(Tl, na.rm = TRUE),
            Max_Hyd = max(Hydrazine, na.rm = TRUE),
            Max_1c2n = max(`1-chloro-2-nitrobenzene`, na.rm = TRUE),
            Max_Ace = max(Acetone, na.rm = TRUE),
            Max_AcM = max(Acrylamide, na.rm = TRUE),
            Max_AcN = max(Acrylonitrile, na.rm = TRUE),
            Max_Ani = max(Aniline, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Man_Az = max(Azobenzene, na.rm = TRUE),
            Man_Bnz = max(Benzene, na.rm = TRUE),
            Man_Bze = max(Benzidine, na.rm = TRUE),
            Man_BEHP = max(`Bis-2-ethyl hexyl phthlate`, na.rm = TRUE),
            Man_BD = max(bromodichloromethane, na.rm = TRUE),
            Man_Bm = max(Bromoform, na.rm = TRUE),
            Man_B13 = max(`Butadiene 1,3`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Max_But = max(Butanol, na.rm = TRUE),
            Max_BuI = max(`Butanol, iso`, na.rm = TRUE),
            Max_Cze = max(Carbazole, na.rm = TRUE),
            Max_CDS = max(`Carbon disulfide`, na.rm = TRUE),
            Max_CA2 = max(`Chloroaniline, 2-`, na.rm = TRUE),
            Max_CA4 = max(`Chloroaniline,4-`, na.rm = TRUE),
            Max_Cbz = max(chlorobenzene, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Max_Cfm = max(Chloroform, na.rm = TRUE),
            Max_Cme = max(chloromethane, na.rm = TRUE),
            Max_Cp2 = max(`Chlorophenol-2`, na.rm = TRUE),
            Max_DCn = max(Dichloran, na.rm = TRUE),
            Max_D14 = max(`Dichlorobenzene 1,4`, na.rm = TRUE),
            Max_D12 = max(`Dichlorobenzene, 1,2`, na.rm = TRUE),
            Max_Dbd = max(Dichlorobenzidine, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Max_D11 = max(`Dichloroethane 1,1`, na.rm = TRUE),
            Max_D12 = max(`Dichloroethane 1,2`, na.rm = TRUE),
            Max_Cis = max(`dichloroethylene, cis`, na.rm = TRUE),
            Max_Trn = max(`Dichloroethylene, trans`, na.rm = TRUE),
            Max_D13 = max(`Dicholorbenzene, 1,3`, na.rm = TRUE),
            Max_DiP = max(`Diisononyl phthlate`, na.rm = TRUE),
            Max_D33 = max(`Dimethylbenzidine 3,3,`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Max_Dio = max(Dioxane, na.rm = TRUE),
            Max_Ebz = max(Ethylbenzene, na.rm = TRUE),
            Max_I = max(Isopropylbenzene, na.rm = TRUE),
            Max_MK = max(MEK, na.rm = TRUE),
            Max_Mhl = max(Methanol, na.rm = TRUE),
            Max_MhA = max(Methylacrylate, na.rm = TRUE),
            Max_MCl = max(`methylene Cl`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Max_MTBE = max(MTBE, na.rm = TRUE),
            Max_Nap = max(Napthalene, na.rm = TRUE),
            Max_Np4 = max(`Nitrophenol, 4-`, na.rm = TRUE),
            Max_pcp = max(pentachlophenol, na.rm = TRUE),
            Max_PT = max(phenanthrene, na.rm = TRUE),
            Max_Phe = max(phenols, na.rm = TRUE),
            Max_Pp2 = max(`Propanol, 2-`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Max_Sty = max(Styrene, na.rm = TRUE),
            Max_Thf = max(Tetrahydrofuran, na.rm = TRUE),
            Max_TCa = max(Tetrachloroethane, na.rm = TRUE),
            Max_TCe = max(Tetrachloroethylene, na.rm = TRUE),
            Max_Tld = max(`toludine, m`, na.rm = TRUE),
            Max_Tle = max(Toluene, na.rm = TRUE),
            Max_111 = max(`Trichloroethane 1,1,1`, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Max_Tce = max(Trichloroethene, na.rm = TRUE),
            Max_Tcp = max(`Trichlorophenol 2,4,6`, na.rm = TRUE),
            Max_VC = max(`Vinyl chloride`, na.rm = TRUE),
            Max_X = max(Xylene, na.rm = TRUE),
            Max_ETPH = max(ETPH, na.rm = TRUE),
            Max_TPH = max(TPH, na.rm = TRUE),
            Max_DEET = max(DEET, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Max_Car = max(Carbaryl, na.rm = TRUE),
            Max_BPA = max(`Bisphenol A`, na.rm = TRUE),
            Max_Caf = max(Caffiene, na.rm = TRUE),
            Max_Cop = max(Copranastol, na.rm = TRUE),
            Max_Cho = max(Cholesterol, na.rm = TRUE),
            Max_DNO = max(`Di-n-octylphthlalte`, na.rm = TRUE),
            Max_Non = max(Nonylphenol, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Max_VOC = max(VOCs, na.rm = TRUE),
            Max_AOX = max(AOX, na.rm = TRUE),
            Max_Fur = max(Furfural, na.rm = TRUE),
            Max_ON = max(`Organic Nitrogen`, na.rm = TRUE),
            Max_PCB = max(PCBs, na.rm = TRUE),
            Max_CA3 = max(`Chloroaniline, 3-`, na.rm = TRUE),
            Max_Dip = max(Diphenamid, na.rm = TRUE)) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarise(Max_Dib = max(Dibenzofuran, na.rm = TRUE),
            Max_Epi = max(Epichlorohydran, na.rm = TRUE),
            Max_Bro = max(Bromine, na.rm = TRUE),
            Max_Tur = max(Turbidity, na.rm = TRUE)) |>
  print(n=127)

# Find n: can't use the exact same code I used for min, max, or mean.
# Well, I can use a modified version of it. The sum function in this case will 
#   get the sum total of observations in this grouped list for each parameter
#   we ask it to calculate. 
# To have it exclude missing values, we use 'is.na'. This argument looks for 
#   null values, but by putting '!' in front of it,
#   we have it specifically look for NOT null values while ignoring the nulls.
# I'm gonna use this code to run through all the parameters,
#   but I'm only going to run 3 at a time plus the total observations
#   to QA as I go.

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            DO_Obs = sum(!is.na(DO)),
            pH_Obs = sum(!is.na(`Chem pH`)),
            Cond_Obs = sum(!is.na(Conductivity))) |>
print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            Sal_Obs = sum(!is.na(Salinity)),
            Alk_Obs = sum(!is.na(Alkalinity)),
            Hard_Obs = sum(!is.na(Hardness))) |>
  print(n=127) 

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            Chl_Obs = sum(!is.na(Chloride)),
            TSS_Obs = sum(!is.na(TSS)),
            NH4_Obs = sum(!is.na(Ammonia))) |>
  print(n=127) 

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            NO3_Obs = sum(!is.na(Nitrate)),
            NO2_Obs = sum(!is.na(Nitrite)),
            TKN_Obs = sum(!is.na(TKN))) |>
  print(n=127) 

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            PO4_Obs = sum(!is.na(Phosphate)),
            P_Obs = sum(!is.na(Phosphorus)),
            SO4_Obs = sum(!is.na(Sulfate))) |>
  print(n=127) 

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            BOD_Obs = sum(!is.na(BOD5)),
            COD_Obs = sum(!is.na(COD)),
            DOC_Obs = sum(!is.na(DOC))) |>
  print(n=127) 

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            TOC_Obs = sum(!is.na(TOC)),
            F_Obs = sum(!is.na(Fluoride)),
            CNT_Obs = sum(!is.na(`CN-T`))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            TRC_Obs = sum(!is.na(TRC)),
            TDS_Obs = sum(!is.na(TDS)),
            MBAS_Obs = sum(!is.na(MBAS))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            OnG_Obs = sum(!is.na(`Oand G`)),
            For_Obs = sum(!is.na(Formaldehyde)),
            CdD_Obs = sum(!is.na(`Cd- Diss.`))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            CdT_Obs = sum(!is.na(`Cd-Tot.`)),
            Cr6_Obs = sum(!is.na(`Cr- +6`)),
            CrD_Obs = sum(!is.na(`Cr- Diss`))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            CrT_Obs = sum(!is.na(`Cr- Tot`)),
            CuD_Obs = sum(!is.na(`Cu- Diss`)),
            CuT_Obs = sum(!is.na(`Cu-Tot`))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            PbD_Obs = sum(!is.na(`Pb- Diss.`)),
            PbT_Obs = sum(!is.na(`Pb- Tot`)),
            NiD_Obs = sum(!is.na(`Ni- Diss`))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            NiT_Obs = sum(!is.na(`Ni-Tot`)),
            ZiD_Obs = sum(!is.na(`Zn- Diss`)),
            ZnT_Obs = sum(!is.na(`Zn-Tot`))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            FeD_Obs = sum(!is.na(`Fe- Diss`)),
            FeT_Obs = sum(!is.na(`Fe- Tot`)),
            MnD_Obs = sum(!is.na(`Mn-Diss`))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            MnT_Obs = sum(!is.na(`Mn- Tot`)),
            AlD_Obs = sum(!is.na(`Al- Diss`)),
            AlT_Obs = sum(!is.na(`Al- Tot`))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            Tin_Obs = sum(!is.na(`Sn-Tot`)),
            AgD_Obs = sum(!is.na(`Ag- Diss`)),
            AgT_Obs = sum(!is.na(`Ag- Tot`))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            MoD_Obs = sum(!is.na(`Mo-Diss`)),
            MoT_Obs = sum(!is.na(`Mo-Tot`)),
            Ca_Obs = sum(!is.na(Ca))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            Mg_Obs = sum(!is.na(`Mg-Tot`)),
            Hg_Obs = sum(!is.na(`Hg-Tot`)),
            Ti_Obs = sum(!is.na(`Ti-Tot`))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            V_Obs = sum(!is.na(`V-Tot`)),
            B_Obs = sum(!is.na(Boron)),
            Co_Obs = sum(!is.na(`Co-Tot`))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            Au_Obs = sum(!is.na(`Au-Tot`)),
            Na_Obs = sum(!is.na(`Na-T`)),
            Ba_Obs = sum(!is.na(`Ba-T`))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            Sb_Obs = sum(!is.na(`Sb-Tot`)),
            As_Obs = sum(!is.na(`As-Tot`)),
            Be_Obs = sum(!is.na(`Be-Tot`))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            Se_Obs = sum(!is.na(`Se-Tot`)),
            Tl_Obs = sum(!is.na(Tl)),
            Hyd_Obs = sum(!is.na(Hydrazine))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            Ch1_Obs = sum(!is.na(`1-chloro-2-nitrobenzene`)),
            Ace_Obs = sum(!is.na(Acetone)),
            Acra_Obs = sum(!is.na(Acrylamide))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            Acro_Obs = sum(!is.na(Acrylonitrile)),
            Ani_Obs = sum(!is.na(Aniline)),
            Azo_Obs = sum(!is.na(Azobenzene))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            Benz_Obs = sum(!is.na(Benzene)),
            Benzi_Obs = sum(!is.na(Benzidine)),
            Bis_Obs = sum(!is.na(`Bis-2-ethyl hexyl phthlate`))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            bro_Obs = sum(!is.na(bromodichloromethane)),
            Bro_Obs = sum(!is.na(Bromoform)),
            B13_Obs = sum(!is.na(`Butadiene 1,3`))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            But_Obs = sum(!is.na(Butanol)),
            IBut_Obs = sum(!is.na(`Butanol, iso`)),
            Car_Obs = sum(!is.na(Carbazole))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            CDs_Obs = sum(!is.na(`Carbon disulfide`)),
            CA2_Obs = sum(!is.na(`Chloroaniline, 2-`)),
            CA4_Obs = sum(!is.na(`Chloroaniline,4-`))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            Cbz_Obs = sum(!is.na(chlorobenzene)),
            Cfm_Obs = sum(!is.na(Chloroform)),
            Cmth_Obs = sum(!is.na(chloromethane))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            ChlP_Obs = sum(!is.na(`Chlorophenol-2`)),
            Dicn_Obs = sum(!is.na(Dichloran)),
            DcBz_Obs = sum(!is.na(`Dichlorobenzene 1,4`))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            DCB12_Obs = sum(!is.na(`Dichlorobenzene, 1,2`)),
            DCBzd_Obs = sum(!is.na(Dichlorobenzidine)),
            DcE11_Obs = sum(!is.na(`Dichloroethane 1,1`))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            DCE12_Obs = sum(!is.na(`Dichloroethane 1,2`)),
            Cis_Obs = sum(!is.na(`dichloroethylene, cis`)),
            Trans_Obs = sum(!is.na(`Dichloroethylene, trans`))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            DBZ13_Obs = sum(!is.na(`Dicholorbenzene, 1,3`)),
            Dii_Obs = sum(!is.na(`Diisononyl phthlate`)),
            DMB33_Obs = sum(!is.na(`Dimethylbenzidine 3,3,`))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            Dio_Obs = sum(!is.na(Dioxane)),
            EthB_Obs = sum(!is.na(Ethylbenzene)),
            Iso_Obs = sum(!is.na(Isopropylbenzene))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            MEK_Obs = sum(!is.na(MEK)),
            Mthl_Obs = sum(!is.na(Methanol)),
            MthA_Obs = sum(!is.na(Methylacrylate))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            MthCl_Obs = sum(!is.na(`methylene Cl`)),
            MTBE_Obs = sum(!is.na(MTBE)),
            Nap_Obs = sum(!is.na(Napthalene))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            NP4_Obs = sum(!is.na(`Nitrophenol, 4-`)),
            PCP_Obs = sum(!is.na(pentachlophenol)),
            Phen_Obs = sum(!is.na(phenanthrene))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            Phen_Obs = sum(!is.na(phenols)),
            Prop_Obs = sum(!is.na(`Propanol, 2-`)),
            Sty_Obs = sum(!is.na(Styrene))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            TetHF_Obs = sum(!is.na(Tetrahydrofuran)),
            TEtha_Obs = sum(!is.na(Tetrachloroethane)),
            TEthy_Obs = sum(!is.na(Tetrachloroethylene))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            Tolud_Obs = sum(!is.na(`toludine, m`)),
            Tolue_Obs = sum(!is.na(Toluene)),
            TE111_Obs = sum(!is.na(`Trichloroethane 1,1,1`))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            TCE_Obs = sum(!is.na(Trichloroethene)),
            TP246_Obs = sum(!is.na(`Trichlorophenol 2,4,6`)),
            VC_Obs = sum(!is.na(`Vinyl chloride`))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            Xyl_Obs = sum(!is.na(Xylene)),
            EPTH_Obs = sum(!is.na(ETPH)),
            TPH_Obs = sum(!is.na(TPH))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            DEET_Obs = sum(!is.na(DEET)),
            CaRyl_Obs = sum(!is.na(Carbaryl)),
            BPA_Obs = sum(!is.na(`Bisphenol A`))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            Caf_Obs = sum(!is.na(Caffiene)),
            Cop_Obs = sum(!is.na(Copranastol)),
            Cho_Obs = sum(!is.na(Cholesterol))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            Dino_Obs = sum(!is.na(`Di-n-octylphthlalte`)),
            Non_Obs = sum(!is.na(Nonylphenol)),
            VOC_Obs = sum(!is.na(VOCs))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            AOX_Obs = sum(!is.na(AOX)),
            Fur_Obs = sum(!is.na(Furfural)),
            OrgN_Obs = sum(!is.na(`Organic Nitrogen`))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            PCB_Obs = sum(!is.na(PCBs)),
            ChlA3_Obs = sum(!is.na(`Chloroaniline, 3-`)),
            Diph_Obs = sum(!is.na(Diphenamid))) |>
  print(n=127)

River_Tox_Data |>
  group_by(Site) |>
  summarize(Total_Obs = n(),
            Dib_Obs = sum(!is.na(Dibenzofuran)),
            Epi_Obs = sum(!is.na(Epichlorohydran)),
            Brom_Obs = sum(!is.na(Bromine)),
            Tur_Obs = sum(!is.na(Turbidity))) |>
  print(n=127)

# export output

write_xlsx(River_Tox_Data,"NPDES_River_Chem.xlsx")
