## Code for 
#The influence of collection methods on phytoplankton enumeration using microscope and FlowCam analysis in coastal waters

# Libraries, functions and constants --------------------------------------
library(tidyverse)
library(vegan)
library(ggsignif)
library(ggpp)
library(vcd)
library(patchwork)
library(grateful)



##
# Constants and functions -------------------------------------------------


colorsforgraphs <- c(
  "#4B474D", "#E3E2E5", "#FA0022", "#FB0DFF", "#00FF32", "#1682FD", "#F89E00", "#D84788", "#E0EC00", "#00F9C8", "#00CCF3", 
  "#C487E2", "#49851C", "#803D00", "#A316FE", "#E3C582", "#004F8E", "#FE6A56", "#FCACB7", "#862A6D", "#229180", "#FC16C6", 
  "#FB007E", "#A1F777", "#B7F0BB", "#B600AE", "#B8C8FC", "#691CB5", "#FE91DC", "#FED000", "#706616", "#B48CB2", "#81F5F1", 
  "#EE8B6E", "#D866FE", "#0DFF95", "#8590EC", "#2622FF", "#B3C85D", "#A5897E", "#51869F", "#B91642", "#AC4F65", "#B9810D", 
  "#6E498D", "#22AFFD", "#809B73", "#47C479", "#9373FF", "#DE163B", "#FC0DA3", "#B7228D", "#FA87FB", "#A23816", "#0DC016", 
  "#FFDBC9", "#CB9172", "#FFBEFD", "#FF7787", "#A9D6E7", "#354F1C", "#8A22A0", "#92FB0D", "#794249", "#1C4BB5", "#4F9BD0", 
  "#C8BB00", "#788A8A", "#FC51EB", "#FFCFE4", "#FA8226", "#DEE1B1", "#99CBBA", "#FE8CB9", "#FF66D3", "#16B0BD", "#CB7CA7", 
  "#B655AC", "#60D2AC", "#0D8651", "#CD00FD", "#E6D5FA", "#A7D50D", "#8885C7", "#9B1642", "#9A00D3", "#A96EE6", "#F34700", 
  "#6C471C", "#9B9422", "#FB4B8D", "#6DB222", "#FCC569", "#704970", "#3D32D4", "#787599", "#0D4F60", "#00F0FE", "#1C4F42", 
  "#EB828C", "#AFEB91", "#6953AE", "#AA9F6D", "#DB229B", "#CE16D9", "#C7A8E4", "#FEB27E", "#9D1C62", "#B9B9A5", "#C8751C", 
  "#8FD9FF", "#BC7F81", "#7AB976", "#B92E0D", "#BBA7B2", "#82FB9B", "#6C6A58", "#8A6300", "#A770B1", "#FE5A73"
)

#This functions gets genus and species name and is 
lookup_table <- tibble::tibble(
  taxon = c(
    "Amphora", "Asterionellopsis", "Bacillaria", "Bacteriastrum", "Bellerochae", "Biddulphia",
    "Centric", "Cerataulina", "C.debilis", "C.decipiens", "C.curvistus", "C.radicans", "C.didymus",
    "C.radiatus", "C.socialis", "C.lauderi", "Chaetoceros", "C.diadema", "C.densus", "C.simplex","C.danicus",
    "C.teres", "C.lorenzianus", "C.eibenii", "C.criophilus", "Chaetognath", "Ciliate",
    "C.frauenfeldianum", "Copepod nauplii", "Corethron", "Coscinodiscus", "C.wailesii",
    "C.granii", "C.coninnus", "C.asteromphalus", "C.closterium", "Cyst", "D.surirella",
    "Detonula", "Dinoflagellate", "Dinophysis", "D.caudata", "Ditylum", "Eucampia", "E.turris",
    "Flagellate", "Guinardia", "Gymnodinium", "P.bipes", "Helicotheca", "Hemiaulus",
    "Heterocapsa", "Lauderia", "Leptocylindrus", "L.minimus", "L.danicus", "G.polyedra",
    "Lithodesmium", "Melosira", "Navicula", "N.scintillans", "Odontella", "Pennate",
    "Planktoniella", "Pleurosigma", "Proboscia", "Prorocentrum", "P.ovatum", "P.divergens",
    "Protoperidinium", "P.depressum", "P.grande", "P.pellucidum", "P.claudicans",
    "P.minutum", "P.pallidum", "P.denticulatum", "P.conicum", "P.conicoides", "P.steinii",
    "P.elegans", "P.nitzschia", "P.pseudonoctiluca", "P.fusiformis", "R.clevei",
    "Rhizosolenia", "Scrippsiella", "Silicoflagellate", "Skeletonema", "S.palmeriana",
    "Stephanopyxis", "Thalassionema", "T.anguste-lineata", "T.rotula", "Thalassiosira",
    "T.constricta", "T.subtilis", "Tintinnid", "Triceratium", "Trieres",
    "T.furca", "T.fusus", "Tripos", "T.lineatum", "T.trichoceros", "T.symmetricus",
    "T.tripos", "T.horridum", "T.massiliense", "T.longirostrum", "T.longpipes",
    "T.macroceros", "T.candelabrum", "T.pentagonus", "T.platycorne",
    "D.tripos", "Zooplankton", 
    "Gonyaulax","Pyrocystis","Cylindrotheca","Noctiluca","Pseudo-nitzschia","Eupyxidicula",
    "Probosica", "C.cf_compressus", "D.brightwellii","E.zondicus", "T.frauenfeldii", "A.glacialis",
    "C.contortus", "C.pelagica", "C.protuberans","C.peruvianus","D.pumila","Asteroplanus",
    "P.horologium", "D.lenticula", "G.spirale", "Katodinium", "P.pyriforme",
    "D.norvegica","Pyrophacus", "P.reticulatum", "Actinocyclus","Cryptophaceae", "D.fragilissimus",
    "L.undulatum","Porosira", "C.affinis", "Alexandrium", "A.ostenfeldii","G.undulans",
    NA
  ),
  genus = c(
    "Amphora", "Asterionellopsis", "Bacillaria", "Bacteriastrum", "Bellerochae", "Biddulphia",
    "Centric", "Cerataulina", "Chaetoceros", "Chaetoceros", "Chaetoceros", "Chaetoceros", "Chaetoceros",
    "Chaetoceros", "Chaetoceros", "Chaetoceros", "Chaetoceros", "Chaetoceros", "Chaetoceros", "Chaetoceros","Chaetoceros",
    "Chaetoceros", "Chaetoceros", "Chaetoceros", "Chaetoceros", "Chaetognath", "Ciliate",
    "Chaetoceros", "Copepod", "Corethron", "Coscinodiscus", "Coscinodiscus",
    "Coscinodiscus", "Coscinodiscus", "Coscinodiscus", "Cylindrotheca", "Cyst", "Delphineis",
    "Detonula", "Dinoflagellate", "Dinophysis", "Dinophysis", "Ditylum", "Eucampia", "Eupyxidicula",
    "Flagellate", "Guinardia", "Gymnodinium", "Protoperidinium", "Helicotheca", "Hemiaulus",
    "Heterocapsa", "Lauderia", "Leptocylindrus", "Leptocylindrus", "Leptocylindrus","Gonyaulax",
    "Lithodesmium", "Melosira", "Navicula", "Noctiluca", "Odontella", "Pennate",
    "Planktoniella", "Pleurosigma", "Proboscia", "Prorocentrum", "Protoperidinium", "Protoperidinium",
    "Protoperidinium", "Protoperidinium", "Protoperidinium", "Protoperidinium", "Protoperidinium",
    "Protoperidinium", "Protoperidinium", "Protoperidinium", "Protoperidinium", "Protoperidinium","Protoperidinium",
    "Protoperidinium", "Pseudo-nitzschia", "Pyrocystis", "Pyrocystis", "Rhizosolenia",
    "Rhizosolenia", "Scrippsiella", "Silicoflagellate", "Skeletonema", "Skeletonema",
    "Stephanopyxis", "Thalassionema", "Thalassiosira", "Thalassiosira", "Thalassiosira",
    "Thalassiosira", "Thalassiosira", "Tintinnid", "Triceratium", "Trieres",
    "Tripos", "Tripos", "Tripos", "Tripos", "Tripos", "Tripos",
    "Tripos", "Tripos", "Tripos", "Tripos", "Tripos",
    "Tripos", "Tripos", "Tripos", "Tripos",
    "Dinophysis", "Zooplankton", 
    "Gonyaulax","Pyrocystis","Cylindrotheca","Noctiluca","Pseudo-nitzschia","Eupyxidicula",
    "Probosica","Chaetoceros", "Ditylum", "Eucampia","Thalassionema", "Asterionellopsis",
    "Chaetoceros","Cerataulina", "Chaetoceros","Chaetoceros","Detonula","Asteroplanus",
    "Pyrophacus", "Diplopsalis", "Gyrodinium", "Katodinium", "Protoperidinium",
    "Dinophysis", "Pyrophacus", "Protoceratium","Actinocyclus", "Cryptophaceae", "Dactyliosolen",
    "Lithodesmium", "Porosira","Chaetoceros", "Alexandrium", "Alexandrium","Gyrodinium",
    NA
  ),
  functional_type =c(
    "Diatom", "Diatom", "Diatom", "Diatom", "Diatom", "Diatom",
    "Diatom", "Diatom", "Diatom", "Diatom", "Diatom", "Diatom", "Diatom",
    "Diatom", "Diatom", "Diatom", "Diatom", "Diatom", "Diatom", "Diatom","Diatom",
    "Diatom", "Diatom", "Diatom", "Diatom", "Zooplankton", "Zooplankton",
    "Diatom", "Zooplankton", "Diatom", "Diatom", "Diatom",
    "Diatom", "Diatom", "Diatom", "Diatom", "Cyst", "Diatom",
    "Diatom", "Dinoflagellate", "Dinoflagellate", "Dinoflagellate", "Diatom", "Diatom", "Diatom",
    "Flagellate", "Diatom", "Dinoflagellate", "Dinoflagellate", "Diatom", "Diatom",
    "Dinoflagellate", "Diatom", "Diatom", "Diatom", "Diatom","Dinoflagellate",
    "Diatom", "Diatom", "Diatom", "Dinoflagellate", "Diatom", "Diatom",
    "Diatom", "Diatom", "Diatom", "Dinoflagellate", "Dinoflagellate", "Dinoflagellate",
    "Dinoflagellate", "Dinoflagellate", "Dinoflagellate", "Dinoflagellate", "Dinoflagellate",
    "Dinoflagellate", "Dinoflagellate", "Dinoflagellate", "Dinoflagellate", "Dinoflagellate","Dinoflagellate",
    "Dinoflagellate", "Diatom", "Dinoflagellate", "Dinoflagellate", "Diatom",
    "Diatom", "Dinoflagellate", "Silicoflagellate", "Diatom", "Diatom",
    "Diatom", "Diatom", "Diatom", "Diatom", "Diatom",
    "Diatom", "Diatom", "Zooplankton", "Diatom", "Diatom",
    "Dinoflagellate", "Dinoflagellate", "Dinoflagellate", "Dinoflagellate", "Dinoflagellate", "Dinoflagellate",
    "Dinoflagellate", "Dinoflagellate", "Dinoflagellate", "Dinoflagellate", "Dinoflagellate",
    "Dinoflagellate", "Dinoflagellate", "Dinoflagellate", "Dinoflagellate",
    "Dinoflagellate", "Zooplankton", 
    "Dinoflagellate","Dinoflagellate","Diatom","Dinoflagellate","Diatom","Diatom",
    "Diatom", "Diatom", "Diatom", "Diatom", "Diatom","Diatom",
    "Diatom","Diatom","Diatom", "Diatom","Diatom","Diatom",
    "Dinoflagellate","Dinoflagellate", "Dinoflagellate", "Dinoflagellate","Dinoflagellate",
    "Dinoflagellate", "Dinoflagellate", "Dinoflagellate", "Diatom","Flagellate","Diatom",
    "Diatom", "Diatom","Diatom","Dinoflagellate","Dinoflagellate","Dinoflagellate",
    NA
  )
)

add_taxonomic_info <- function(data, lookup_table) {
  data %>%
    left_join(lookup_table, by = "taxon") %>% 
    mutate(
      genus = ifelse(is.na(genus), "Unknown", genus),
      functional_type = ifelse(is.na(functional_type), "Unknown", functional_type)
    )
}



##
# Importing in microscope and flowcam counts ------------------------------

m_data <- read.csv2("data/micro_counts.csv")
f_data <- read.csv2("data/flowcam_counts.csv")

m_data <- m_data %>%
  select(-X) %>% 
  mutate(tow_size = as.integer(tow_size),
         total_cells =  as.numeric(total_cells),
         sample_vol = as.numeric(sample_vol),
         dilution = as.numeric(dilution),
         vol_collected = as.numeric(vol_collected),
         area_chamber = as.numeric(area_chamber),
         area_counted = as.numeric(area_counted),
         lat = as.numeric(lat),
         lon = as.numeric(lon),
         functional_type2 = ifelse(functional_type %in% c("Diatom", "Dinoflagellate"),
                                  functional_type,
                                  "Other")
         )


f_data <- f_data %>%
  select(-X) %>% 
  mutate(functional_type2 = ifelse(functional_type %in% c("Diatom", "Dinoflagellate"),
                                   functional_type,
                                   "Other"))

str(f_data) 
str(m_data)

##
# Calculation from count to abs_abundance ---------------------------------
## microscope ##
m_phyconc <- m_data %>% 
  group_by(site, station,tow_size,taxon) %>%
  ###For the calculation it is reported as cells per liter
  reframe(abs_abun = case_when(
    tow_type == "U" ~ ((total_cells * area_chamber) / area_counted * (vol_collected/sample_vol)),
    tow_type == "H" ~ ((total_cells * sample_vol * area_chamber) / area_counted * (vol_collected)) / dilution)
  ) %>% 
  mutate(method = "microscope") 

m_phyconc %>%
  group_by(tow_size) %>%
  reframe(range = range(abs_abun))

#All volumes in the datasheet are in Liters
# sample_vol -
#for H  it is the volume concentrated to after net tow (Vulcan vol)
#for U it is the settled volume

# vol_waterfiltered - 
#for H it is the calculated volume filtered during net tow (2 min 2 knot area of net opening)
#for U it is the volume of the water settled 

## flowcam ##
f_phyconc <- f_data %>% 
  mutate(
    total_cells = as.numeric(total_cells),
    vol_sample = as.numeric(vol_sample),
    vol_anlaysed = as.numeric(vol_anlaysed),
    vol_collected = as.numeric(vol_collected),
    dilution = as.numeric(dilution)
  ) %>% 
  group_by(site, station,tow_size,taxon) %>%
  ###For the calculation it is reported as cells per liter
  reframe(abs_abun = case_when(
    tow_type == "U" ~ ((total_cells ) / (vol_sample / vol_anlaysed / vol_collected)),
    tow_type == "H" ~ ((total_cells) / (vol_sample / vol_anlaysed / vol_collected) / dilution))) %>% 
  mutate(method = "flowcam")


f_phyconc %>% 
  group_by(tow_size) %>%
  reframe(range = range(abs_abun))


#vol_sample - 
#volume of the vulcan tube or the settled volume of grab samples

#vol_anlaysed -
#Volume of sample that particle images were taken from 

#vol_collected - 
#volume of water collected during field sampling


######   Combining flowcam and microscope data ------------
#countdata - combination of flowcam and microscope counts
#concdata - combination of flowcam and microscope densities (absolute abundance) 


#raw count data 
countdata <-  m_data %>% 
  mutate(method = "microscope") %>% 
  full_join(f_data %>%  mutate(method = "flowcam",
                               vol_collected = as.numeric(vol_collected),
                               lat = as.numeric(lat),
                               lon = as.numeric(lon)
                               )) %>% 
  group_by(site, station,tow_size,method, taxon) %>% 
  reframe(total_cells = sum(total_cells))
  
countdata %>% 
  group_by(method, tow_size) %>% 
  reframe(aves = range(total_cells))

#abs abundance
concdata <- m_phyconc %>% 
  full_join(f_phyconc) %>% 
  group_by(site, station,tow_size,method, taxon) %>% 
  reframe(abs_abun = sum(abs_abun)) 

concdata %>%
  # filter(method == "flowcam") %>% 
  group_by(method,tow_size) %>% 
  reframe(range = range(abs_abun))


###
# Diversity indices calculation  ----------------------------------------
diversity_indices <- countdata %>%
  mutate(tow_size = as.factor(tow_size),
         method = as.factor(method)) %>% 
  group_by(site, station, tow_size, method) %>%
  summarise(Shannon = diversity(total_cells, index = "shannon"),
            richness = specnumber(total_cells),
            Pielou= Shannon/log(richness)) %>% 
  ungroup()

testdata_hillebrand <- countdata %>% 
  pivot_wider(id_cols = c(site,station, tow_size,method),
              values_fn = sum,
              names_from = taxon,
              values_fill = 0,
              values_from = total_cells)

testdata_hillebrand$S <- specnumber(testdata_hillebrand[,5:ncol(testdata_hillebrand)])
testdata_hillebrand$ENS <- diversity(testdata_hillebrand[,5:ncol(testdata_hillebrand)], "invsimpson")
summary(testdata_hillebrand[,c(1:4,98:99)])
# str(testdata_hillebrand)
# testdata_hillebrand[1:4]

renyi_methods <- renyi(testdata_hillebrand[,5:ncol(testdata_hillebrand)], scales = c(0, 1, 2),
                       hill = TRUE)

renyi_methods <- renyi_methods %>% rename(S.meth="0", expH.meth="1", ENS.meth="2")

div_renyi <- cbind(testdata_hillebrand[,c("site","station","tow_size","method")], renyi_methods)

div_renyi <- div_renyi %>% mutate(tow_size = as.factor(tow_size), 
                                  method =  as.factor(method))

# plot(div_renyi)
diversity_indices <- diversity_indices %>% right_join(div_renyi,
                                                     by = c("site","station","tow_size", "method"))

# Calculating relative abundance -----------------------------------------
conc_sum <- concdata %>% 
  group_by(site, station,tow_size, method,taxon) %>%
  reframe(total_conc_taxon = sum(abs_abun))

# Step 2: Calculate total number of species population (Σ Nsi) per group
total_conc_sum <- conc_sum %>% 
  group_by(site, station,tow_size,method) %>%
  reframe(total_conc = sum(total_conc_taxon, na.rm = TRUE))

# Step 3: Calculate relative abundance (%)
rela_abun <- conc_sum %>%
  left_join(total_conc_sum, by = c("site","station","tow_size", "method")) %>%
  mutate(rela_abun_densi = (total_conc_taxon / total_conc)*100)

###
# Create data frame to use for analysis ------------------------------------

ays_data <- concdata  %>% 
  select(site, station,tow_size,method,taxon,abs_abun) %>% 
  left_join(countdata %>% select(site, station, tow_size,method,taxon,total_cells)) %>% 
  left_join(rela_abun %>% select(site, station, tow_size,method,taxon,rela_abun_densi)) %>% 
  left_join(diversity_indices %>% mutate(tow_size = as.integer(as.character(tow_size)))) %>% 
  as_tibble()

ays_data <- ays_data %>% 
  mutate(taxon = case_when(
    taxon == "Noctiluca" ~ "N.scintillans",
    taxon == "Pseudo-nitzschia" ~ "P.nitzschia",
    TRUE ~ taxon
  ))

ays_data <- add_taxonomic_info(ays_data,lookup_table)

ays_data <- ays_data %>% filter(!functional_type %in% c("Zooplankton", "Cyst"))

ays_data <- ays_data %>% mutate(tow_size = as.factor(tow_size),
                                functional_type2 = ifelse(functional_type %in% c("Diatom", "Dinoflagellate"),
                                                         functional_type,
                                                         "Other"))

ays_data <- ays_data %>%
  mutate(size_class = case_when(
    taxon %in% c("Cryptophaceae", "Flagellate", "Heterocapsa", "Katodinium", 
                 "Prorocentrum", "Scrippsiella") ~ "Nanophytoplankton",
    taxon %in% c("Actinocyclus", "Asterionellopsis", "A.glacialis", 
                 "Asteroplanus", "Bacteriastrum", "Chaetoceros", 
                 "Corethron", "Coscinodiscus", "Detonula", "Ditylum", 
                 "Eucampia", "Guinardia", "Helicotheca", "Hemiaulus", 
                 "Leptocylindrus", "Lithodesmium", "Odontella", 
                 "Pleurosigma", "Porosira", "Rhizosolenia", 
                 "Skeletonema", "Stephanopyxis", "Thalassionema", 
                 "Thalassiosira", "Alexandrium", "A.ostenfeldii", 
                 "Dinoflagellate", "Dinophysis", "Gonyaulax", 
                 "Gymnodinium", "Noctiluca", "Protoperidinium", 
                 "Tripos", "Cyst", "Pyrophacus", "Silicoflagellate","P.depressum",
                 "C.pelagica", "C.affinis", "C.didymus", "C.simplex", "C.cf_compressus", "E.turris",
                 "C.curvistus", "C.decipiens", "C.debilis", "C.densus", "C.contortus", "P.horologium",
                 "C.diadema", "C.radicans", "C.lauderi", "C.protuberans", "C.peruvianus", 
                 "C.socialis", "C.wailesii", "C.closterium", "D.fragilissimus", "D.pumila", 
                 "D.norvegica", "D.lenticula", "D.brightwellii", "E.zondicus", "P.bipes", 
                 "G.spirale", "G.undulans", "L.minimus", "C.danicus", "L.danicus", 
                 "G.polyedra", "L.undulatum", "P.pyriforme", "N.scintillans", "Odontella", 
                 "Pennate", "P.ovatum", "P.reticulatum", "P.pallidum", "P.claudicans", 
                 "P.conicum", "P.divergens", "P.denticulatum", "P.depessum", "P.nitzschia", 
                 "T.frauenfeldii", "T.anguste-lineata", "T.constricta", "T.rotula", 
                 "T.lineatum", "T.furca", "T.tripos", "T.macroceros") ~ "Microphytoplankton",
    TRUE ~ "Other"  # For taxa not classified
  ))


rare_sp <- ays_data %>%
  filter(abs_abun < 1) %>%
  distinct(method, tow_size, taxon) 

ays_data %>% 
  distinct(taxon) %>% 
  reframe(count = n())

write.csv(ays_data %>% 
            distinct(taxon),
          "taxon.csv")


ays_data %>% 
  filter(total_cells!= 0) %>% 
  group_by(size_class, method) %>% 
  reframe(tot = sum(total_cells))

ays_data %>% 
  group_by(functional_type) %>% 
  reframe(tot = sum(total_cells))

#unique taxa per method
ays_data %>% 
  distinct(method,tow_size, taxon) %>%
  group_by(taxon) %>%
  filter(n_distinct(tow_size,method) == 1) %>%
  ungroup() %>% 
  # group_by(method,tow_size) %>%
  # summarise(n_taxa = n_distinct(taxon), .groups = 'drop') %>%
  # pivot_wider(names_from = c(method,tow_size),
  #             values_from = n_taxa)
  pivot_wider(names_from = c(tow_size),
            values_from = taxon,
            values_fn =list(taxon = ~ paste(., collapse = ", "))) -> utaxa_meth_names

write.csv(utaxa_meth_names, "UniTaxa_methods.csv")


ays_data %>% 
  distinct(method,tow_size, taxon) %>%
  group_by(taxon) %>%
  ungroup() %>% 
  arrange(taxon) %>% 
  pivot_wider(names_from = c(tow_size),
              values_from = taxon,
              values_fn =list(taxon = ~ paste(., collapse = ", ")))->taxapermethod

write.csv(taxapermethod, "Taxa_methods.csv")

ays_data %>% 
  group_by(size_class) %>%
  distinct(taxon) %>%
  ungroup() %>% 
  arrange(taxon) %>% 
  pivot_wider(names_from = c(size_class),
              values_from = taxon,
              values_fn =list(taxon = ~ paste(., collapse = ", "))) -> sizeclass


write.csv(sizeclass, "SizeClass_Taxa.csv")


ays_data %>% 
  group_by(method , tow_size) %>% 
  filter(abs_abun>0) %>% 
  reframe(mean_aa = median(abs_abun))


ays_data %>%
  filter(abs_abun > 0) %>% 
  group_by(method, tow_size) %>% 
  summarise(taxon_count = n_distinct(genus), .groups = "drop") %>%  #change genus to taxon
  pivot_wider(names_from = c(method),
              values_from = taxon_count)
ays_data %>% 
  group_by(method,functional_type) %>%
  reframe(n_taxa = n_distinct(taxon)) %>% 
  pivot_wider(names_from = c(method),
              values_from = n_taxa)

##
# Proportion plot - Figure 1 ----------------------------------------------
ggplot(data = ays_data %>% 
         mutate(functional_type = ifelse(functional_type %in% c("Diatom", "Dinoflagellate"), 
                                        functional_type, 
                                        "Other")),
       aes(x = tow_size, y = rela_abun_densi, fill = taxon)) +
  geom_bar(stat = "identity", width = 0.8, position = "fill") +
  scale_fill_manual(name = "", values = colorsforgraphs) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c("0" = "water grab",
                              "20" = "20 µm net",
                              "55" = "55 µm net")) +
  labs(title = "",
       x ="",
       y = "") +
  facet_grid(functional_type ~ method,scale =  "free_y") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey70"),
        panel.grid.minor = element_line(color = "grey80"),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5, color = "black"),
        legend.key = element_rect(fill = "transparent"),
        legend.title = element_text(size = 12),
        strip.background = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.8, "cm"), 
        legend.key.height = unit(0.1,"cm"),
        legend.byrow = TRUE,
        axis.text.y = element_text(size = 12, color = "black"),
        strip.placement = "outside",
        text = element_text(family = "Arial")) +
  guides(fill=guide_legend(byrow=TRUE, ncol = 11, nrow= 9))

ggsave(filename = "plots/Figure 1.jpg", width = 20, height = 12, dpi = 600)

#


# count plot  -------------------------------------------------------------


count_long <- ays_data %>%
  mutate(sample_id = paste(method, tow_size, sep = "_")) %>% 
  group_by(sample_id, genus) %>%
  summarise(total_abundance = sum(abs_abun, na.rm = TRUE)) %>%
  ungroup() %>% 
  pivot_wider(names_from = sample_id, values_from = total_abundance, values_fill = 0) %>% 
  pivot_longer(-genus, names_to = "sample_id", values_to = "count") %>% 
  separate(sample_id, into = c("method", "tow_size")) %>% 
  filter(!(method == "flowcam" & count == 0))

ggplot(count_long,
       aes(x = tow_size, y = genus, fill = log10(count+1))) +
  geom_tile() +
  facet_wrap(~method, scales = "free_y",
             labeller = labeller(method = c(
               "flowcam" = "FlowCam 8000",
               "microscope" = "Microscope"
             ))) +
  scale_fill_viridis_c() +
  scale_x_discrete(labels = wrap_labels(
    c("Water grab",
      "20µm net",
      "55µm net"), width =12, new.line="\n")) +
  labs(x = "", y = "", fill = expression(log[10]("count"))) +
  theme(
    axis.text.x = element_text(size = 12, angle = 0,vjust=0.5,hjust=0.5, color = "black"),
    axis.title.x = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey80"),
    legend.position = "right",
    axis.title = element_text(size = 12),
    legend.key = element_rect(fill = "transparent"),
    panel.grid.major.y = element_line(color = "grey75"),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.text.y = element_text(size = 8),
    strip.placement = "outside")

ggplot(ays_data %>%
         mutate(
           tow_size = factor(
             tow_size,
             levels = c(0, 20, 55),                      
             labels = c("water grab", "20 µm net", "55 µm net"))) %>% 
  filter(!(total_cells == 0)), 
       aes(x = taxon, y = log10(total_cells+1), fill = tow_size)) +
  geom_boxplot() +
  facet_wrap(method~., scales = "free_x",
             labeller = labeller(method = c(
               "flowcam" = "FlowCam 8000",
               "microscope" = "Microscope"
             ))) +
  labs(x = "", y = "", title = "", fill = "") +
  theme(
    axis.text.x = element_text(size = 12, angle = 90, vjust=0.5,hjust=0.5, color = "black"),
    axis.title.x = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey80"),
    legend.position = "right",
    axis.title = element_text(size = 12),
    legend.key = element_rect(fill = "transparent"),
    panel.grid.major.y = element_line(color = "grey75"),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.text.y = element_text(size = 8),
    strip.placement = "outside")


ggplot(ays_data %>% 
         mutate(ts_meth = paste(tow_size, method, sep = "_")) %>% 
         unite(ngrps, c(tow_size,method,genus)) %>%
         mutate(functional_type = ifelse(functional_type %in% c("Diatom", "Dinoflagellate"), 
                                         functional_type, 
                                         "Other")) %>% 
         filter(total_cells > 0),
       aes(x = ts_meth, y = log(total_cells), fill = ts_meth)) +
  geom_boxplot() +
  geom_jitter(aes(color = functional_type)) +
  scale_x_discrete(labels = wrap_labels(
    c("Water grab FlowCam",
      "Water grab Microscope",
      "20µm net FlowCam",
      "20µm net Microscope",
      "55µm net FlowCam",
      "55µm net Microscope"), width =12, new.line="\n"))+
  scale_fill_manual(values = c("#1f77b4", "#2ca02c", "#17becf",
                               "#bcbd22", "#80b1d3", "#91cf60"),
                    name = "Collection method",
                    labels = c("Water grab, FlowCam",
                               "Water grab, Utermöhl",
                               "20µm net, FlowCam",
                               "20µm net, Sedgewick-Rafter",
                               "55µm net, FlowCam",
                               "55µm net, Sedgewick-Rafter")) +
  # facet_grid(functional_type~., scales = "free_x") +
  labs(subtitle = "",
       x = "",
       y = expression(log("counts"))) +
  theme(
    axis.text.x = element_text(size = 12, angle = 0,vjust=0.5,hjust=0.5, color = "black"),
    axis.title.x = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey80"),
    axis.title = element_text(size = 12),
    legend.position = "none",
    legend.key = element_rect(fill = "transparent"),
    panel.grid.major.y = element_line(color = "grey75"),
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    strip.placement = "outside") +
  geom_signif(test =  "wilcox.test",
              comparisons = list(
                c( "0_flowcam","0_microscope"),
                c( "20_flowcam","20_microscope"),
                c( "55_flowcam","55_microscope")),
              map_signif_level = TRUE, step_increase = 0,
              y_position = 6, vjust = 0, textsize = 4)





ggplot(ays_data %>% 
         mutate(ts_meth = paste(tow_size, method, sep = "_"),
                ts_meth = factor(ts_meth, levels = c("0_flowcam", "20_flowcam", "55_flowcam",
                                                     "0_microscope", "20_microscope", "55_microscope"))) %>% 
         unite(ngrps, c(tow_size,method,genus)) %>%
         mutate(functional_type = ifelse(functional_type %in% c("Diatom", "Dinoflagellate"), 
                                         functional_type, 
                                         "Other")) %>% 
         filter(total_cells > 0),
       aes(x = ts_meth, y = log(total_cells), fill = ts_meth)) +
  geom_boxplot() +
  geom_jitter(aes(color = functional_type)) +
  scale_x_discrete(labels = wrap_labels(
    c("Water grab FlowCam",
      "20µm net FlowCam",
      "55µm net FlowCam",
      "Water grab Microscope",
      "20µm net Microscope",
      "55µm net Microscope"), width =12, new.line="\n"))+
  scale_fill_manual(values = c("#1f77b4", "#80b1d3", "#17becf",
                               "#bcbd22", "#2ca02c", "#91cf60")) +
  # facet_grid(functional_type~., scales = "free_x") +
  labs(subtitle = "",
       x = "",
       y = expression(log("counts"))) +
  theme(
    axis.text.x = element_text(size = 12, angle = 0,vjust=0.5,hjust=0.5, color = "black"),
    axis.title.x = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey80"),
    axis.title = element_text(size = 12),
    legend.position = "right",
    legend.key = element_rect(fill = "transparent"),
    panel.grid.major.y = element_line(color = "grey75"),
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    strip.placement = "outside") +
  guides(fill = "none") +
  geom_signif(test =  "wilcox.test",
              comparisons = list(
                c( "0_flowcam","20_flowcam"),
                c( "0_flowcam","55_flowcam"),
                c( "0_microscope","20_microscope"),
                c( "0_microscope","55_microscope")),
              map_signif_level = TRUE, step_increase = 0.1,
              y_position = 6, vjust = 0, textsize = 4)


##
# Absolute abundance - Figure 2a---------------------------------------
ggplot(ays_data %>% 
         mutate(ts_meth = paste(tow_size, method, sep = "_"),
                ts_meth = factor(ts_meth, levels = c("0_flowcam", "20_flowcam", "55_flowcam",
                                                     "0_microscope", "20_microscope", "55_microscope"))) %>% 
         mutate(functional_type = ifelse(functional_type %in% c("Diatom", "Dinoflagellate"), 
                                        functional_type, 
                                        "Other")),
       aes(x = ts_meth, y = log10(abs_abun), fill = ts_meth)) +
  geom_boxplot() +
  scale_x_discrete(labels = wrap_labels(
    c("Water grab FlowCam",
      "20µm net FlowCam",
      "55µm net FlowCam",
      "Water grab Microscope",
      "20µm net Microscope",
      "55µm net Microscope"), width =12, new.line="\n"))+
  scale_fill_manual(values = c("#1f77b4", "#80b1d3", "#17becf",
                               "#bcbd22", "#2ca02c", "#91cf60")) +
  labs(subtitle = "",
       x = "",
       y = expression(log[10]("Abs. abundance"))) +
  theme(
    axis.text.x = element_text(size = 12, angle = 45,vjust=0.5,hjust=0.5, color = "black"),
    axis.title.x = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey80"),
    legend.position = "none",
    axis.title = element_text(size = 12),
    legend.key = element_rect(fill = "transparent"),
    panel.grid.major.y = element_line(color = "grey75"),
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    strip.placement = "outside",
    text = element_text(family = "Arial")) +
  geom_signif(test =  "wilcox.test",
              comparisons = list(
                c( "0_flowcam","20_flowcam"),
                c( "0_flowcam","55_flowcam"),
                c( "0_microscope","20_microscope"),
                c( "0_microscope","55_microscope")),
              map_signif_level = TRUE, step_increase = 0.1,
              y_position = 6, vjust = 0, textsize = 4) +
  ggtitle("a - absolute abundance") -> abs_abun_kwplot1

##
# Relative abundance plot - Figure 2b ------------------------------

ays_data <- ays_data %>%
  mutate(rela_abun_densi = replace_na(rela_abun_densi, 0))

range(ays_data$rela_abun_densi)

hist(log10(ays_data$rela_abun_densi))

ggplot(ays_data %>% 
         mutate(ts_meth = paste(tow_size, method, sep = "_"),
                ts_meth = factor(ts_meth, levels = c("0_flowcam", "20_flowcam", "55_flowcam",
                                                     "0_microscope", "20_microscope", "55_microscope"))) %>% 
         mutate(functional_type = ifelse(functional_type %in% c("Diatom", "Dinoflagellate"), 
                                        functional_type, 
                                        "Other")),
       aes(x = ts_meth, y = log10(rela_abun_densi), fill = ts_meth)) +
  geom_boxplot() +
  scale_x_discrete(labels = wrap_labels(
    c("Water grab FlowCam",
      "20µm net FlowCam",
      "55µm net FlowCam",
      "Water grab Microscope",
      "20µm net Microscope",
      "55µm net Microscope"), width =12, new.line="\n"))+
  scale_fill_manual(values = c("#1f77b4", "#80b1d3", "#17becf",
                               "#bcbd22", "#2ca02c", "#91cf60")) +
  labs(subtitle = "",
       x = "",
       y = expression(log[10]("Relative abundance"))) +
  theme(
    axis.text.x = element_text(size = 12, angle = 45,vjust=0.5,hjust=0.5, color = "black"),
    axis.title.x = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey80"),
    legend.position = "none",
    axis.title = element_text(size = 12),
    legend.key = element_rect(fill = "transparent"),
    panel.grid.minor.x = element_line(color = "black"),
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.text.y = element_text(size = 12, color = "black"),
    strip.placement = "outside",
    text = element_text(family = "Arial")) +
  geom_signif(test =  "wilcox.test",
              comparisons = list(
                c( "0_flowcam","20_flowcam"),
                c( "0_flowcam","55_flowcam"),
                c( "0_microscope","20_microscope"),
                c( "0_microscope","55_microscope")),
              map_signif_level = TRUE, step_increase = 0.1,
              y_position = 6, vjust = 0, textsize = 4) +
  ggtitle("b - relative abundance") -> rela_abun_kwplot1


##
#####  Figure 2 - method paper-------
#Plot of kw for absolute and relative abundance 
kw_abun_plots <- abs_abun_kwplot1 + 
  rela_abun_kwplot1 +  
  plot_layout(guides = "collect", ncol= 2, nrow = 1)


#print(kw_abun_plots)
ggsave(filename = "plots/Figure 2.jpg", width = 12, height = 8, dpi = 600)

###
# Species richness plots --------------------------------------------------

ggplot(diversity_indices %>% 
           mutate(ngrps = paste(tow_size,method, sep = "_"),
                  ngrps = factor(ngrps, levels = c("0_flowcam", "20_flowcam", "55_flowcam",
                                                   "0_microscope", "20_microscope", "55_microscope"))),
       aes(x = ngrps, y = S.meth, fill = ngrps)) +
  geom_boxplot() +
  scale_x_discrete(labels = wrap_labels(
    c("Water grab FlowCam",
      "20µm net FlowCam",
      "55µm net FlowCam",
      "Water grab Microscope",
      "20µm net Microscope",
      "55µm net Microscope"), width =12, new.line="\n"))+
  scale_fill_manual(values = c("#1f77b4", "#80b1d3", "#17becf",
                               "#bcbd22", "#2ca02c", "#91cf60")) +
  labs(title = "",
       x = "",
       y = "Num. Species") +
  theme(axis.text.x = element_text(size = 12, angle = 45,vjust=0.5,hjust=0.5, color = "black"),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = "none",
        axis.title = element_text(size = 12),
        legend.key = element_rect(fill = "transparent"),
        panel.grid.major.x = element_line(color = "grey75"),
        panel.grid.major.y = element_line(color = "grey75"),
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.text.y = element_text(size = 12,colour = "black"),
        strip.placement = "outside",
        text = element_text(family = "Arial")) +
  geom_signif(test =  "wilcox.test",
              comparisons = list(
                c( "0_flowcam","20_flowcam"),
                c( "0_flowcam","55_flowcam"),
                c( "0_microscope","20_microscope"),
                c( "0_microscope","55_microscope")),
              map_signif_level = TRUE, step_increase = 0.1,
              y_position = 33, vjust = 0, textsize = 4) +
  ggtitle("a - species richness") -> sprich_plot1



#exp(H') 
ggplot(ays_data %>% 
         mutate(ngrps = paste(tow_size,method, sep = "_"),
                ngrps = factor(ngrps, levels = c("0_flowcam", "20_flowcam", "55_flowcam",
                                                 "0_microscope", "20_microscope", "55_microscope"))),
       aes(x = ngrps, y = expH.meth, fill = ngrps)) +
  geom_boxplot() +
  scale_x_discrete(labels = wrap_labels(
    c("Water grab FlowCam",
      "20µm net FlowCam",
      "55µm net FlowCam",
      "Water grab Microscope",
      "20µm net Microscope",
      "55µm net Microscope"), width =12, new.line="\n"))+
  scale_fill_manual(values = c("#1f77b4", "#80b1d3", "#17becf",
                               "#bcbd22", "#2ca02c", "#91cf60")) +
  labs(title = "",
       x = "",
       y = "exp(H')") +
  theme(axis.text.x = element_text(size = 12, angle = 45,vjust=0.5,hjust=0.5, color = "black"),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = "none",
        axis.title = element_text(size = 12),
        legend.key = element_rect(fill = "transparent"),
        panel.grid.major.x = element_line(color = "grey75"),
        panel.grid.major.y = element_line(color = "grey75"),
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.text.y = element_text(size = 12,colour = "black"),
        strip.placement = "outside",
        text = element_text(family = "Arial")) +
  geom_signif(test =  "wilcox.test",
              comparisons = list(
                c( "0_flowcam","20_flowcam"),
                c( "0_flowcam","55_flowcam"),
                c( "0_microscope","20_microscope"),
                c( "0_microscope","55_microscope")),
              map_signif_level = TRUE, step_increase = 0.1,
              vjust = 0, textsize = 4) + 
  ggtitle("b - exp(Shannon-Weiner) diversity") -> shan_plot1

#1/D
ggplot(ays_data %>% 
         mutate(ngrps = paste(tow_size,method, sep = "_"),
                ngrps = factor(ngrps, levels = c("0_flowcam", "20_flowcam", "55_flowcam",
                                                 "0_microscope", "20_microscope", "55_microscope"))),
       aes(x = ngrps, y = ENS.meth, fill = ngrps)) +
  geom_boxplot() +
  scale_x_discrete(labels = wrap_labels(
    c("Water grab FlowCam",
      "20µm net FlowCam",
      "55µm net FlowCam",
      "Water grab Microscope",
      "20µm net Microscope",
      "55µm net Microscope"), width =12, new.line="\n"))+
  scale_fill_manual(values = c("#1f77b4", "#80b1d3", "#17becf",
                               "#bcbd22", "#2ca02c", "#91cf60")) +  
  labs(title = "",
       x = "",
       y = "1/D") +
  theme(
    axis.text.x = element_text(size = 12, angle = 45,vjust=0.5,hjust=0.7, color = "black"),
    axis.title.x = element_blank(),
    panel.background = element_rect(fill = "white"),
    legend.position = "none",
    axis.title = element_text(size = 12),
    legend.key = element_rect(fill = "transparent"),
    panel.grid.major.x = element_line(color = "grey75"),
    panel.grid.major.y = element_line(color = "grey75"), 
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.text.y = element_text(size = 12,colour = "black"),
    strip.placement = "outside",
    text = element_text(family = "Arial")) +
  geom_signif(test =  "wilcox.test",
              comparisons = list(
                c( "0_flowcam","20_flowcam"),
                c( "0_flowcam","55_flowcam"),
                c( "0_microscope","20_microscope"),
                c( "0_microscope","55_microscope")),
              map_signif_level = TRUE, step_increase = 0.1,
              vjust = 0, textsize = 4) +
  ggtitle("c - Inverse Simpson Index") -> piel_plot1

##
#####  Figure 3 - method paper-------
#Species richness and diversity plots showing stats
com_kw_plots <- (sprich_plot1+
                   shan_plot1 +
                   piel_plot1) +
  plot_layout(guides = "collect", ncol = 3, nrow = 1)


print(com_kw_plots)


ggsave(filename = "plots/Figure 3.jpg", width = 12, height = 8, dpi = 600)




###
####

# NMDS --------------------------------------------------------------------
nmds_data <-  ays_data %>% 
  filter(method == "microscope") %>% 
  select(site, station, tow_size,method,taxon,rela_abun_densi ) %>% 
  pivot_wider(names_from  = taxon,
              values_from = rela_abun_densi ,
              values_fn = list(rela_abun_densi  = sum),
              values_fill = list(rela_abun_densi  = 0)) %>% 
  arrange(site, station,tow_size,method)

groupdata <- nmds_data  %>%
  group_by(tow_size,method) %>% 
  mutate(groups = cur_group_id()) %>% 
  ungroup()

adun_data <- groupdata[,6:ncol(groupdata)-1]
adun_data[is.na(adun_data)] <- 0
info_data <- groupdata[,c(1:3,last(ncol(groupdata)))]

nmds <- metaMDS(adun_data,distance = "bray")

plot(nmds)

data.scores <-  as.data.frame(scores(nmds)$sites)

data.scores$sample = nmds_data$sample
data.scores$tow_size  = nmds_data$tow_size
data.scores$method = nmds_data$method

##must run code above for nmds creation
ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes( color = tow_size, 
                            shape = method),
             position = position_jitter()) +
  # geom_text(aes(label = sample), hjust = -0.2, vjust = -0.2, size = 3) +
  scale_color_manual(values = c("#1f77b4", "#2ca02c", "#17becf"),
                     labels = wrap_labels(
                       c("Water grab",
                         "20 µm net",
                         "55 µm net"))) +
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", 
        axis.title.y = element_text(face = "bold", size = 12), 
        axis.title.x = element_text(face = "bold", size = 12, colour = "black"), 
        legend.title = element_text(size = 12, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank(),
        text = element_text(family = "Arial")) + 
  labs(x = "NMDS1", colour = "", y = "NMDS2", shape = "") +
  annotate("text", x = 0.8, y = -1, label = paste("Stress =", round(nmds$stress, 4)),
           hjust = 1, vjust = 1, size = 4, color = "black") +
  guides(shape = "none") +
  ggtitle("a - microscope analysis ") -> nmds_plot1



#####  Figure 4 - method paper-------
#NMDS PLOTS
nmds_plots <- (nmds_plot1+
                   nmds_plot2) +
  plot_layout(guides = "collect", ncol = 2, nrow = 1) & 
  theme(legend.position = "bottom")

#print(com_kw_plots)
ggsave(filename = "plots/Figure 4.jpg", width = 12, height = 8, dpi = 600)


l##
##########       ANOSIM analysis    #######
# Change between richness and abundance
# Function to run ANOSIM and return results -courtesy for ChatGPT
anisom_data <- ays_data %>% 
  # filter(method == "microscope") %>%
  filter(method == "flowcam") %>%
  select(site, station, tow_size, method, taxon, rela_abun_densi) %>% 
  pivot_wider(names_from = taxon, 
              values_from = rela_abun_densi, 
              values_fn = list(rela_abun_densi = sum), 
              values_fill = list(rela_abun_densi = 0)) %>% 
  arrange(site, station, tow_size, method)

str(anisom_data)


matrix_anisom_data <- as.matrix(anisom_data[5:ncol(anisom_data)])

ano <- anosim(matrix_anisom_data,
              anisom_data$tow_size, 
              dist = "bray",
              permutations = 9999)

plot(ano)


tow_sizes <- unique(anisom_data$tow_size)

# Initialize an empty list to store results
pairwise_results <- list()

# Loop through all pairs of tow sizes
for (i in 1:(length(tow_sizes) - 1)) {
  for (j in (i + 1):length(tow_sizes)) {
    # Subset data for the two tow sizes
    subset_data <- anisom_data[anisom_data$tow_size %in% c(tow_sizes[i], tow_sizes[j]), ]
    matrix_subset <- as.matrix(subset_data[, 5:ncol(subset_data)])
    
    # Perform ANOSIM for the two groups
    ano_result <- anosim(matrix_subset, subset_data$tow_size, distance = "bray", permutations = 9999)
    
    # Store the results in the list
    pairwise_results[[paste(tow_sizes[i], "vs", tow_sizes[j])]] <- c(
      R_stat = ano_result$statistic,
      p_value = ano_result$signif
    )
  }
}

# Convert the list of results into a data frame
pairwise_table <- do.call(rbind, pairwise_results)
pairwise_table <- data.frame(Comparison = rownames(pairwise_table), pairwise_table, row.names = NULL)

# Print the table
print(pairwise_table)


###
###
##### Figure 5 - Size classes ------------------------------------------------------------
jpeg("plots/Figure 5.jpg", width = 800, height = 600)
mosaic(~ size_class + tow_size, data = ays_data %>% filter(method == "microscope"),
       shade = TRUE, legend = TRUE, main = NULL,
       set_labels = list(tow_size = c("Water grab", "20 µm net", "55 µm net")),
       set_varnames = list (tow_size = "", size_class = "")
       )
dev.off()


# ggsave(ms_plot, filename = "plots/Figure 5.jpg", width = 12, height = 8, dpi = 600)


ays_data <- ays_data %>%
  mutate(size_class = factor(size_class, levels = unique(size_class)))

ggplot(ays_data %>%  filter(method == "microscope"),
       aes(x = tow_size, fill = size_class)) +
  geom_bar(position = "fill") +  
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c("0" = "water grab",
                              "20" = "20 µm net",
                              "55" = "55 µm net")) +
  facet_wrap(~ method) +
  labs(
    title = "Size Class Distribution by Tow Size",
    x = "",
    y = "",
    fill = ""
  ) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey70"),
        panel.grid.minor = element_line(color = "grey80"),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5, color = "black"),
        legend.key = element_rect(fill = "transparent"),
        legend.title = element_text(size = 12),
        strip.background = element_blank(),
        legend.position = "bottom",
        strip.text = element_blank(),
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.8, "cm"), 
        legend.key.height = unit(0.1,"cm"),
        legend.byrow = TRUE,
        axis.text.y = element_text(size = 12, color = "black"),
        strip.placement = "outside",
        text = element_text(family = "Arial"))



size_class_summary <- ays_data %>% filter(method == "microscope") %>% 
  group_by(method, tow_size, size_class) %>%
  summarise(count = n(), .groups = "drop")

ggplot(size_class_summary, aes(x = tow_size, y = count, fill = size_class)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(labels = c("0" = "water grab",
                              "20" = "20 µm net",
                              "55" = "55 µm net")) +
  facet_wrap(~ method) +
  labs(
    title = "Size Class Distribution by Tow Size",
    x = "",
    y = "",
    fill = ""
  ) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey70"),
        panel.grid.minor = element_line(color = "grey80"),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5, color = "black"),
        legend.key = element_rect(fill = "transparent"),
        legend.title = element_text(size = 12),
        strip.background = element_blank(),
        legend.position = "bottom",
        strip.text = element_blank(),
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.8, "cm"), 
        legend.key.height = unit(0.1,"cm"),
        legend.byrow = TRUE,
        axis.text.y = element_text(size = 12, color = "black"),
        strip.placement = "outside",
        text = element_text(family = "Arial"))


###
# Correlations & Regression ------------------------------------------------------------
#For collection methods 
# Table 3 and 4 

data_correlations <- ays_data %>%  
  mutate(tow_size = as.factor(tow_size),  
         method = as.factor(method),
         across(!c(site, station,tow_size,method,genus, taxon), ~replace_na(., 0))) %>% 
  pivot_wider(names_from = tow_size, 
              values_from = c(rela_abun_densi, abs_abun,
                              S.meth, expH.meth, ENS.meth),
              values_fn = c(list(rela_abun_densi  = sum),list(abs_abun  = sum),
                            list(S.meth  = sum),list(expH.meth  = sum), list(ENS.meth = sum)),
              values_fill = c(list(rela_abun_densi  = 0),list(abs_abun  = 0),
                              list(S.meth  = 0),list(expH.meth  = 0),list(ENS.meth = 0))
  ) 


#Change what you correlating here 
docorr_data_correlations <- data_correlations %>%
  filter(method == "microscope") %>% 
  group_by(taxon) %>% 
  summarise(
    spearman_cor = tryCatch(cor.test(abs_abun_0, expH.meth_55, # change to correlate different variables
                                     method = "spearman", alternative = "two.sided", exact = FALSE)$estimate, error = function(e) NA),
    p_value = tryCatch(cor.test(abs_abun_0, expH.meth_55,
                                method = "spearman", alternative = "two.sided", exact = FALSE)$p.value, error = function(e) NA)
  ) %>%
  ungroup() %>% 
  mutate(significance = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01  ~ "**",
    p_value < 0.05  ~ "*",
    TRUE ~ ""
  ))

docorr_data_correlations %>%  filter(significance != "")


#plot#plotgenus_functionaltype_lookup
ggplot(docorr_data_correlations %>%  filter(significance != ""), 
       # process_phyconc(species_genus_lookup,genus_functionaltype_lookup),
       aes(x = taxon, y = spearman_cor, fill = taxon)) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = colorsforgraphs)+
  # facet_grid(~site, scales = "free",
  #            labeller = labeller(tow_size = c("0" = "Water grab",
  #                                             "20" = "20µm net",
  #                                             "55" = "55µm net"),
  #                                site = c("AB" = "Algoa Bay",
  #                                         "SHB" = "St.Helena Bay",
  #                                         "WB" = "Walker Bay"))) +
  coord_flip() +
  labs(x = "", y = "Spearman Correlation", fill = "",
       subtitle = "") +
  theme(
    # axis.title.x = element_blank(),
    # axis.text.x = element_blank(),
    # strip.text = element_blank(),
    # axis.text.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 6.5, vjust = 0),
    strip.text = element_text(size = 10),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey70"),
    panel.grid.minor = element_line(color = "grey70"),
    legend.position = "none",
    axis.title = element_text(size = 10),
    legend.key = element_rect(fill = "transparent"),
    panel.grid.major.x = element_blank(),
    legend.title = element_text(size = 10),
    strip.background = element_blank(),
    legend.text = element_text(size = 10),
    strip.placement = "outside") + 
  geom_text(aes(label = significance), hjust = 4, size = 4)




##
# Citations ---------------------------------------------------------------

cite_packages(out.dir = getwd())

