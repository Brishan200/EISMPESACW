## Code for 
#Phytoplankton enumeration in coastal waters and the role of collection methods 

# Libraries, functions and constants --------------------------------------
library(tidyverse)
library(vegan)
library(ggsignif)
library(ggpp)
library(vcd)
library(patchwork)
library(grateful)
library(gt)
library(ggvenn)
library(ggalluvial)


##
# Constants and functions -------------------------------------------------


colorsforgraphs <- c(
  "#4B474D", "#49851C", "#FA0022", "#FB0DFF", "#00FF32", "#1682FD", "#F89E00", "#D84788", "#E0EC00", "#00F9C8", "#00CCF3", 
  "#C487E2", "#803D00", "#A316FE", "#E3C582", "#004F8E", "#FE6A56", "#FCACB7", "#862A6D", "#229180", "#FC16C6", 
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
    "Dictyocha",
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
    "Dictyocha",
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
    "Silicoflagellate",
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
# Importing in microscope  counts ------------------------------
setwd("C:/Resources/Campus Work/PhD/DATA/BioSCape")

##Microscope data ##
phyto_raw <-  read_csv("Phyto-counts/Phyto_abund_BioSCape_microcsope.csv")

#filter for 3 stations in each bay and for the flight day of each bay 
m_data <- phyto_raw %>% 
  select(-1) %>% 
  filter( date_sampled %in% c("2023-10-25","2023-10-31","2023-11-08"),
    (site == "SHB" & station %in% c("St2", "St3", "St4")) |
      (site == "WB" & station %in% c("St2", "St3", "St4")) |
      (site == "AB" & station %in% c("St1", "St2", "St3"))) %>% 
  mutate(station = case_when(
    site %in% c("SHB", "WB") & station == "St2" ~ "1",
    site %in% c("SHB", "WB") & station == "St3" ~ "2",
    site %in% c("SHB", "WB") & station == "St4" ~ "3",
    site == "AB" & station == "St1" ~ "1",
    site == "AB" & station == "St2" ~ "2",
    site == "AB" & station == "St3" ~ "3"
  ))

unique(m_data$tow_size)
unique(m_data$tow_type)
unique(m_data$site)
unique(m_data$station)
unique(m_data$date_sampled)


sites <- data.frame(
  site = c("AB", "AB", "AB", "SHB", "SHB", "SHB", "WB", "WB", "WB"),
  station = c("1", "2", "3", "1", "2", "3", "1", "2", "3"),
  lat = c(-33.895833, -33.825833, -33.767778, -32.71255, -32.68301, -32.65564, -34.537778, -34.515556, -34.493611),
  lon = c(25.704167, 25.754167, 25.905833, 18.10277, 18.08783, 18.07399, 19.318611, 19.309167, 19.3)
)

m_data <- m_data %>% left_join(sites)

m_data <-  m_data %>% 
  mutate(taxon = case_when(
    taxon == "Noctiluca" ~ "N.scintillans",
    taxon == "Pseudo-nitzschia" ~ "P.nitzschia",
    taxon == "G.polyhedra" ~ "G.polyedra",
    taxon == "Ciliates" ~ "Ciliate",
    taxon == "Tintinnids" ~ "Tintinnid",
    taxon == "P.depessum" ~ "P.depressum",
    TRUE ~ taxon
  ))

m_data <- add_taxonomic_info(m_data,lookup_table)

str(m_data)

##
# Calculation from count to abs_abundance ---------------------------------
## microscope ##
m_data %>%
  group_by(site,tow_type,tow_size) %>%
  reframe(range = median(abs_abun)) %>% 
  pivot_wider(names_from = c(tow_type, tow_size), values_from = range)



m_data %>%
  group_by(tow_size, tow_type) %>%
  reframe(range = median(abs_abun))

#All volumes in the datasheet are in Liters
# sample_vol -
#for H  it is the volume concentrated to after net tow (Vulcan vol)
#for U it is the settled volume

# vol_waterfiltered - 
#for H it is the calculated volume filtered during net tow (2 min 2 knot area of net opening)
#for U it is the volume of the water settled 


######   Combining flowcam and microscope data ------------
#countdata - combination of flowcam and microscope counts
#concdata - combination of flowcam and microscope densities (absolute abundance) 
#raw count data 
countdata <-  m_data %>%  
  group_by(site, station,date_sampled, tow_type, tow_size,method, taxon) %>% 
  reframe(total_cells = sum(total_cells))

countdata %>% 
  group_by(tow_type, tow_size) %>% 
  reframe(aves = range(total_cells))

#abs abundance
concdata <- m_data %>% 
  group_by(site, station,date_sampled, tow_size,tow_type,method, taxon) %>% 
  reframe(abs_abun = sum(abs_abun)) 

concdata %>%
  group_by(site,tow_type,tow_size) %>% 
  reframe(range = median(abs_abun))


###
# Diversity indices calculation  ----------------------------------------
diversity_indices <- countdata %>%
  mutate(tow_size = as.factor(tow_size),
         tow_type = as.factor(tow_type)) %>% 
  group_by(site, station, tow_size, tow_type) %>%
  summarise(Shannon = diversity(total_cells, index = "shannon"),
            richness = specnumber(total_cells),
            Pielou= Shannon/log(richness)) %>% 
  ungroup()


renyidata <- countdata %>% 
  pivot_wider(id_cols = c(site,station,tow_size,tow_type),
              values_fn = sum,
              names_from = taxon,
              values_fill = 0,
              values_from = total_cells)

renyidata$S <- specnumber(renyidata[,5:ncol(renyidata)])
renyidata$ENS <- renyidata(renyidata[,5:ncol(renyidata)], "invsimpson")
summary(renyidata[,c(1:4,109:110)])
# str(renyidata)
# renyidata[1:4]


renyi_methods <- renyi(renyidata[,5:ncol(renyidata)],
                       scales = c(0, 0.5, 1, 2, 4, 16, 32, Inf),
                       hill = TRUE)

renyi_methods <- renyi_methods %>% 
  rename(S.meth="0", expH.meth="1", ENS.meth="2", BP.Ind = "Inf" )

div_renyi <- cbind(renyidata[,c("site","station","tow_size","tow_type")], renyi_methods)

div_renyi <- div_renyi %>% mutate(tow_size = as.factor(tow_size), 
                                  tow_type =  as.factor(tow_type))

 # plot(div_renyi)
diversity_indices <- diversity_indices %>% right_join(div_renyi,
                                                      by = c("site","station","tow_size", "tow_type"))


##
# Calculating relative abundance -----------------------------------------
conc_sum <- concdata %>% 
  group_by(site, station,tow_size, tow_type,taxon) %>%
  reframe(total_conc_taxon = sum(abs_abun))

# Step 2: Calculate total number of species population (Σ Nsi) per group
total_conc_sum <- conc_sum %>% 
  group_by(site, station,tow_size,tow_type) %>%
  reframe(total_conc = sum(total_conc_taxon, na.rm = TRUE))

# Step 3: Calculate relative abundance (%)
rela_abun <- conc_sum %>%
  left_join(total_conc_sum, by = c("site","station","tow_size", "tow_type")) %>%
  mutate(rela_abun_densi = (total_conc_taxon / total_conc)*100)

###
# Create data frame to use for analysis ------------------------------------

ays_data <- concdata  %>% 
  select(site, station,tow_size,tow_type,taxon,abs_abun) %>% 
  left_join(countdata %>% select(site, station, tow_size,taxon,total_cells)) %>% 
  left_join(rela_abun %>% select(site, station, tow_size,taxon,rela_abun_densi)) %>% 
  left_join(diversity_indices %>% mutate(tow_size = as.integer(as.character(tow_size)))) %>% 
  as_tibble()

ays_data <- add_taxonomic_info(ays_data,lookup_table)

ays_data <- ays_data %>% filter(!functional_type %in% c("Zooplankton", "Cyst"))

ays_data <- ays_data %>% mutate(tow_size = as.factor(tow_size),
                                functional_type2 = ifelse(functional_type %in% c("Diatom", "Dinoflagellate"),
                                                          functional_type,
                                                          "Other"))

rare_sp <- ays_data %>%
  filter(abs_abun < quantile(ays_data$abs_abun, 0.05)) %>%
  distinct(site,tow_type, tow_size, taxon) 

ays_data %>% 
  distinct(taxon) %>% 
  reframe(count = n())

ays_data %>% 
  distinct(taxon) %>% 
  print(n = Inf)

write.csv(ays_data %>% 
            distinct(taxon),
          "taxon.csv")

# Step 1. De-duplicate
base <- ays_data %>%
  distinct(site, tow_type, tow_size, taxon)

# Step 2. Collapse taxa per site × tow config
collapsed <- base %>%
  unite("coll_config", tow_type, tow_size, sep = "_size") %>% 
  group_by(site, coll_config) %>%
  summarise(
    taxa = paste(sort(unique(taxon)), collapse = ", "),
    .groups = "drop"
  )

# Step 3. Pivot wider with site + tow_size as headers
wide_tab <- collapsed %>%
  pivot_wider(
    names_from = coll_config,
    values_from = taxa
  ) %>%
  arrange(site)

# Step 4. Pretty print with gt, using site as spanner and tow_size as sub-header
gt_table <- wide_tab %>%
  gt(rowname_col = "site") %>%
  # tab_spanner_delim(delim = "_") %>%
  tab_header(
    title = "Taxa"
  )

gt_table


ays_data %>% 
  group_by(functional_type) %>% 
  reframe(tot = sum(total_cells))

ays_data %>% 
  distinct(site, tow_size, tow_type, taxon) %>%
  group_by(site, tow_size, tow_type) %>%
  summarise(n_taxa = n_distinct(taxon), .groups = 'drop') %>%
  pivot_wider(names_from = c(tow_size,tow_type),
              values_from = n_taxa)

ays_data %>% 
  distinct(site, tow_size, tow_type,genus, taxon) %>%
  group_by(site, tow_size, tow_type) %>%
  summarise(n_taxa = n_distinct(genus), .groups = 'drop') %>%
  pivot_wider(names_from = c(tow_size,tow_type),
              values_from = n_taxa)

#unique taxa per site, tow size, tow type
ays_data %>% 
  distinct(site, tow_size, tow_type, taxon) %>%
  group_by(taxon) %>%
  filter(n_distinct(tow_type) == 1 & n_distinct(tow_size) == 1) %>%
  ungroup() %>% 
  arrange(taxon) %>% 
  group_by(site, tow_size,tow_type) %>%
  summarise(n_taxa = n_distinct(taxon), .groups = 'drop') %>%
  pivot_wider(names_from = c(tow_size,tow_type),
              values_from = n_taxa)

ays_data %>% 
  distinct(site, tow_size, tow_type, taxon) %>%
  group_by(taxon) %>%
  filter(n_distinct(tow_type) == 1 & n_distinct(tow_size) == 1) %>%
  ungroup() %>% 
  arrange(taxon) %>% 
  group_by(site, tow_size, tow_type) %>%
  summarise(taxa = paste(unique(taxon), collapse = ", "), .groups = "drop") %>%
  pivot_wider(names_from = site, values_from = taxa)


ays_data %>% 
  group_by(site, tow_type , tow_size) %>% 
  reframe(mean_aa = median(abs_abun),
          sd_aa = sd(abs_abun)) %>% 
  select(!sd_aa) %>% 
  pivot_wider(names_from = c(tow_type, tow_size), values_from = mean_aa)


# Prepare data: one row per taxon × site × tow_type × tow_size
sankey_data_taxon <- ays_data %>%
  distinct(site, station, tow_size,tow_type,taxon, total_cells,functional_type) %>%
  unite("coll_config", tow_type,tow_size, sep = "_size")

# Plot Sankey (alluvial)
ggplot(sankey_data_taxon,
       aes(axis1 = coll_config, axis2 = site, 
           axis3 = functional_type,
           y = log1p(total_cells))) +
  geom_alluvium(aes(fill = coll_config), alpha = 0.7, width = 0.35) +
  geom_stratum(width = 0.25, fill = "grey80", color = "black") +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            data = function(d) {
              d %>%
                dplyr::mutate(functional_type = ifelse(functional_type %in% c("Flagellate", "Silicoflagellate"), "", functional_type))
            }) +
  scale_x_discrete(limits = c("Collection method", "Site", "Fucntional group"),
                   expand = c(.05, .05)) +
  scale_fill_manual(values = c("#0072B2","#E69F00","#D55E00","#009E73","purple")) +
  labs(title = "",
       fill = "",
       y = "") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.text = element_text(size = 12, color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        text = element_text(size = 12, color = "black"))

ggsave(filename = "plots/Figure 1a.jpg", width = 12, height = 8, dpi = 300)


ggplot(sankey_data_taxon %>%  
         filter(functional_type %in% c("Flagellate", "Silicoflagellate")),
       aes(axis1 = coll_config, axis2 = site, 
           axis3 = functional_type,
           y = log1p(total_cells))) +
  geom_alluvium(aes(fill = coll_config), alpha = 0.7, width = 0.35) +
  geom_stratum(width = 0.25, fill = "grey80", color = "black") +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            check_overlap = TRUE) +
  scale_x_discrete(limits = c("Collection method", "Site", "Fucntional group"),
                   expand = c(.05, .05)) +
  scale_fill_manual(values = c("#0072B2","#E69F00","#D55E00","#009E73","purple")) +
  labs(title = "",
       fill = "",
       y = "") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.text = element_text(size = 12, color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        text = element_text(size = 12, color = "black")) 

ggsave(filename = "plots/Figure 1b.jpg", width = 12, height = 8, dpi = 300)

##
# Absolute abundance - Figure 2a---------------------------------------
ggplot(ays_data %>% 
         mutate(ts_meth = paste(tow_size, tow_type, sep = "_"),
                ts_meth = factor(ts_meth, levels = c("0_U", "20_H", "20_V", "55_H", "55_V"),
                                 labels = c("Water grab",
                                            "20µm net horizontal",
                                            "20µm net vertical",
                                            "55µm net horizontal",
                                            "55µm net vertical"))),
       aes(x = ts_meth, y = log10(abs_abun), fill = ts_meth)) +
  geom_boxplot() +
  geom_jitter(aes(color = functional_type)) +
  scale_color_manual(values = c("#0072B2","#E69F00","#D55E00","#009E73")) +
  scale_x_discrete(labels = wrap_labels(
    c("Water grab",
      "20µm net horizontal",
      "20µm net vertical",
      "55µm net horizontal",
      "55µm net vertical"), width =12, new.line="\n"))+
  scale_fill_manual(values = c("#E69F00", "#bcbd22",
                               "#1f77b4", "#2ca02c",
                               "#17becf", "#91cf60"),
                    name = "",
                    labels = c("Water grab",
                               "20µm net horizontal",
                               "20µm net vertical",
                               "55µm net horizontal",
                               "55µm net vertical")) +
  facet_grid(~site,scales = "free",
             labeller = labeller(site = c("AB" = "Algoa Bay",
                                          "SHB" = "St. Helena Bay",
                                          "WB" = "Walker Bay"))) +
  labs(subtitle = "",
       x = "",
       y = expression(log[10]("Abs. abundance"))) +
  theme(
    axis.text.x = element_text(size = 12, angle = 90,vjust=0.5,hjust=0.5, color = "black"),
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
  guides(fill = "none") +
  geom_signif(test =  "wilcox.test",
              comparisons = list(
                c( "Water grab","20µm net horizontal"),
                c( "Water grab","20µm net vertical"),
                c( "Water grab","55µm net horizontal"),
                c( "Water grab","55µm net vertical"),
                c( "20µm net vertical","20µm net horizontal"),
                c( "55µm net vertical","55µm net horizontal"),
                c( "20µm net horizontal","55µm net horizontal"),
                c( "20µm net vertical","55µm net vertical")),
              map_signif_level = TRUE, step_increase = 0.1,
              y_position = 6, vjust = 0, textsize = 4) +
  ggtitle("absolute abundance") -> abs_abun_kwplot1

##
# Relative abundance plot - Figure 2b ------------------------------
ays_data <- ays_data %>%
  mutate(rela_abun_densi = replace_na(rela_abun_densi, 0))

range(ays_data$rela_abun_densi)

hist(log10(ays_data$rela_abun_densi))

ggplot(ays_data %>% 
         mutate(ts_meth = paste(tow_size, tow_type, sep = "_"),
                ts_meth = factor(ts_meth, levels = c("0_U", "20_H", "20_V", "55_H", "55_V"),
                                 labels = c("Water grab",
                                            "20µm net horizontal",
                                            "20µm net vertical",
                                            "55µm net horizontal",
                                            "55µm net vertical"))),
       aes(x = ts_meth, y = log10(rela_abun_densi), fill = ts_meth)) +
  geom_boxplot() +
  geom_jitter(aes(color = functional_type)) +
  scale_color_manual(values = c("#0072B2","#E69F00","#D55E00","#009E73")) +
  scale_x_discrete(labels = wrap_labels(
    c("Water grab",
      "20µm net horizontal",
      "20µm net vertical",
      "55µm net horizontal",
      "55µm net vertical"), width =12, new.line="\n"))+
  scale_fill_manual(values = c("#E69F00","#bcbd22",
                               "#1f77b4", "#2ca02c",
                               "#17becf", "#91cf60"),
                    name = "",
                    labels = c("Water grab",
                               "20µm net horizontal",
                               "20µm net vertical",
                               "55µm net horizontal",
                               "55µm net vertical")) +
  facet_grid(~site,scales = "free",
             labeller = labeller(site = c("AB" = "Algoa Bay",
                                          "SHB" = "St. Helena Bay",
                                          "WB" = "Walker Bay"))) +
  guides(fill = "none") +
  labs(subtitle = "",
       x = "",
       y = expression(log[10]("Relative abundance"))) +
  theme(
    axis.text.x = element_text(size = 12, angle = 90,vjust=0.5,hjust=0.5, color = "black"),
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
                c( "Water grab","20µm net horizontal"),
                c( "Water grab","20µm net vertical"),
                c( "Water grab","55µm net horizontal"),
                c( "Water grab","55µm net vertical"),
                c( "20µm net vertical","20µm net horizontal"),
                c( "55µm net vertical","55µm net horizontal"),
                c( "20µm net horizontal","55µm net horizontal"),
                c( "20µm net vertical","55µm net vertical")),
              map_signif_level = TRUE, step_increase = 0.1,
              y_position = 3, vjust = 0, textsize = 4) +
  ggtitle("relative abundance") -> rela_abun_kwplot1


##
#####  Figure 3 -------
#Plot of kw for absolute and relative abundance 

kw_abun_plots <- (abs_abun_kwplot1 /  rela_abun_kwplot1) +  
  plot_layout(guides = "collect", axes ="collect",
              ncol= 2, nrow = 1) +
  plot_annotation(tag_levels = "a")


print(kw_abun_plots)
ggsave(filename = "plots/Figure 2.jpg", width = 12, height = 8, dpi = 300)

###
# Renyi plots --------------------------------------------------
renyidata <- ays_data %>% 
  filter(functional_type %in% c("Diatom", "Dinoflagellate", "Flagellate", "Silicoflagellate")) %>%
  select(site,station,tow_size, tow_type,taxon,total_cells) %>% 
  mutate(coll_config = paste(tow_type, tow_size, sep = "_size")) %>% 
  arrange(taxon) %>% 
  pivot_wider(id_cols = c(coll_config, site),
              values_fn = sum,
              names_from = taxon,
              values_fill = 0,
              values_from = total_cells)

renyidata$S <- specnumber(renyidata[,3:ncol(renyidata)])
renyidata$ENS <- diversity(renyidata[,3:ncol(renyidata)], "invsimpson")
summary(renyidata[,c(1:2,102:103)])

renyi_methods <- renyi(renyidata[,3:ncol(renyidata)],
                       scales = c(0, 0.5, 1, 2, 4, 16, 32, Inf),
                       hill = TRUE)

renyi_methods <- renyi_methods %>% rename(S.meth="0", expH.meth="1", ENS.meth="2", BP.Ind = "Inf" )

div_renyi <- cbind(renyidata[,c("coll_config","site")], renyi_methods)

div_renyi_long <- div_renyi %>%
  pivot_longer(cols = c(`0.5`, `4`, `16`, `32`, BP.Ind),
               names_to = "alpha",
               values_to = "renyi_index") %>%
  mutate(alpha = factor(alpha, levels = c("0.5","4","16","32","BP.Ind")))

ggplot(div_renyi_long,
      aes(x = as.numeric(as.character(alpha)), 
          y = renyi_index,
          group = coll_config, 
          color = coll_config)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~site, nrow = 1, labeller = labeller(site = c(
    "AB" = "Algoa Bay",
    "SHB" = "St Helena Bay",
    "WB" = "Walker Bay"
  ))) +
  scale_x_continuous(breaks = c(0.5, 4, 16, 32), labels = c("0.5","4","16","32")) +
  scale_color_manual(values = c("#0072B2","#E69F00","#009E73", "#D55E00", "#CC79A7")) +
  labs(title = "",
       x = expression(alpha),
       y = "Renyi diversity index",
       color = "Collection method") +
  theme(
    axis.text.x = element_text(size = 12, angle = 0,vjust=0.5,hjust=0.5, color = "black"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey80"),
    legend.position = "bottom",
    axis.title = element_text(size = 12, color = "black"),
    legend.key = element_rect(fill = "transparent"),
    panel.grid.minor.x = element_line(color = "grey80"),
    legend.title = element_text(size = 12),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.text.y = element_text(size = 12, color = "black"),
    strip.placement = "outside")


ggsave(filename = "plots/Figure 3.jpg", width = 12, height = 8, dpi = 300)


###
# NMDS --------------------------------------------------------------------

# Step 1: Build community matrix (samples = site × coll_config, columns = taxa)
comm_matrix <- ays_data %>%
  group_by(site,station, tow_size, tow_type, taxon) %>%
  summarise(abundance = sum(abs_abun), .groups = "drop") %>%
  mutate(coll_config = paste(tow_type, tow_size, sep = "_")) %>%
  select(coll_config, site,station, tow_size, tow_type, taxon, abundance) %>%
  pivot_wider(names_from = taxon, values_from = abundance, values_fill = 0)

# Save metadata
metadata <- comm_matrix %>%
  select(coll_config, site,station, tow_size, tow_type)

# Drop non-taxa cols for the matrix
comm_only <- comm_matrix %>%
  select(-coll_config, -site,-station, -tow_size, -tow_type)

# Bray-Curtis dissimilarity
dist_mat <- vegdist(comm_only, method = "bray")

# PERMANOVA
adonis_result <- adonis2(dist_mat ~ coll_config * site,
                         data = metadata, permutations = 999)


print(adonis_result)

dispersion_methods <- betadisper(dist_mat, metadata$coll_config)
anova(dispersion_methods)

dispersion <- betadisper(dist_mat, metadata$site)
anova(dispersion)

# Extract distances from betadisper object
disp_df <- data.frame(
  coll_config = metadata$site,
  distance = dispersion$distances
)

ggplot(disp_df, aes(x = coll_config, y = distance, fill = coll_config)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(x = "Collection method", y = "Distance to centroid",
       title = "Multivariate dispersion by collection method") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")


# Run NMDS (2D)
nmds <- metaMDS(comm_only, distance = "bray", k = 2, trymax = 100)

nmds_scores <- as.data.frame(scores(nmds, display = "sites")) %>%
  bind_cols(metadata)

stress_val <- round(nmds$stress, 3)

ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2,
                        color = site , shape = coll_config)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(aes(group = site), color = "black", linetype = "dashed") +
  scale_color_manual(values = c("#0072B2","#E69F00","#009E73", "#D55E00", "#CC79A7")) +
  labs(title = "",
       color = "", shape = "Collection method") +
  theme(
    axis.text.x = element_text(size = 12, angle = 0,vjust=0.5,hjust=0.5, color = "black"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey80"),
    legend.position = "bottom",
    axis.title = element_text(size = 12, color = "black"),
    legend.key = element_rect(fill = "transparent"),
    panel.grid.minor.x = element_line(color = "grey80"),
    legend.title = element_text(size = 12),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.text.y = element_text(size = 12, color = "black"),
    strip.placement = "outside") +
  annotate("text", 
           x = 1.6, 
           y = -2, 
           label = paste("Stress =", stress_val),
           size = 4)

ggsave(filename = "plots/Figure 4.jpg", width = 12, height = 8, dpi = 300)


####
