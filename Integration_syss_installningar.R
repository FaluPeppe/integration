# Skript som anropar func_INtegration_syss.R för att köra ut 
# ================================ Här gör vi inställningar  ==============================================

source("G:/Samhällsanalys/Automatisering och R/Skript/integration/func_Integration_syss.R", encoding = "utf-8", echo = FALSE)

options(dplyr.summarise.inform = FALSE)

# --------------------------- inställning för hela skriptet - ändras sällan -------------------------------------

skriv_till_Excelfil <- FALSE

output_mapp_xls <- "G:\\Samhällsanalys\\API\\Fran_R\\integration\\"
output_mapp <- output_mapp_xls

# sökväg till logga för att kunna lägga in den i diagrammen
logga_path <- "G:/Samhällsanalys/MallarLoggor/logo_liggande_fri_svart.png"

# --------------------------- inställningar som görs per körning ------------------------------------------

# Välj region med kommun- eller länskod. Välj "riket" TRUE om vi vill hämta ner statistik om riket, FALSE om vi vill hämta statistik om Dalarna
vald_region <- c("2034", "2039", "2062", "2084")          # man kan välja flera  2034, 2039, 2062

huvudnamn <- c("integration_syss_lagutb", 
               "integration_syss_nyanl",
               "integration_syss_utrFodd",
               "integration_syss_utomeurpFodd")


# övriga förinställda diagram som finns (och som läggs som huvudnamn):
# integration_syss_utrFodd
# integration_syss_utomeurpFodd
# integration_syss_lagutb
# integration_syss_nyanl

bara_en_region <- TRUE         # om TRUE så visas bara den eller de regioner som valts i raden ovan, annars jämförs
# med regionens övriga kommuner, gäller bara om kommuner väljs

for (rapportlista in 1:length(huvudnamn)) {
  for (reglista in 1:length(vald_region)){
    SkapaIntegrationsDiagram(huvudnamn[rapportlista], 
                             vald_region[reglista],
                             skriv_till_Excelfil, 
                             output_mapp_xls,
                             output_mapp, 
                             logga_path, 
                             bara_en_region)
  }
}






