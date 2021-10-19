library(pxweb)
library(writexl)
library(dplyr)
library(readr)            # för att använda parse_number
library(ggplot2)
library(grid)
library(png)
library(ggrepel)
library(RColorBrewer)
library(httr)             # för att komma förbi brandväggen

source("G:/Samhällsanalys/Automatisering och R/Skript/func_logga_i_diagram.R", encoding = "utf-8", echo = FALSE)
source("G:/Samhällsanalys/Automatisering och R/Skript/API_func.R", encoding = "utf-8", echo = FALSE)



SkapaIntegrationsDiagram <- function(huvudnamn = "integration_syss_utrFodd", 
                                     vald_region = "20",
                                     skriv_till_Excelfil = FALSE,
                                     output_mapp_xls = "G:\\Samhällsanalys\\API\\Fran_R\\integration\\",
                                     output_mapp = "G:\\Samhällsanalys\\API\\Fran_R\\integration\\",
                                     logga_path,
                                     diagram_capt = "",
                                     bara_en_region = TRUE,
                                     ladda_ned_api = TRUE
                                     ){
  # För att komma förbi proxyn
  set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090, username = Sys.getenv("userid"), password = Sys.getenv("pwd")))
  set_config(config(ssl_verifypeer = 0L))
  
  geo_riket <- FALSE
  if (tolower(vald_region) == "riket" | vald_region == "00") geo_riket <- TRUE  
  
  # sätt standardvärden
  summeringsvariabel = "Andel förvärvsarbetande, procent"
  bakgrundsvar_filter = "samtliga 20-64 år'"
  kon_filter = "kön == 'män och kvinnor'"
  utb_filter = "utbildningsnivå == 'samtliga utbildningsnivåer'"
    
  # Lägg in "procent" som titel på y-axeln om "procent" finns med i summeringsvariabeln
  if (grepl("procent", summeringsvariabel)) {
    y_titel <- "procent"
  } else {
    y_titel <- NA
  }
    
  
  if (huvudnamn == "integration_syss_lagutb"){
    utb_filter <- "utbildningsnivå == 'utbildningsnivå: förgymnasial utbildning'" 
    bakgrundsvar_filter <- "bakgrundsvariabel == 'födelseregion: Sverige' | bakgrundsvariabel == 'samtliga utrikes födda'"
    
  } else if (huvudnamn == "integration_syss_gymn_utb"){
    utb_filter <- "utbildningsnivå == 'utbildningsnivå: gymnasial utbildning'" 
    bakgrundsvar_filter <- "bakgrundsvariabel == 'födelseregion: Sverige' | bakgrundsvariabel == 'samtliga utrikes födda'"
      
  } else if (huvudnamn == "integration_syss_hogutb"){
    utb_filter <- "utbildningsnivå == 'utbildningsnivå: eftergymnasial utbildning'" 
    bakgrundsvar_filter <- "bakgrundsvariabel == 'födelseregion: Sverige' | bakgrundsvariabel == 'samtliga utrikes födda'"
    
  } else if (huvudnamn == "integration_syss_nyanl"){
    # Nyanlända, 0-9 år i 3 kategorier
    bakgrundsvar_filter <- "bakgrundsvariabel == 'vistelsetid 0-1 år' | bakgrundsvariabel == 'vistelsetid 2-3 år' | bakgrundsvariabel == 'vistelsetid 4-9 år'"   
    
  } else if (huvudnamn == "integration_syss_utrFodd") {
    # födda i Sverige respektive utrikes födda invandrare
    bakgrundsvar_filter <- "bakgrundsvariabel == 'födelseregion: Sverige' | bakgrundsvariabel == 'samtliga utrikes födda'"
    
    
  } else if (huvudnamn == "integration_syss_utomeurpFodd"){
    # födda i Sverige respektive utanför Europa
    bakgrundsvar_filter <- "bakgrundsvariabel == 'födelseregion: Sverige' | bakgrundsvariabel == 'födelseregion: övriga världen'"
  
  }
  
  
  
  
  # ====================================== Hämta regioner/kommuner ==============================================
  AktuellRegion <- NULL   # används för diagramtitel
  
  baralan <- any(nchar(vald_region) == 2)
  # om det finns län med i variabeln vald_region så körs bara län och inga kommuner hämtas
  if (baralan == FALSE) {
    # Fyll regiontabell
    regdf <- hamtaregtab()
    
    # Här väljer vi om vi ska ta med riket och länet också
    # Om vi bara vill ha län och inte kommuner, lägg in en vektor här nedan
    if (bara_en_region){
      location_aktRegion <- vald_region
    } else {
      location_aktRegion <- hamtakommuner(vald_region, tamedriket = FALSE, tamedlan = FALSE) 
    }
    
  
    # plocka ut vanligaste län i vektorn med alla regioner i 
    vanlan <- names(sort(table(substr(location_aktRegion,1,2)),decreasing=TRUE)[1])
    if (is.null(AktuellRegion)) AktuellRegion <- ifelse(length(location_aktRegion) 
                                                        > 1, paste0(regdf$region[regdf$regionkod
                                                            == vanlan], "s kommuner"), regdf$region[regdf$regionkod == vald_region])
  } else {
    regdf <- hamtaregtab()
    location_aktRegion <- vald_region
    AktuellRegion <- regdf$region[regdf$regionkod == vald_region]
    if (baralan == TRUE & bara_en_region == FALSE) AktuellRegion <- "Sveriges län"
  }
  # ================================================================================================================
  
  # skapa filnamn
  lan_filtrering <- location_aktRegion
  if (geo_riket == TRUE) lan_filtrering <- "00"
  if (baralan == TRUE & bara_en_region == FALSE) lan_filtrering <- '*'
  suffix <- ifelse(geo_riket == TRUE, "_riket", AktuellRegion)
  filnamn_diagram <- paste0(huvudnamn, suffix,".png")
  filnamn_xls <- paste0(huvudnamn, suffix,".xlsx")
  
  
  
  # ================ Hämta tabell från SCB =============================================
  url_adress <- ifelse(geo_riket == TRUE, "/OV0104/v1/doris/sv/ssd/AA/AA0003/AA0003B/IntGr1RikKonUtb",
                       "/OV0104/v1/doris/sv/ssd/AA/AA0003/AA0003B/IntGr1LanKonUtb")
  if (geo_riket == FALSE & baralan == FALSE) url_adress <- "/OV0104/v1/doris/sv/ssd/AA/AA0003/AA0003B/IntGr1KomKonUtb"
  
  url_adress <- paste0("http://api.scb.se", url_adress)
  
  # API-uttag 
  px_uttag <- pxweb_get(url = url_adress,
                        query = list(
                          Region = lan_filtrering,
                          Kon = c('*'),
                          UtbNiv = c('*'),
                          BakgrVar = c('*'),
                          ContentsCode = c('*'),
                          Tid = c('*')
                        )
  ) 
  
  # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
  # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
  px_df <- as.data.frame(px_uttag) %>% 
    cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
            select(Region)) %>% rename(regionkod = Region) %>% relocate(regionkod, .before = region)
  
  # Spara som Excel-fil
  if (skriv_till_Excelfil == TRUE) write_xlsx(px_df,paste0(output_mapp_xls, filnamn_xls))
  
  
  # ===================================== Gör diagram =====================================================
  
  # på något märkligt vis blir det dubbletter av vissa rader dessa rader tar bort dessa
  px_df_ej_dubblett <- px_df[!duplicated(px_df[,1:6]),]
  
  plot_df <- px_df_ej_dubblett %>%   
    filter(!! rlang::parse_expr(utb_filter)) %>%        # filtrera på utbildningsnivåer - se inställningar högst upp
    filter(!! rlang::parse_expr(kon_filter)) %>%        # filtrera på kön - se inställningar högst upp
    #filter(regionkod == lan_filtrering) %>%             # filtrera på län - se inställningar högst upp
    filter(!! rlang::parse_expr(bakgrundsvar_filter)) %>%    # filtrera på bakgrundsvariabel - se inställningar högst upp
    group_by(år, region, bakgrundsvariabel) %>% 
    summarise(antal = sum(get(summeringsvariabel)))
  
  
  
  # ======================================== specialanpassningar av titlar och kategorinamn ===========================
  
  # byt namn på kategorierna
  plot_df$bakgrundsvariabel[plot_df$bakgrundsvariabel == "födelseregion: Sverige"] <- "födda i Sverige"
  plot_df$bakgrundsvariabel[plot_df$bakgrundsvariabel == "födelseregion: övriga världen"] <- "födda utanför Europa"
  plot_df$bakgrundsvariabel[plot_df$bakgrundsvariabel == "samtliga utrikes födda"] <- "utrikes födda"
  plot_df$region <- ifelse(plot_df$region == "Riket", "Sverige", plot_df$region)
  
  sumvar_titel <- ifelse(summeringsvariabel == "Andel förvärvsarbetande, procent", 
                         "Andel förvärvsarbetande 20-64 år", summeringsvariabel)
  sumvar_titel <- ifelse(utb_filter == "utbildningsnivå == 'utbildningsnivå: förgymnasial utbildning'",
                         paste0(sumvar_titel, " lågutbildade (ej gymnasieutbildning)"),sumvar_titel)
  sumvar_titel <- ifelse(utb_filter == "utbildningsnivå == 'utbildningsnivå: gymnasial utbildning'",
                         paste0(sumvar_titel, " gymnasieutbildade"),sumvar_titel)
  sumvar_titel <- ifelse(utb_filter == "utbildningsnivå == 'utbildningsnivå: eftergymnasial utbildning'",
                         paste0(sumvar_titel, " högutbildade (eftergymnasial utbildning)"),sumvar_titel)
  
  # ===================================================================================================================
  
  
  # diagramfärger
  chart_col <- brewer.pal(name="Greens", n=9)[c(4,6,8,9)]
  diagramtitel <- paste0(sumvar_titel,"\ni ", AktuellRegion)
  
  
  p<-plot_df %>% 
    ggplot(aes(x=år, y=antal, fill = bakgrundsvariabel)) +
    geom_bar(position = "dodge", stat="identity")+
      theme(axis.text.x = element_text(size = 8, angle = 45),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          axis.ticks = element_blank(),
          legend.position = "bottom",
          legend.margin = margin(-10,0,0,0),
          legend.title = element_blank(),
          #legend.text = element_text(size = 12),
          plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(face = "italic", size = 5,
                                      hjust = 0, vjust = 0),
          plot.caption.position = "plot",
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(size=0.8, colour = "lightgrey"),
          panel.grid.minor.y = element_line(size=0.4, colour = "lightgrey") ,
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    labs(title = diagramtitel, 
         x = element_blank(),
         caption = diagram_capt) +
    {if (is.na(y_titel)){
      labs(y = element_blank())
    } else {
      labs(y = y_titel)
    }} +
    scale_fill_manual(values=chart_col) +
    scale_y_continuous(limits = c(0,100),
                       breaks = seq(0, 100, by = 10), 
                       minor_breaks = seq(0, 100, by = 2)) +
    facet_wrap(~ region, scales = "free") +
    {if(length(unique(plot_df$region)) == 1){
      theme(strip.text = element_blank())
    } else {  
      theme(strip.text = element_text(color = "black"),
            strip.background = element_blank(),
            axis.text.x = element_text(size = 6))
      }}
  
  #p 
  
  # Ändra höjd och bredd på den sparade png-filen, + ange mapp och filnamn
  bredd <- 12
  hojd <- 7
  
  # Ändra höjd och bredd på den sparade png-filen utifrån hur många regioner
  # som är med i diagrammet, + ange mapp och filnamn
  bredd <- ifelse(length(unique(plot_df$region)) == 1, 7, 13)
  hojd <- ifelse(length(unique(plot_df$region)) == 1, 4, 8)
  # och om det är fler än 20 kommuner
  bredd <- ifelse(length(unique(plot_df$region)) > 20 , 19, bredd)
  hojd <- ifelse(length(unique(plot_df$region)) > 20, 12, hojd)
  
  fullpath <- paste0(output_mapp, filnamn_diagram)
  ggsave(fullpath, width = bredd, height = hojd)
  
  # Lägg till logga till diagrammet =======================================
  
  if (!is.null(logga_path)){  
    add_logo(
      plot_path = paste0(output_mapp, filnamn_diagram), # url or local file for the plot
      logo_path = logga_path, # url or local file for the logo
      logo_position = "bottom right", # choose a corner
      # 'top left', 'top right', 'bottom left' or 'bottom right'
      logo_scale = 15,
      #10 as default, but can change to manually make logo bigger (lägre tal = större logga)
      replace = TRUE
    )
  }
# slut på funktionen
}