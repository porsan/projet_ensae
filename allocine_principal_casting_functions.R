format_classique<- function (poste_total, nom, longueur_voulue) {

    poste_format<- character(longueur_voulue)
    
    if (length(poste_total)!=0) {
        
        for (i in 1:min(length(poste_format),length(poste_total))) {
            poste_format[i]<-gsub("[()]","",poste_total[i])
        }
        
    }
    names(poste_format)<- paste0(nom,seq(1:length(poste_format)))
    return(poste_format)
}



format_id<- function(poste_total, poste_format, nom) {
    
    longueur_voulue<- length(poste_format)
    index_url<- integer(longueur_voulue)
    liste_url<- integer(longueur_voulue)
    id_url<- integer(longueur_voulue)
    
    if (length(poste_total)!=0) {
        
        for (i in 1:min(length(poste_total),length(poste_format))) {
            
            if (length(grep(poste_format[i],names(links)))!= 0) {
                index_url[i]<- grep(poste_format[i],names(links))
                liste_url[i]<- links[index_url[i]]
                id_url[i]<- unlist(str_extract_all(liste_url[i], "[0-9]+"))
            } else {next}
            
        }
        
        indices<-seq_along(poste_format)
        condition_na<-indices<=length(poste_total)
        
        index_url[condition_na & as.character(index_url)=="0"] <- NA
        liste_url[condition_na & liste_url=="0"] <- NA
        id_url[condition_na & id_url=="0"] <- NA
        
        index_url[(!condition_na) & as.character(index_url)=="0"] <- ""
        liste_url[(!condition_na) & liste_url=="0"] <- ""
        id_url[(!condition_na) & id_url=="0"] <- ""
        
    } else {
        liste_url<- rep("",longueur_voulue)
        id_url<- rep("",longueur_voulue)
    }
    names(id_url)= paste0(nom, seq(1:length(id_url)))
    return(id_url)

}



format_voix<- function(type_voix) {
    
    voixx<-voix[grepl(type_voix,voix)]
    voixx<- unlist(str_split(voixx,"\\n"))
    voixx<- str_trim(voixx)
    voixx<- voixx[voixx!=""]
    voixx<- voixx[-1]
    voixx<- gsub("Rôle : ","",voixx)
    
    if (length(voixx)!=0) {
        
        length_voixx<- max(2,length(voixx))
        voixx1<-voixx[1:min(length_voixx,16)][seq(1,length(voixx[1:min(length_voixx,16)])-1,2)]
        voixx_roles1<-voixx[1:min(length_voixx,16)][seq(2,length(voixx[1:min(length_voixx,16)]),2)]
        
        if (length(voixx) > 16) {
            
            length_voixx2<- max(18,length(voixx))
            voixx2<-voixx[17:length_voixx2][seq(2,length(voixx[17:length_voixx2]),2)]
            voixx_roles2<-voixx[17:length_voixx2][seq(1,length(voixx[17:length_voixx2])-1,2)]
            voixx<-c(voixx1,voixx2)
            voixx_roles<-c(voixx_roles1,voixx_roles2)
            
        } else {
            
            voixx<-voixx1
            voixx_roles<-voixx_roles1
        }
        
        
    } else {
        voixx<-""
        voixx_roles<-""
    }
    return(list(voixx,voixx_roles))
}



format_postes<- function(nom, longueur_voulue, noms_bruts) {
    poste_total<-autres_fonctions[names(autres_fonctions) %in% noms_bruts]
    format_classique(poste_total, nom, longueur_voulue)
}


