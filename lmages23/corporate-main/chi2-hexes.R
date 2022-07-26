library(tidyverse)
hexes <- function(include_hexes = NULL){
  chi_color <- "#5C191E"
  chi_color <- "red"
  
  hexdf <- data.frame(x=c(1,3,5,7,2,4,6,8,1,3,5,7,2,4,6,8),
                      y=c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4))
  
  hexdf <- hexdf %>% mutate(text = paste0(x,",", y))
  
  hexdf$text2 <- 1:nrow(hexdf)
  hexdf$row_number <- 1:nrow(hexdf)
  
  hexdf$text2[11]<- "KAI"
  hexdf$text2[10]<- "Mobile"
  hexdf$text2[5]<- "Zoom\nTeams\nMeet"
  hexdf$text2[13]<- "Hybrid/\nFace-to-face"
  hexdf$text2[7]<- "LA"
  hexdf$text2[4] <- "LMS"
  hexdf$text2[6] <- "Content\nRepo"
  hexdf$text2[15] <- "Adaptive"
  hexdf$text2[16] <- "Assessment\n Repo"
  hexdf$text2[3] <- "UDP"
  hexdf$show <- hexdf$row_number != hexdf$text2
  

  # if(!is.null(include_hexes)){
  #   
  #   hexdf$show <- 
  #     str_detect(hexdf$text2,include_hexes)
  # }
  
  hexdf <- hexdf %>% 
    mutate(fill = ifelse(text2 %in% c("KAI","Mobile","LA", "Adaptive"),"red","lightblue")) %>% 
    mutate(alpha = ifelse(text2 %in% c("KAI","Mobile","LA", "Adaptive"),.3,1)) %>% 
    filter(show)
  
   if(!is.null(include_hexes)){
     hexdf$fill[!str_starts(hexdf$text2, include_hexes)] <- "white"
  #   hexdf$color[!str_starts(hexdf$text2, include_hexes)] <- "white"
     hexdf$text2[!str_starts(hexdf$text2, include_hexes)] <- ""
   }
  
  hexdf %>% 
    ggplot( aes(x = x, y = y, label = text2)) + 
    geom_hex(stat = "identity", alpha=.3,fill=hexdf$fill) +
    geom_text(stat="identity", color="black")+
    scale_fill_manual(values = c(white="white",red=chi_color))+
    scale_x_continuous(limits = c(0,12)) +
    scale_y_continuous(limits = c(0,6))+
    theme_void()+
    theme(legend.position = "none")
}