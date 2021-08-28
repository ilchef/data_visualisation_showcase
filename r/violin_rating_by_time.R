



violin_rating_by_time <- function(df,var,color,title){
        
        label <- df %>%
                filter(!is.na(Decade))%>%
                filter(!is.na(!!as.name(var)))%>%
                mutate(Decade = fct_reorder(factor(Decade),Release.Date))%>%
                group_by(Decade)%>%
                summarise(temp=median(!!as.name(var)))
        
        names(label)[names(label)=="temp"] <- var
        
        plot <- df %>%
                filter(!is.na(Decade))%>%
                mutate(Decade = fct_reorder(factor(Decade),Release.Date))%>%
                ggplot(aes(x=Decade,y=!!as.name(var)))+
                geom_violin(color=color,fill=color,position="dodge",outlier.colour="transparent",alpha = 0.4,draw_quantiles = c(0.5))+
                geom_label(data = label, aes(label = !!as.name(var)%>%round(1)%>%format(nsmall=1)),color=color,vjust=-0.4,hjust=0.9)+
                theme_aes()+ 
                theme(legend.position = "NA")  + 
                ggtitle(title)
        
        return(plot)
}

