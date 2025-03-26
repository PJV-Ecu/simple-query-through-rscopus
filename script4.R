#Useful references

#https://cran.r-project.org/web/packages/rscopus/vignettes/api_key.html
#https://cran.r-project.org/web/packages/rscopus/vignettes/multi_author.html
#https://github.com/HBLL-Collection-Development/scopus-api/blob/master/README.md
#https://www.ilovephd.com/list-of-subject-areas-covered-by-scopus-database/

library(pacman)

p_load("rscopus", "dplyr", "tidyr", "ggplot2", "ggthemes", "stringr")

#The script will loop through each of the institutions defined in the 'univ" variable and produce a barplot with the total number of publications for all subject areas in the Scopus database and for an specific year.  

if (have_api_key()) {
    make_query <- function(univ_name) {
        paste0("AFFIL(\"", univ_name, "\") AND PUBYEAR = 2024") #opción: AND ACCESSTYPE(OA)
    }
    
    univ <- c(
        "Universidad Central del Ecuador",
        "Escuela Politécnica Nacional",
        "Universidad San Francisco de Quito",
        "Pontificia Universidad Católica del Ecuador",
        "Universidad de Cuenca",
        "Universidad Técnica Particular de Loja",
        "Universidad Técnica de Ambato",
        "Universidad de Guayaquil",
        "Escuela Superior Politécnica del Litoral",
        "Universidad Agraria del Ecuador",
        "Universidad Estatal de Milagro",
        "Universidad Técnica de Manabí",
        "Universidad Técnica Estatal de Quevedo",
        "Universidad Nacional de Chimborazo",
        "Universidad Estatal Península de Santa Elena",
        "Universidad Estatal de Bolívar",
        "Universidad Estatal Amazónica",
        "Universidad de las Fuerzas Armadas ESPE",
        "Universidad Andina Simón Bolívar",
        "Universidad Internacional del Ecuador",
        "Universidad Católica de Santiago de Guayaquil",
        "Universidad Casa Grande",
        "Universidad del Azuay",
        "Universidad Tecnológica ECOTEC",
        "Universidad SEK",
        "Universidad Tecnológica Indoamérica",
        "Universidad Politécnica Salesiana",
        "Universidad Laica Vicente Rocafuerte de Guayaquil",
        "Universidad Laica Eloy Alfaro de Manabí",
        "Universidad Estatal del Sur de Manabí",
        "Universidad Estatal del Norte de Esmeraldas Luis Vargas Torres",
        "Universidad Estatal de Loja",
        "Universidad Estatal de Portoviejo",
        "Universidad Estatal de Santa Elena",
        "Universidad Estatal de Riobamba",
        "Universidad Estatal de Quevedo",
        "Universidad Técnica Luis Vargas Torres de Esmeraldas",
        "Universidad Técnica de Babahoyo",
        "Universidad Técnica de Cotopaxi",
        "Universidad Técnica de Machala",
        "Universidad Técnica del Norte",
        "Universidad Técnica Estatal de Bolivar",
        "Universidad Estatal del Carchi",
        "Universidad Regional Amazónica Ikiam",
        "Universidad de Investigación de Tecnología Experimental Yachay",
        "Universidad Nacional de Educación"
    )
    #i <- 1
    #subj_area <- subject_areas()[i] # Selects the first subject area
    #print(subj_area)
    
    results_df <- data.frame(University = character(), Total_Results = numeric(), stringsAsFactors = FALSE)
    
    for (university in univ) {
        query <- make_query(university)
        print(paste("Searching for:", university))
        completeArticle <- scopus_search(
            query = query,
            view = "STANDARD",
            count = 25
        )
        if (!is.null(completeArticle)) {
            total_results <- completeArticle$total_results
            total_results <- as.numeric(total_results)
            print(paste("Total results for", university, ":", total_results))
            print(names(completeArticle))
            results_df <- rbind(results_df, data.frame(University = university, Total_Results = total_results, stringsAsFactors = FALSE))
        } else {
            print(paste("No results found for", university))
            results_df <- rbind(results_df, data.frame(University = university, Total_Results = NA, stringsAsFactors = FALSE))
        }
    }
    
    # Create the bar plot
    plot <- ggplot(results_df, aes(y = reorder(University, Total_Results), x = Total_Results, colour = University,
                                   xend = 0, yend = University)) +
        geom_segment(linewidth = 3) +
        geom_point(size = 5) +
        geom_vline(xintercept = 881, linetype = 2, colour = "grey20") +
        theme_solarized_2() +
        labs(
            title = "Publicaciones en todas las áreas",
            x = "Número de publicaciones Scopus",
            y = "Institutos públicos de investigación"
        ) +
        theme(legend.position = "none")
    
    print(plot)
    
    ggsave("plot3.png", plot, width = 13.14, height = 8.12) #golden ratio is used
    
} else {
    print("API key not found. Please set your API key using set_api_key().")
}
