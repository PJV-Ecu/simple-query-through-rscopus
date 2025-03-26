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
        "Universidad Casa Grande",
        "Universidad Tecnológica ECOTEC",
        "Escuela Politécnica Nacional",
        "Escuela Superior Politécnica Agropecuaria de Manabí",
        "Universidad de las Fuerzas Armadas",
        "Escuela Superior Politécnica de Chimborazo",
        "Escuela Superior Politécnica del Litoral",
        "Facultad Latinoamericana de Ciencias Sociales",
        "Instituto de Altos Estudios Nacionales",
        "Pontificia Universidad Católica del Ecuador",
        "Universidad Particular Internacional SEK",
        "Universidad Agraria del Ecuador",
        "Universidad Andina Simón Bolívar",
        "Universidad Católica de Cuenca",
        "Universidad Central del Ecuador",
        "Universidad Católica de Santiago de Guayaquil",
        "Universidad de Cuenca",
        "Universidad del Azuay",
        "Universidad de Las Américas",
        "Universidad Estatal Amazónica",
        "Universidad Estatal de Bolívar",
        "Universidad Particular de Especialidades Espíritu Santo",
        "Universidad de Guayaquil",
        "Universidad de los Hemisferios",
        "Universidad Internacional del Ecuador",
        "Universidad Tecnológica Israel",
        "Universidad Laica Eloy Alfaro de Manabí",
        "Universidad Laica Vicente Rocafuerte de Guayaquil",
        "Universidad Metropolitana",
        "Universidad Nacional de Chimborazo",
        "Universidad Estatal de Milagro",
        "Universidad Estatal del Sur de Manabí",
        "Universidad Regional Autónoma de los Andes",
        "Universidad Iberoamericana del Ecuador",
        "Universidad Nacional de Loja",
        "Universidad de Otavalo",
        "Universidad del Pacífico Escuela de Negocios",
        "Universidad Politécnica Estatal del Carchi",
        "Universidad Politécnica Salesiana",
        "Universidad Estatal Península de Santa Elena",
        "Universidad San Francisco de Quito",
        "Universidad Particular San Gregorio de Portoviejo",
        "Universidad Técnica de Ambato",
        "Universidad Técnica de Cotopaxi",
        "Universidad UTE",
        "Universidad Tecnológica Empresarial de Guayaquil",
        "Universidad Técnica Estatal de Quevedo",
        "Universidad Tecnológica Indoamérica",
        "Universidad Técnica de Manabí",
        "Universidad Técnica de Machala",
        "Universidad Técnica del Norte",
        "Universidad Técnica Particular de Loja",
        "Universidad de Especialidades Turísticas",
        "Universidad Técnica de Babahoyo",
        "Universidad Técnica Luis Vargas Torres de Esmeraldas",
        "Universidad de Investigación de Tecnología Experimental Yachay",
        "Universidad Regional Amazónica Ikiam",
        "Universidad Nacional de Educación",
        "Universidad de las Artes",
        "Universidad Intercultural de las Nacionalidades Y Pueblos Indígenas Amawtay Wasi",
        "Universidad del Río",
        "Universidad Bolivariana del Ecuador",
        "Universidad de Seguridad Ciudadana y Ciencias Policiales"
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
