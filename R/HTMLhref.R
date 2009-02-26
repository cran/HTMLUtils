`HTMLhref` <- function (href, txt, file = get(".HTML.file"), append = TRUE) 
{
    cat(paste('<a href=\"', href, '\"> ', txt, ' </a>', sep = ""), append = append,  file = file)
}