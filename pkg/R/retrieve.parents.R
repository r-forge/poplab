retrieve.parents <-
function (pop, bigpop) 
{
    pop <- pop[, c("ID", "yob", "sex", "m", "f", "yod", "yoi")]
    parents <- unique(c(pop[, "m"], pop[, "f"]))
    parents <- parents[parents %in% setdiff(parents, pop[, "ID"])]
    notaround <- bigpop[match(parents, bigpop[, "ID"]), ]
    colnames(notaround) <- c("ID", "yob", "sex", "m", "f", "yod", 
        "yoi")
    notaround[is.na(notaround[, "ID"]), "ID"] <- 0
    notaround[is.na(notaround[, "yob"]), "yob"] <- 0
    notaround[is.na(notaround[, "sex"]), "sex"] <- 0
    notaround[is.na(notaround[, "m"]), "m"] <- 0
    notaround[is.na(notaround[, "f"]), "f"] <- 0
    notaround[is.na(notaround[, "yod"]), "yod"] <- 0
    notaround[is.na(notaround[, "yoi"]), "yoi"] <- 0
    notaround <- notaround[notaround[, "ID"] != 0 & notaround[, 
        "yob"] != 0, ]
    return(notaround)
}
