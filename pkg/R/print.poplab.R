print.poplab <-
function (x, option, year, folder, ...) 
{
    require(MASS)
    if (option == "current") {
        pop.t <- x[[2]]
        colnames(pop.t) <- c("ID", "yob", "sex", "m", "f", "yod", 
            "yoi")
        pop.t <- pop.t[pop.t[, "yob"] <= year, ]
    }
    if (option == "base") {
        if (is.null(dim(x)[2])) 
            pop.t <- x[[1]]
        if (!is.null(dim(x)[2])) 
            pop.t <- x
    }
    where <- switch(option, base = paste("base_pop_", year, ".txt", 
        sep = ""), current = paste("simpop_endYr_", year, ".txt", 
        sep = ""))
    write.matrix(pop.t, file = file.path(folder, where), sep = " ")
    cat("Data saved to", folder, "folder as", where, "file \n")
}
