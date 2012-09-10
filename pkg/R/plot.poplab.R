plot.poplab <-
function (x, option, population.fem, population.male, year, folder, 
    ...) 
{
    if (missing(option)) {
        option <- "current"
    }
    if (option == "current") 
        pop.t <- x[[2]]
    if (option == "base") {
        if (is.null(dim(x)[2])) 
            pop.t <- x[[1]]
        if (!is.null(dim(x)[2])) 
            pop.t <- x
    }
    realfem <- readin(f.name = population.fem, folder)
    realmale <- readin(f.name = population.male, folder)
    if ((!(year %in% as.integer(colnames(realfem)[-1]))) | (!(year %in% 
        as.integer(colnames(realmale)[-1])))) 
        stop(paste("No female or male population counts for the year", 
            year, "in data files"))
    realmale <- realmale[, c(1, which(colnames(realmale) == year, 
        arr.ind = TRUE))]
    colnames(realmale) <- c("age", "counts")
    realfem <- realfem[, c(1, which(colnames(realfem) == year, 
        arr.ind = TRUE))]
    colnames(realfem) <- c("age", "counts")
    draw.profile(pop.t, year, realmale, realfem)
}
