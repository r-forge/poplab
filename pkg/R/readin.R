readin <-
function (f.name, folder) 
{
    name <- scan(file = file.path(folder, f.name), quiet = TRUE, 
        what = "character", nlines = 1)
    pop <- matrix(scan(file = file.path(folder, f.name), skip = 1, 
        quiet = TRUE), ncol = length(name), byrow = TRUE)
    colnames(pop) <- name
    pop
}
