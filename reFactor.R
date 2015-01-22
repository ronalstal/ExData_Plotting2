reFactor <- function(x) {
    
    ## reFactor(x):
    ##     x is a data.frame or data.table
    ##     re-calculates the levels of all factors in x
    ##     returns the modified x
    
    # get all columns of x
    for (colStr in objects(x)) {
        # column as an evaluated expression
        col <- eval(parse(text=paste0("x$",colStr)))
        # test if col is a factor
        if (is.factor(col)) {
            # re-factor this col
            eval(parse(text=paste0("x$",colStr,"<- as.factor(as.character(col))")))
        }
    }
    return(x)
}