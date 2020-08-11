library(miic)

compare_version = "1.4.2"

# Comparing results to saved
compare_miic_results <- function(adj_matrix, ...){

    res_file = paste(..., sep="_")
    #write.table(adj_matrix, paste0(compare_version, "/", res_file, ".txt"))
    compare_adj_matrix = read.table(paste0(compare_version, "/", res_file, ".txt"))

    return(all(adj_matrix == compare_adj_matrix))
}

# Run miic with neuro network
run_miic_neuro <- function(...){

    message("Testing parameters : ")
    message(paste(..., sep=" "))

    res <- tryCatch({
        set.seed(0)
        miic(input_data = input_df, state_order = state_order,  ...=...)
    }, error = function(cond){
        message(cond)
        return(NULL)
    })

    if(is.null(res))
        return(FALSE)
    else{
        return(compare_miic_results(res$adj_matrix, ...))
    }
}


# Apply function FUN to all combinations of arguments and append results to
# data frame of arguments
cmapply <- function(FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE,
    USE.NAMES = TRUE)
{
    l <- expand.grid(..., stringsAsFactors=FALSE)
    # Filter out nonsensical combinations
    l <- l[!(l$orientation == FALSE & l$propagation == TRUE),]
    l <- l[!(l$orientation == FALSE & l$latent == "ori"),]
    l <- l[!(l$orientation == FALSE & l$consistent == "orientation"),]

    test_result <- do.call(mapply, c(
        list(FUN=FUN, MoreArgs = MoreArgs, SIMPLIFY = SIMPLIFY, USE.NAMES = USE.NAMES),
        l
    ))
    if (is.matrix(test_result)) r <- t(test_result)
    cbind(l, test_result)
}


################################################################################

# Import data
set.seed(1)
N_rows = 400

# EXAMPLE HEMATOPOIESIS
data(hematoData)
input_df <- hematoData
input_df <- input_df[sample(1:nrow(input_df),N_rows),]
state_order <- NULL

# Run all tests
tests_results = cmapply(FUN=run_miic_neuro,
                        test_mar=c(T,F),
                        orientation=c(T,F),
                        propagation=c(T,F),
                        consistent=c("no", "skeleton", "orientation"),
                        n_shuffles = c(0, 100),
                        conf_threshold = c(1e-1),
                        latent=c("no","yes","ori"))

# Check results
if(any(tests_results$test_result==FALSE)){
    message("Some tests failed : ")
    message(paste0(capture.output(tests_results[tests_results$test_result==FALSE,]), collapse = "\n"))
    stop()
} else{
    message("All tests passed!")
}
