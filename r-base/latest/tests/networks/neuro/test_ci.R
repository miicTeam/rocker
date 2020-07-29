library(miic)
df_1 <- read.table("data/input.tsv", header=T, sep="\t", nrows=500)
#df_1 <- read.table("data/input.tsv", header=T, sep="\t")
df_2 <- read.table("data/52var_20_09_2019.tsv", header=T, sep="\t", nrows=500)
#df_2 <- read.table("data/52var_20_09_2019.tsv", header=T, sep="\t", nrows=5000)
state_order_1 <- read.table("data/state_order_1.tsv", header=T)
state_order_2 <- NULL
# consistent
list_c <- c("no", "no", "no", "no", "ske", "ori", "no")
# latent
#list_l <- c("no", "no", "ort", "yes", "no", "no", "no")
list_l <- c("no", "no", "ori", "yes", "no", "no", "no")
# test_mar
list_t <- c(F, rep_len(T, 6))
# n_shuffles
list_s <- c(rep_len(0, 6), 10)
for (data_id in c(1:2)) {
  # test config
  df <- eval(parse(text=paste("df", data_id, sep="_")))
  order <- eval(parse(text=paste("state_order", data_id, sep="_")))
  # output
  for (test_id in c(1:7)) {
    set.seed(0)
    res <- miic(df,
                #---  parameters in older versions
                #categoryOrder=order,
                #confidenceShuffle=list_s[test_id],
                #confidenceThreshold=0.1,
                #testMAR=list_t[test_id],
                #nThreads=4,
                #---
                state_order=order,
                n_shuffles=list_s[test_id],
                conf_threshold=0.1,
                test_mar=list_t[test_id],
                n_threads=4,
                #---
                consistent=list_c[test_id],
                latent=list_l[test_id]
    )
    adj <- paste0(paste("out/adj_base", data_id, test_id, sep="_"), ".tsv")
    write.table(res$adj_matrix, adj)
    print(paste0(paste("Test", data_id, test_id, sep="_"), ": "))
    print(res$time)
    rm(res)
  }
}
