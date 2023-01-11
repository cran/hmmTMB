## ----dist-list, echo = FALSE--------------------------------------------------
dist_list <- lapply(hmmTMB:::dist_list, function(dist) dist$name_long())
par_list <- lapply(hmmTMB:::dist_list, function(dist) dist$parnames())
order <- order(names(par_list))
for(i in 1:length(par_list)) {
  j <- order[i]
  cat(paste0("\"", names(par_list)[j], "\": ", dist_list[[j]], 
             "(", paste0(par_list[[j]], collapse = ", "), ")\n"))
}

