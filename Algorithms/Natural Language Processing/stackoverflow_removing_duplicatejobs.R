similar_jobs <- read.csv("similar_stackoverflow_jobs.csv", header = TRUE, stringsAsFactors = FALSE)
final_delete_jobs_df <- NULL
unique_jobs <- unique(c(similar_jobs$a, similar_jobs$b))
cnt <- 1
for(job in unique_jobs){
  possible_delete_jobs <- c(similar_jobs[which(similar_jobs$a == job), 'b'], similar_jobs[which(similar_jobs$b == job), 'a'])
  delete_jobs_df <- data.frame(x = job, y = possible_delete_jobs)
  if(length(which(final_delete_jobs_df$y == job)) == 0){
    final_delete_jobs_df <- rbind(final_delete_jobs_df, delete_jobs_df)
  } else {
    df_y = delete_jobs_df[is.na(match(delete_jobs_df$y, final_delete_jobs_df$y)), 'y']
    if(length(df_y) > 0){
      print(cnt)
      delete_jobs_df <- data.frame(x = final_delete_jobs_df[which(final_delete_jobs_df$y == job)[1], 'x'],
                                   y = df_y)
      final_delete_jobs_df <- rbind(final_delete_jobs_df, delete_jobs_df)
    }
  }
  cnt <- cnt+1
}

current_files_in_directory <- list.files(path = "stackoverflow_jobs")
dir.create("unique_stackoverflow_jobs")
unique_files <- setdiff(as.character(current_files_in_directory), as.character(final_delete_jobs_df$y))
file.copy(from=currentfiles, to = "uique_stackoverflow_jobs", copy.mode = TRUE)