library(tidyverse)

# Define a function to convert time format (min:sec) to seconds
time_to_seconds <- function(time) {
  parts <- strsplit(time, ":")[[1]]
  minutes <- as.numeric(parts[1])
  seconds <- as.numeric(parts[2])
  total_seconds <- minutes * 60 + seconds
  return(total_seconds)
}

# Timestamps
timestamps <- c(
  "00:04", "00:09", "00:12", "00:22", "00:28", "00:32", "01:12", "01:27",
  "02:04", "02:30", "02:33", "02:42", "02:52", "02:57", "03:01", "03:11",
  "03:23", "03:27", "03:32", "03:36", "03:44", "04:02", "04:10", "04:37",
  "04:40", "04:47", "04:51", "05:17", "06:25", "06:49", "07:19", "07:39",
  "08:56", "09:15", "10:22", "10:46", "11:44", "12:03", "13:10", "13:32",
  "14:18", "14:42", "14:55", "15:12", "16:43", "17:05", "18:29", "18:47",
  "19:39", "20:04", "21:14", "21:48", "22:43", "23:04", "23:51", "24:23",
  "24:48", "25:23", "26:10", "26:39", "27:12", "27:33", "27:57", "28:49",
  "28:56", "29:15", "29:24", "29:38", "29:43", "29:59", "30:05", "30:30",
  "31:08", "31:25", "32:05", "32:34", "32:39", "33:01", "33:49", "34:16",
  "34:35", "35:04", "36:10", "36:29", "36:45", "37:18", "37:29", "38:29",
  "38:55", "39:04", "39:12", "39:26", "39:33", "39:46"
)

# Convert timestamps to seconds
timestamps_seconds <- sapply(timestamps, time_to_seconds)

# Create speaker labels
speaker <- rep(c(1, 2), length(timestamps) / 2)

# Create the data frame
df <- data.frame(speaker = speaker, start_time = timestamps_seconds)
df$end_time <- c(df$start_time[-1] - 1, 2394)


df <- arrange(df, speaker)


# Convert data frame to list of lists for each speaker
speaker_lists <- lapply(split(df, df$speaker), function(x) {
  list_of_lists <- lapply(1:nrow(x), function(i) {
    c(x$start_time[i], x$end_time[i])
  })
  return(list_of_lists)
})

# Convert the lists to strings
speaker_strings <- lapply(speaker_lists, function(x) {
  paste0("[", paste0(apply(do.call(rbind, x), 1, paste, collapse=", "), collapse="],\n"), "]")
})

# Output the formatted strings
cat("Speaker 1:\n", speaker_strings[[1]], "\n\n", "Speaker 2:\n", speaker_strings[[2]], "\n")

write.csv(df, "/Users/Simon/Desktop/Sample_RP_Sequenced.csv")
