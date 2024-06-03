
# Frames -------------------------------------------------------------------

# Load data
source("NI_Loading.R")

# Retrieve frames from Thomas' file

## Important: If I plan to use the df with the frames for further annotation, I have to use the lemmas from the data_raw file. I can't just remove stuff!

# Read in Thomas' file with all LUs 
frames <- read.table(file= "../Null Instantiation/FrameNet_all_LUs.csv", 	quote="", header=T, comment.char="", sep="\t", skipNul=T)

# Separate the word class from the LU
frames_sep <- separate_wider_delim(frames, cols = LEXICAL_UNIT, delim = ".", names = c("LEXICAL_UNIT", "POS"), too_few = "align_start")

# Remove white spaces
frames_sep$POS <- str_replace_all(frames_sep$POS, fixed(" "), "")

# Remove all nouns
frames_sep |> filter(POS == "v") -> frames_v

# Check matches
unique(NI_data$lemma) %in% frames_v$LEXICAL_UNIT

# Assign frames to my lemmas

## Small df for testing
NI_frames <- tibble(lemma = unique(data_raw$lemma),
                    frame =  frames_v[match(unique(data_raw$lemma), frames_v$LEXICAL_UNIT),]$FRAME)

## Do that for the main df

data_raw |> 
  mutate(frame = frames_v[match(data_raw$lemma, frames_v$LEXICAL_UNIT),]$FRAME) -> NI_data_frames

NI_data_frames |> 
  select(lemma, frame)

## Export the main df

write_xlsx(NI_data_frames, "Summary_28_03_2024_frames.xlsx")
