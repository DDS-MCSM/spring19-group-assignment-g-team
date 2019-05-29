library(R.utils)
install.packages("jsonlite")

dir.create("./data")
if (!file.exists ("./data/2016-11-heisenberg-cowrie.tar.bz2"))
{
download.file(url == "https://opendata.rapid7.com/heisenberg.cowrie/2016-11-heisenberg-cowrie.tar.bz2")
}
# uncompress data  bzip2
# R.utils::bunzip2("./data/2016-11-heisenberg-cowrie.tar.bz2","./dataunzipped") --- No és correcte el pas de parametres

# load data
  raw_data <- file("./data/2016-11-cowrie/1_13_0_part.json")
  df <- jsonlite::stream_in(raw_data)
  write.csv (df,file = "./data/1_5_0.csv")


