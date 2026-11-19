library(data.table)
Test <- "getCommonChunk improved in #238"
src.dir <- file.path("~/R/animint2/.ci/atime/bisect", atime:::test_file_name(Test))
(tests.R.vec <- Sys.glob(file.path(src.dir, "*", "tests.R")))
ref.dt.list <- list()
for(tests.R in tests.R.vec){
  eval(parse(tests.R))
  tdata <- test.list[[Test]]
  ref.vec <- unlist(unname(tdata[sapply(tdata, is.character)]))
  ref.dt.list[[tests.R]] <- data.table(Rname=names(ref.vec), ref=ref.vec)
}
(data.dir <- file.path("data", system("hostname", intern=TRUE), basename(dirname(src.dir))))
unlink(data.dir, recursive = TRUE)
dir.create(data.dir, showWarnings = FALSE, recursive = TRUE)
file.copy(src.dir, data.dir, recursive = TRUE)
(ref.dt <- unique(rbindlist(ref.dt.list))[
, gert::git_log(ref, 1, repo="~/R/animint2")
, by=.(Rname, ref)][, .(Rname, commit, time)])
fwrite(ref.dt, "refs.csv")
