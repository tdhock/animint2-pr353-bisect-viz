library(animint2)
library(data.table)
details.dt.list <- list()
dot.dt.list <- list()
results.dt.list <- list()
(Bisect.dir.vec <- Sys.glob("data/dinf-thock-02i/bisect/getCommonChunk_improved_in__238/*"))
age.colors <- c(
  old="violet",
  between="blue",
  new="red")
Age <- function(age)factor(age, names(age.colors))
for(Bisect.dir in Bisect.dir.vec){
  Bisect <- function(dt)dt[
  , Bisect := basename(Bisect.dir)
  ][]
  Commit <- function(dt)Bisect(dt)[
  , Commit := paste(Bisect, commit)
  ][]
  Test_commits <- fread(file.path(Bisect.dir, "Test_commits.csv"))
  Test_commits_long <- melt(
    Test_commits,
    measure.vars=measure(age=Age, pattern="(old|new)"),
    value.name="version")
  version.meta <- Test_commits_long[, .(
    version=c(version, "HEAD"),
    age=c(age, Age("between")))]
  for(csv in Sys.glob(file.path(Bisect.dir, "csv", "*"))){
    result.row <- Commit(fread(csv))
    results.dt.list[[csv]] <- melt( 
      result.row,
      measure.vars=measure(variable, version, sep=".")
    )[version.meta, on="version"]
  }
  for(result.RData in Sys.glob(file.path(Bisect.dir, "RData", "*"))){
    (objs <- load(result.RData))
    commit <- sub(".RData", "", basename(result.RData))
    details <- Commit(tres.or.status$measurements[, data.table(
      commit, version=expr.name, N, median, min, max)
    ])[version.meta, on="version"]
    details.dt.list[[paste(Bisect.dir, commit)]] <- details
    dot.dt.list[[paste(Bisect.dir, commit)]] <- Commit(tpred$prediction[, data.table(
      commit,
      unit.value,
      version=expr.name,
      N
    )][version.meta, on="version"])
  }
}
(details.dt <- rbindlist(details.dt.list))
(dot.dt <- rbindlist(dot.dt.list))
status.colors <- c(
  "0"="black",
  "125"="orange",
  "1"="white")
St <- function(s)factor(s, names(status.colors))
(results.dt <- rbindlist(results.dt.list)[, let(
  Status = St(status)
)][variable=="log10_N"])

timeline.dt <- results.dt[
, .SD[1]
, by=.(Bisect, commit, start, end)
][order(start)][
, bisect := factor(Bisect, unique(Bisect))
][]
to.min <- function(x, start)(x-min(start))/60
gg <- ggplot()+
  ggtitle(sprintf(
    "R data.table test case: %s",
    Test_commits$Test))+
  theme_bw()+
  geom_segment(aes(
    to.min(start, start), bisect,
    xend=to.min(end, start), yend=bisect),
    size=1,
    data=timeline.dt)+
  geom_point(aes(
    to.min(start, start), bisect),
    shape=1,
    data=timeline.dt)+
  scale_y_discrete(
    "Bisect run (old-new)")+
  scale_x_continuous(
    "Minutes from start of first git bisect (dot at start of each version tested)")
print(gg)

(ref.dt <- fread("refs.csv"))
(rank.dt <- unique(rbind(
  results.dt[, .(time, commit)],
  ref.dt[Rname %in% results.dt$version, .(time, commit)]
))[order(time)][, rank := .I][, .(commit, rank)])
results.rank <- rank.dt[results.dt, on="commit"]
HEAD.dt <- results.rank[version=="HEAD"]

HEAD.stats <- dcast(HEAD.dt, rank + commit + time + variable + age ~ ., list(min, median, max, length))
ggplot()+
  geom_ribbon(aes(
    rank,
    ymin=value_min,
    ymax=value_max,
    fill=age),
    alpha=0.5,
    data=HEAD.stats)+
  scale_color_manual(values=age.colors)+
  scale_fill_manual(values=age.colors)+
  geom_line(aes(
    rank, value_median, color=age),
    data=HEAD.stats)+
  facet_grid(variable ~ ., scales="free")

not.HEAD <- rank.dt[
  ref.dt[
    results.rank[version!="HEAD"][, .(Rname=version, variable, value)],
    on="Rname"],
  on="commit"]
med.line.list <- list(not.HEAD, HEAD.dt)
(med.line.dt <- dcast(rbindlist(lapply(
  med.line.list,
  function(DT)DT[, c("rank", "variable", "value")])),
  rank + variable ~ .,
  list(median=function(x)median(x, na.rm=TRUE), length)
)[, let(
  age = Age("between"),
  value_approx = approx(rank, value_median, rank)$y
)][])
ggplot()+
  scale_color_manual(values=age.colors)+
  geom_line(aes(
    rank, value_median, color=age),
    data=med.line.dt)+
  facet_grid(variable ~ ., scales="free")

ref.rank <- rank.dt[
  ref.dt,
  on="commit",
  nomatch=0L
][!c("Parent4558","PR7401"), on="Rname"][, y := max(med.line.dt$value_median)-.I*diff(range(med.line.dt$value_median))/.N]
ggplot()+
  theme_bw()+
  geom_hline(aes(
    yintercept=rank),
    color="grey",
    data=ref.rank)+
  geom_path(aes(
    value_median, rank),
    data=med.line.dt[variable=="log10_N"])+
  geom_text(aes(
    -Inf, rank, label=Rname,
    vjust=ifelse(grepl("Parent",Rname), 1, 0)),
    data=ref.rank,
    hjust=0,
    color="grey50")

(limit.dt <- data.table(seconds=tpred$seconds.limit))
gg <- ggplot()+
  ggtitle(sprintf(
    "R data.table test case: %s",
    Test_commits$Test))+
  theme_bw()+
  geom_vline(aes(
    xintercept=rank),
    color="grey",
    data=ref.rank)+
  geom_path(aes(
    rank, 10^value_median),
    data=med.line.dt[variable=="log10_N"])+
  geom_text(aes(
    rank, 10^y, label=sprintf(
      "%s\n%s", Rname, strftime(time, "%Y-%m-%d")),
      hjust=ifelse(grepl("Parent|Slow",Rname), 1, 0)),
    data=ref.rank,
    vjust=0,
    color="grey50")+
  scale_y_log10(sprintf("N = throughput at %s seconds", paste(limit.dt$seconds)))+
  scale_x_continuous("Commit rank over time")
print(gg)

#git merge-base --is-ancestor <possible-ancestor-commit> <commit>
#     git_commit_descendant_of(ancestor, ref = "HEAD", repo = ".")
repo <- "~/R/animint2"
ilist <- gert::git_commit_info("d6e41e69", repo=repo)
first.parents.dt.list <- list()
for(rank.i in 1:nrow(rank.dt)){
  rank.row <- rank.dt[rank.i]
  ilist <- gert::git_commit_info(rank.row$commit, repo=repo)
  for(parent in ilist$parents){
    parent.dt <- rank.dt[parent, on="commit", nomatch=0L]
    before.i <- rank.i-1
    while(nrow(parent.dt)==0 && 0<before.i){
      before.dt <- rank.dt[before.i]
      if(gert::git_commit_descendant_of(before.dt$commit, parent, repo=repo)){
        parent.dt <- before.dt
      }
      before.i <- before.i-1
    }
    if(nrow(parent.dt)==0){
      parent.dt <- data.table(commit=NA, rank=-Inf)
    }
    first.parents.dt.list[[paste(rank.i, parent)]] <- data.table(
      child=rank.row,
      parent.commit=parent,
      before=parent.dt)
  }
}
(first.parents.dt <- rbindlist(first.parents.dt.list))
first.parents.dt[parent==before.commit]
med.seg.dt.list <- list()
for(prefix in c("child","before")){
  on.vec <- c(rank=paste0(prefix,".rank"))
  med.seg.dt.list[[prefix]] <- med.line.dt[first.parents.dt, .(
    rank,
    value=ifelse(is.na(value_approx), -Inf, value_approx)
  ), on=on.vec]
}
(med.seg.dt <- do.call(data.table, med.seg.dt.list))
ggplot()+
  geom_segment(aes(
    child.rank, child.value,
    xend=before.rank, yend=before.value),
    data=med.seg.dt)+
  geom_point(aes(
    rank, value_approx, color=is.na(value_median)),
    data=med.line.dt)

HEAD.N <- HEAD.dt[variable=="log10_N"]
HEAD.N.stats <- dcast(
  HEAD.N,
  Bisect ~ .,
  list(length, min, max),
  value.var="rank"
)[order(rank_min, rank_max)][, y := .I][]
Bisect.range <- nc::capture_first_df(
  HEAD.N.stats,
  Bisect=list(old=".*", "-", new=".*"))
for(age in c("old", "new")){
  set(Bisect.range, j=paste0("rank_", age), value=ref.dt[
    Bisect.range[[age]],
    rank.dt[commit, .(rank), on="commit"],
    on="Rname"])
}
(Bisect.ext <- melt(
  Bisect.range,
  measure.vars=measure(
    value.name,
    age=Age,
    pattern="(.*)_(old|new)")))
(lim.segs <- results.rank[age!="between", {
  ##browser()
  .(
  med_value=median(value, na.rm=TRUE)
  )}, by=.(Bisect, variable, age)][Bisect.range, on="Bisect"])
(zoom.range <- dcast(
  results.rank,
  Bisect + variable ~ .,
  list(
    min=function(x)min(x,na.rm=TRUE),
    max=function(x)max(x,na.rm=TRUE)),
  value.var=c("value","rank")))
norm01 <- function(DT)DT[
  zoom.range,
  on=.(Bisect, variable)
][, let(
  rank_rel=rank-rank_min,
  value_norm=(value-value_min)/(value_max-value_min)
)][]
norm01(data.table(med.seg.dt)[, let(value=before.value, rank=before.rank)])
status.zoom <- norm01(status.points[, value := value_approx])
(points.zoom <- norm01(results.rank))
(segs.zoom <- zoom.range[, med.seg.dt[(
  child.rank %between% c(rank_min, rank_max) & child.value %between% c(value_min, value_max)
)|(
  before.rank %between% c(rank_min, rank_max) & before.value %between% c(value_min, value_max)
)], by=names(zoom.range)])
for(prefix in c("child", "before")){
  vname <- function(...)paste0(prefix, ".", c(...))
  segs.zoom[, vname("rank_rel", "value_norm") := list(
    get(vname("rank"))-rank_min,
    (get(vname("value"))-value_min)/(value_max-value_min)
  )]
}
bg.color <- "grey"
ref.rank <- rank.dt[
  ref.dt,
  on="commit",
  nomatch=0L
]
ref.color <- "deepskyblue"
line.size <- 1
href.dt <- unique(details.dt[, .(Commit, commit)])
status.points <- med.line.dt[
  HEAD.N[, .(Status, Bisect, rank)],
  on="rank"]
viz <- animint(
  out.dir="viz",
  title="animint2 git bisect performance analysis for PR353",
  source="https://github.com/tdhock/animint2-pr353-bisect-viz/blob/main/viz.R",
  bisects=ggplot()+
    ggtitle("Overview of all commits tested, select bisect")+
    theme_bw()+
    theme(legend.position="none")+
    theme_animint(height=700, width=600, rowspan=2)+
    geom_rect(aes(
      xmin=rank_old, xmax=rank_new,
      key=1,
      ymin=-Inf, ymax=Inf),
      alpha=0.3,
      color=NA,
      fill="grey",
      showSelected="Bisect",
      data=data.table(Bisect.range))+
    geom_tallrect(aes(
      xmin=rank-0.5, xmax=rank+0.5,
      key=rank),
      showSelected="Bisect",
      color=bg.color,
      fill=bg.color,
      data=data.table(HEAD.dt)[, panel := variable])+
    geom_rect(aes(
      xmin=rank_min, xmax=rank_max,
      ymin=value_min, ymax=value_max,
      key=1),
      color="black",
      fill="black",
      size=1,
      alpha=0.2,
      data=zoom.range[, panel := variable],
      showSelected="Bisect")+
    geom_vline(aes(
      xintercept=rank),
      color=ref.color,
      size=1,
      data=ref.rank)+
    geom_label_aligned(aes(
      rank, 0, label=sprintf(" %s ", Rname)),
      color=ref.color,
      alignment="horizontal",
      data=data.table(panel="Bisect", ref.rank))+
    geom_segment(aes(
      ifelse(age=="old", rank_old, rank_min), med_value,
      xend=ifelse(age=="old", rank_max, rank_new), yend=med_value,
      key=age,
      color=age),
      size=1,
      showSelected=c("age","Bisect"),
      data=data.table(lim.segs)[, panel := variable])+
    geom_segment(aes(
      x=rank_old, xend=rank_new,
      y=y, yend=y),
      size=4,
      clickSelects="Bisect",
      alpha_off=0.3,
      data=data.table(Bisect.range, panel = "Bisect"))+
    geom_text(aes(
      x=max(rank_new), y=1,
      key=1,
      label=Bisect),
      hjust=1,
      showSelected="Bisect",
      data=data.table(Bisect.range, panel = "Bisect"))+
    geom_segment(aes(
      x=rank_min, xend=rank_max,
      y=y, yend=y),
      clickSelects="Bisect",
      alpha_off=0.3,
      size=7,
      data=data.table(Bisect.range, panel = "Bisect"))+
    geom_point(aes(
      rank, value,
      key=paste(rank, age),
      color=age),
      size=3,
      showSelected=c("age","Bisect"),
      data=data.table(results.rank)[age!="between"][, panel := variable])+
    geom_segment(aes(
      child.rank, child.value,
      color=age,
      xend=before.rank, yend=before.value),
      size=line.size,
      data=data.table(med.seg.dt, panel="log10_N", age=Age("between")))+
    geom_point(aes(
      rank, value_approx,
      key=rank,
      fill=Status,
      color=age),
      size=3,
      showSelected=c("Status", "age","Bisect"),
      data=status.points[, panel := variable])+
    geom_tallrect(aes(
      xmin=rank-0.5, xmax=rank+0.5,
      key=rank),
      clickSelects="Commit",
      showSelected="Bisect",
      alpha=1,
      alpha_off=0,
      color="black",
      color_off=NA,
      fill=NA,
      data=data.table(HEAD.dt)[, panel := variable])+
    geom_point(aes(
      rank, y, color=age,
      key=paste(Bisect, age)),
      clickSelects="Bisect",
      alpha_off=0.3,
      showSelected="age",
      size=4,
      data=data.table(Bisect.ext, panel = "Bisect"))+
    facet_grid(panel ~ ., scales="free")+
    scale_color_manual(values=age.colors, drop=FALSE)+
    scale_fill_manual(values=status.colors, drop=FALSE)+
    scale_y_continuous("")+
    scale_x_continuous(
      "Commit rank over time",
      breaks=seq(1, 100, by=2)),
  details=ggplot()+
    ggtitle("Asymptotic timing results for selected commit")+
    theme_bw()+
    theme(legend.position="none")+
    theme_animint(height=300, width=600, last_in_row=TRUE)+
    geom_hline(aes(
      yintercept=seconds),
      color="grey",
      data=limit.dt)+
    geom_text(aes(
      0, seconds*1.1,
      label=sprintf(
        "time limit = %s seconds",
        paste(seconds)
      )),
      hjust=0,
      color="grey50",
      data=limit.dt)+
    geom_ribbon(aes(
      N, ymin=min, ymax=max,
      key=age, fill=age, group=age),
      showSelected=c("age","Commit"),
      alpha=0.2,
      color=NA,
      data=details.dt)+
    geom_line(aes(
      N, median,
      key=age, color=age, group=age),
      showSelected=c("age","Commit"),
      data=details.dt)+
    geom_point(aes(
      N, unit.value,
      key=age, color=age),
      showSelected=c("age","Commit"),
      fill="white",
      size=3,
      data=dot.dt)+
    geom_label_aligned(aes(
      N, unit.value, label=age,
      key=age, color=age),
      showSelected=c("age","Commit"),
      alignment="horizontal",
      vjust=-0.1,
      alpha=0.5,
      data=dot.dt)+
    geom_text(aes(
      0, 0,
      label=substring(commit, 1, 8),
      href=paste0("https://github.com/animint/animint2/commit/", commit),
      key=1),
      hjust=0,
      vjust=0,
      data=href.dt,
      showSelected="Commit")+
    scale_x_log10("N = data size")+
    scale_color_manual(values=age.colors)+
    scale_fill_manual(values=age.colors)+
    scale_y_log10("Computation time (seconds)"),
  zoom=ggplot()+
    ggtitle("Zoom to bisect, relative values")+
    theme_bw()+
    theme_animint(width=550)+
    geom_tallrect(aes(
      xmin=rank_rel-0.5, xmax=rank_rel+0.5,
      key=rank_rel),
      showSelected="Bisect",
      color=bg.color,
      fill=bg.color,
      data=points.zoom[version=="HEAD"])+
    geom_segment(aes(
      before.rank_rel, before.value_norm,
      color=age,
      xend=child.rank_rel, yend=child.value_norm,
      key=child.rank_rel),
      size=line.size,
      showSelected="Bisect",
      data=data.table(segs.zoom, age="between"))+
    geom_point(aes(
      rank_rel, value_norm,
      key=paste(rank, age),
      color=age),
      size=3,
      showSelected="Bisect",
      data=points.zoom[age!="between"])+
    geom_point(aes(
      rank_rel, value_norm,
      key=rank,
      fill=Status,
      color=age),
      size=3,
      showSelected="Bisect",
      data=status.zoom[age=="between"])+
    coord_cartesian(ylim=c(0,1))+
    facet_grid(variable ~ ., scales="free")+
    scale_color_manual(values=age.colors, drop=FALSE)+
    scale_fill_manual(values=status.colors, drop=FALSE)+
    scale_y_continuous("")+
    geom_tallrect(aes(
      xmin=rank_rel-0.5, xmax=rank_rel+0.5,
      key=rank_rel),
      clickSelects="Commit",
      showSelected="Bisect",
      alpha=1,
      color="black",
      color_off=NA,
      fill=NA,
      data=points.zoom[version=="HEAD"])+
    scale_x_continuous(
      "Commit rank relative to bisect",
      breaks=seq(1, 100, by=2)),
  duration=list(
    Commit=1000,
    age=1000,
    Bisect=1000)
)
viz

if(FALSE){
  animint2pages(viz, "2026-11-18-animint2-pr353", chromote_sleep_seconds=3)
  animint2::update_gallery("~/R/gallery-atime")
}
