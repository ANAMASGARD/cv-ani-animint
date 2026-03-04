# port of cv.ani() from the animation package
# https://yihui.org/animation/example/cv-ani/

library(animint2)

set.seed(42)

N <- 150
k <- 10
x <- runif(N)
x <- x[sample(N)]

# split N points into k folds as evenly as possible
fold_sizes <- rep(N %/% k, k)
if (N %% k > 0) fold_sizes[1:(N %% k)] <- fold_sizes[1:(N %% k)] + 1
kf <- cumsum(c(1, fold_sizes))

# static borders — all 10 fold outlines, always visible
fold_rects <- data.frame(
  fold_id = 1:k,
  xmin    = kf[1:k] - 0.5,
  xmax    = kf[2:(k+1)] - 0.5,
  ymin    = min(x),
  ymax    = max(x)
)

# green box that moves to highlight the current test fold
highlight_rect <- data.frame(
  fold = 1:k,
  xmin = kf[1:k] - 0.5,
  xmax = kf[2:(k+1)] - 0.5,
  ymin = min(x),
  ymax = max(x)
)

# every point labelled Test or Train for each fold turn
points_data <- do.call(rbind, lapply(1:k, function(fold_i) {
  test_idx <- kf[fold_i]:(kf[fold_i + 1] - 1)
  data.frame(
    index = 1:N,
    value = x,
    fold  = fold_i,
    role  = ifelse(1:N %in% test_idx, "Test", "Train")
  )
}))

# one row per fold for the selector plot
fold_sizes_df <- data.frame(
  fold = 1:k,
  size = diff(kf)
)

# plot 1:- main cv diagram 
plot_cv <- ggplot() +
  geom_rect(
    data = fold_rects,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = NA, color = "gray60", linetype = "dashed"
  ) +
  geom_rect(
    data = highlight_rect,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    showSelected = "fold",
    fill = "lightgreen", alpha = 0.5, color = "green4"
  ) +
  geom_point(
    data = points_data,
    aes(x = index, y = value, color = role, key = index),
    showSelected = "fold",
    size = 3
  ) +
  scale_color_manual(values = c("Test" = "tomato", "Train" = "steelblue")) +
  labs(
    title = "k-Fold Cross Validation (k = 10)",
    x = "Sample Index", y = "Value",
    color = ""
  ) +
  theme_bw()

# plot 2: fold selector — gray bars always visible, red = current fold ---
plot_folds <- ggplot(fold_sizes_df, aes(x = fold, y = size, key = fold)) +
  geom_bar(
    stat = "identity", position = "identity",
    fill = "gray80", color = "gray40"
  ) +
  geom_bar(
    stat = "identity", position = "identity",
    showSelected = "fold",
    clickSelects = "fold",
    fill = "tomato", color = "gray40"
  ) +
  scale_x_continuous(breaks = 1:k) +
  labs(
    title = "Click a fold  |  red = current test fold",
    x = "Fold", y = "Test set size"
  ) +
  theme_bw()

# deployed on GitHub Pages
viz <- animint(
  title  = "Demonstration of k-Fold Cross Validation",
  cvplot = plot_cv,
  folds  = plot_folds,
  time   = list(variable = "fold", ms = 800),
  source = "https://github.com/ANAMASGARD/cv-ani-animint/blob/main/cv_ani_viz.R"
)

animint2pages(viz, github_repo = "ANAMASGARD/cv-ani-animint")


## local preview (uncomment to run locally instead)
#viz <- animint(
#  title  = "Demonstration of k-Fold Cross Validation",
#  cvplot = plot_cv,
#  folds  = plot_folds,
#  time   = list(variable = "fold", ms = 800)
#)
#animint2dir(viz, out.dir = "cv_ani_output", open.browser = TRUE)
