#' Plot an aibias object
#'
#' @description
#' Visualizes audit results. Supports four plot types:
#' - `"trajectory"`: Bias trajectory \eqn{B_{g,r}(t)} over time
#' - `"heatmap"`: Group-time disparity surface
#' - `"transition"`: Group-specific transition probabilities
#' - `"amplification"`: Amplification index \eqn{A_{g,r}(t)} over time
#'
#' @param x An `aibias` object.
#' @param type Character. Plot type. One of `"trajectory"`, `"heatmap"`,
#'   `"transition"`, `"amplification"`.
#' @param show_ci Logical. Show bootstrap CIs if available. Default TRUE.
#' @param color_palette Character vector of colors for groups. If NULL,
#'   uses a sensible default.
#' @param ... Ignored.
#'
#' @return A `ggplot2` object.
#'
#' @examples
#' \donttest{
#' data(lending_panel)
#' obj <- aib_audit(lending_panel,
#'                  id = "applicant_id", time = "year",
#'                  group = "race",     decision = "approved",
#'                  ref_group = "White", verbose = FALSE)
#' plot(obj, type = "trajectory")
#' plot(obj, type = "heatmap")
#' }
#'
#' @export
plot.aibias <- function(x,
                        type          = c("trajectory", "heatmap",
                                          "transition", "amplification"),
                        show_ci       = TRUE,
                        color_palette = NULL,
                        ...) {
  type <- match.arg(type)

  switch(type,
    trajectory   = .plot_trajectory(x, show_ci, color_palette),
    heatmap      = .plot_heatmap(x),
    transition   = .plot_transition(x, color_palette),
    amplification = .plot_amplification(x, color_palette)
  )
}


# ---- Trajectory plot -------------------------------------------------------

.plot_trajectory <- function(obj, show_ci, color_palette) {
  if (is.null(obj$bias))
    abort("Run `aib_describe()` before plotting type = 'trajectory'.")

  traj     <- obj$bias$trajectory
  ref      <- obj$bias$ref_group
  raw_times <- obj$meta$raw_times

  # Map integer times back to original labels
  inv_map  <- setNames(names(obj$meta$time_map), obj$meta$time_map)
  traj$time_label <- as.numeric(inv_map[as.character(traj$time)])

  # Bootstrap CI merge
  if (show_ci && !is.null(obj$bootstrap)) {
    ci    <- obj$bootstrap$ci
    ci$time_label <- as.numeric(inv_map[as.character(ci$time)])
    traj  <- traj |>
      left_join(ci |> select(.data$group, .data$time, .data$B_lo, .data$B_hi),
                by = c("group", "time"))
  }

  groups   <- unique(traj$group)
  pal      <- color_palette %||% .default_palette(length(groups))

  p <- ggplot(traj, aes(x = .data$time_label, y = .data$B_raw,
                        color = .data$group, fill = .data$group)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5)

  if (show_ci && !is.null(obj$bootstrap) && "B_lo" %in% names(traj)) {
    p <- p + geom_ribbon(aes(ymin = .data$B_lo, ymax = .data$B_hi),
                         alpha = 0.15, color = NA)
  }

  p +
    scale_color_manual(values = pal) +
    scale_fill_manual(values  = pal) +
    labs(
      title    = "Bias Trajectory",
      subtitle = paste0("Raw disparity vs. reference group: ", ref),
      x        = "Time",
      y        = expression(B[g*","*r](t)),
      color    = "Group",
      fill     = "Group"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}


# ---- Heatmap plot ----------------------------------------------------------

.plot_heatmap <- function(obj) {
  if (is.null(obj$bias))
    abort("Run `aib_describe()` before plotting type = 'heatmap'.")

  traj    <- obj$bias$trajectory
  inv_map <- setNames(names(obj$meta$time_map), obj$meta$time_map)
  traj$time_label <- as.character(inv_map[as.character(traj$time)])

  ggplot(traj, aes(x = .data$time_label, y = .data$group,
                   fill = .data$B_raw)) +
    geom_tile(color = "white", linewidth = 0.4) +
    scale_fill_gradient2(
      low      = "#d73027",
      mid      = "white",
      high     = "#1a9850",
      midpoint = 0,
      name     = "Disparity"
    ) +
    labs(
      title    = "Group-Time Disparity Surface",
      subtitle = paste0("Ref: ", obj$bias$ref_group,
                        "  |  Red = disadvantaged, Green = advantaged"),
      x        = "Time",
      y        = "Group"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold"),
      axis.text.x   = element_text(angle = 45, hjust = 1),
      panel.grid    = element_blank()
    )
}


# ---- Transition plot -------------------------------------------------------

.plot_transition <- function(obj, color_palette) {
  if (is.null(obj$transitions))
    abort("Run `aib_transition()` before plotting type = 'transition'.")

  pooled <- obj$transitions$pooled
  groups <- levels(pooled$group)
  pal    <- color_palette %||% .default_palette(length(groups))

  pooled$trans_label <- ifelse(
    pooled$from_state == 0, "Recovery (0 \u2192 1)", "Retention (1 \u2192 1)"
  )

  ggplot(pooled, aes(x = .data$group, y = .data$p_transition,
                     fill = .data$group)) +
    geom_col(position = "dodge", width = 0.6, alpha = 0.85) +
    geom_text(aes(label = round(.data$p_transition, 2)),
              vjust = -0.4, size = 3.5) +
    facet_wrap(~ .data$trans_label) +
    scale_fill_manual(values = pal) +
    labs(
      title    = "Transition Probabilities by Group",
      subtitle = "Recovery = P(D_t=1 | D_{t-1}=0)  |  Retention = P(D_t=1 | D_{t-1}=1)",
      x        = "Group",
      y        = "Probability",
      fill     = "Group"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold"),
      panel.grid.major.x = element_blank()
    )
}


# ---- Amplification plot ----------------------------------------------------

.plot_amplification <- function(obj, color_palette) {
  if (is.null(obj$amplification))
    abort("Run `aib_amplify()` before plotting type = 'amplification'.")

  idx     <- obj$amplification$index
  inv_map <- setNames(names(obj$meta$time_map), obj$meta$time_map)
  idx$time_label <- as.numeric(inv_map[as.character(idx$time)])

  groups <- unique(idx$group)
  pal    <- color_palette %||% .default_palette(length(groups))

  ggplot(idx, aes(x = .data$time_label, y = .data$A_index,
                  color = .data$group)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    scale_color_manual(values = pal) +
    labs(
      title    = "Bias Amplification Index",
      subtitle = expression(A[g*","*r](t) == B[g*","*r](t*"|"*1) - B[g*","*r](t*"|"*0)),
      x        = "Time",
      y        = expression(A[g*","*r](t)),
      color    = "Group"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}


# ---- Helpers ---------------------------------------------------------------

.default_palette <- function(n) {
  palettes <- list(
    `2` = c("#E41A1C", "#377EB8"),
    `3` = c("#E41A1C", "#377EB8", "#4DAF4A"),
    `4` = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"),
    `5` = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00")
  )
  if (as.character(n) %in% names(palettes)) {
    return(palettes[[as.character(n)]])
  }
  # Fallback to ggplot2 default
  hues <- seq(15, 375, length.out = n + 1)
  grDevices::hcl(h = hues, l = 65, c = 100)[seq_len(n)]
}

# Pipe-safe NULL coalescing
`%||%` <- function(a, b) if (is.null(a)) b else a
