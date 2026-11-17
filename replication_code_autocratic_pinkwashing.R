# =============================================================================
# Replication code for "The Curious Case of Autocratic Pinkwashing: Ana Brnabić and LGBTQ+ Rights in Serbia" for figures 2 and 3.
#
#   Figure 2  Perceived effectiveness of the government in combating prejudice
#             and intolerance against LGBTIQ people, Serbia
#             Source: EU LGBTIQ Survey III (FRA, 2023), item D5
#   Figure 3  (a) Attitudes towards gays and lesbians in Serbia, ESS rounds 9-11
#             (b) LGB attitudes across South-East Europe, LiTS IV (2022-23)
#
# How to run
#   Set the working directory to the folder containing this
#   script and run the whole script. Required input files, in ./data/ :
#     lits_iv.dta                   EBRD Life in Transition Survey IV
#     ESS country data - RS.dta     European Social Survey, Serbia, rounds 9-11
#     eu_lgbt_survey.dta            FRA EU LGBTIQ Survey III
#   Output, written to ./output/ :
#     figure2_government.pdf / .png
#     figure3_attitudes.pdf  / .png
#
# =============================================================================

library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(stringr)

data_dir <- "data"
out_dir  <- "output"
dir.create(out_dir, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 0. Shared settings (figure size, fonts, devices, colours)
# -----------------------------------------------------------------------------

fig_width <- 17   # cm, two-column journal width
base_pt   <- 8    # base font size in points

font_family <- if (requireNamespace("systemfonts", quietly = TRUE) &&
                   "Inter" %in% systemfonts::system_fonts()$family) "Inter" else "sans"

# Vector PDF via Cairo when available; otherwise R's pdf() with Helvetica
# (on a Mac, Cairo needs XQuartz: xquartz.org)
has_cairo <- tryCatch({
  f <- tempfile(fileext = ".pdf"); cairo_pdf(f); grid::grid.newpage(); dev.off(); TRUE
}, warning = function(w) FALSE, error = function(e) FALSE)
if (!has_cairo) font_family <- "sans"
pdf_device <- if (has_cairo) cairo_pdf else
  function(filename, width, height, ...)
    grDevices::pdf(filename, width = width, height = height, encoding = "CP1250")
png_device <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"

save_fig <- function(plot, stem, height) {
  ggsave(file.path(out_dir, paste0(stem, ".pdf")), plot, width = fig_width,
         height = height, units = "cm", device = pdf_device)
  ggsave(file.path(out_dir, paste0(stem, ".png")), plot, width = fig_width,
         height = height, units = "cm", dpi = 600, bg = "white", device = png_device)
}

ink      <- "#1f1f1d"
ink_soft <- "#6b6a65"
grid_col <- "#e6e4dd"

# =============================================================================
# FIGURE 2: Government response, EU LGBTIQ Survey III, Serbia
# =============================================================================

# -----------------------------------------------------------------------------
# 1. Data: respondents living in Serbia (A9 == 30)
# -----------------------------------------------------------------------------

lgbt_survey <- read_dta(file.path(data_dir, "eu_lgbt_survey.dta"))


lgbt_rs <- lgbt_survey %>%
  filter(A9 == 30)

num <- function(x) as.numeric(zap_labels(x))

# -----------------------------------------------------------------------------
# 2. D5: "Do you think the government in Serbia effectively combats prejudice
#    and intolerance against LGBTIQ people?"
#    Original codes: 1 = yes, definitely ... 4 = no, definitely not; -99 = DK.
#    Recoded to run from "No, definitely not" to "Yes, definitely", then DK.
# -----------------------------------------------------------------------------

# Diverging colours: orange = worse, blue = better, pale grey = don't know
col_worse2  <- "#b8461a"   # no, definitely not
col_worse1  <- "#f2a07a"   # no, probably not
col_better1 <- "#86b6ef"   # yes, probably
col_better2 <- "#1c5cab"   # yes, definitely
col_dk      <- "#ebe9e2"   # don't know

gov_levels <- c("No, definitely not", "No, probably not", "Yes, probably",
                "Yes, definitely", "Don't know")
gov_fills  <- setNames(c(col_worse2, col_worse1, col_better1, col_better2, col_dk),
                       gov_levels)

d5 <- lgbt_rs |>
  transmute(value = num(D5)) |>
  filter(!is.na(value)) |>
  mutate(item = factor("Government"),
         category = factor(case_when(value == 4 ~ 1, value == 3 ~ 2, value == 2 ~ 3,
                                     value == 1 ~ 4, value == -99 ~ 5),
                           levels = 1:5, labels = gov_levels)) |>
  count(item, category, name = "n_cat", .drop = FALSE)


# -----------------------------------------------------------------------------
# 3. Plot: 100% stacked bar (unweighted percentages of respondents)
# -----------------------------------------------------------------------------

theme_gov <- function() {
  theme_minimal(base_size = base_pt, base_family = font_family) +
    theme(text                 = element_text(colour = ink),
          axis.text            = element_text(colour = ink),
          axis.text.x          = element_text(colour = ink_soft),
          panel.grid           = element_blank(),
          plot.title.position  = "plot",
          legend.position      = "bottom",
          legend.justification = "center",
          legend.text          = element_text(size = base_pt - 1,
                                              margin = margin(l = 1.5, r = 4)),
          legend.key.size      = unit(2.8, "mm"),
          legend.margin        = margin(2, 0, 0, 0),
          plot.margin          = margin(4, 8, 8, 4))
}

stacked_bars <- function(data_long, fills) {
  d <- data_long |>
    group_by(item) |>
    mutate(pct = 100 * n_cat / sum(n_cat)) |>
    ungroup() |>
    mutate(label = if_else(pct >= 4, sprintf("%.0f", pct), ""),
           txt   = if_else(category %in% names(fills)[fills %in% c(col_worse2, col_better2)],
                           "white", ink))
  ggplot(d, aes(x = pct, y = item, fill = category)) +
    geom_col(position = position_stack(reverse = TRUE), width = 0.62,
             colour = "white", linewidth = 0.4) +
    geom_text(aes(label = label, colour = txt, group = category),
              position = position_stack(vjust = 0.5, reverse = TRUE),
              size = (base_pt - 0.5) / .pt, family = font_family) +
    scale_fill_manual(values = fills, name = NULL, drop = FALSE) +
    scale_colour_identity() +
    scale_x_continuous(labels = function(x) paste0(x, "%"),
                       breaks = seq(0, 100, 25), expand = c(0, 0)) +
    scale_y_discrete(labels = function(x) str_wrap(x, 22)) +
    labs(x = NULL, y = NULL) +
    guides(fill = guide_legend(nrow = 1)) +
    theme_gov()
}

# Panel title and question wording are given in the paper caption
fig2 <- stacked_bars(d5, gov_fills) +
  theme(axis.text.y = element_blank(),
        plot.margin = margin(6, 8, 4, 4))

save_fig(fig2, "figure2_government", height = 4.5)

# Numbers behind Figure 2 (percent of respondents who answered D5)
print(d5 |> mutate(pct = round(100 * n_cat / sum(n_cat), 1)))


# =============================================================================
# FIGURE 3: (a) ESS Serbia, rounds 9-11; (b) LiTS IV, South-East Europe
# =============================================================================

# -----------------------------------------------------------------------------
# 4. Panel a data: ESS Serbia, rounds 9-11
#    Original coding: 1 = agree strongly ... 5 = disagree strongly
#    Recoded:         1 = disagree strongly ... 5 = agree strongly
#    (higher = stronger agreement with the statement)
# -----------------------------------------------------------------------------

data_serbia_ess <- read_dta(file.path(data_dir, "ESS country data - RS.dta"))

clean_ess <- function(x) {
  x <- as.numeric(zap_labels(x))
  replace(x, x > 5, NA)          # safety net for 7/8/9 missing codes
}

ess_rs <- data_serbia_ess %>%
  mutate(
    round     = factor(essround, levels = c(9, 10, 11),
                       labels = c("2018", "2022", "2024")),
    freehms_a  = 6 - clean_ess(freehms),    # free to live as they wish
    hmsacld_a  = 6 - clean_ess(hmsacld),    # same adoption rights
    hmsfmlsh_a = 6 - clean_ess(hmsfmlsh)    # would feel ashamed of family member
  )

item_labels_ess <- c(
  freehms_a  = "Gays and lesbians should be free to live their own life as they wish",
  hmsacld_a  = "Gay and lesbian couples should have the same rights to adopt children as straight couples",
  hmsfmlsh_a = "If a close family member was gay or lesbian, I would feel ashamed"
)

# Weighted means (anweight) with 95% CIs from the SE of the weighted mean
trends <- ess_rs %>%
  select(round, anweight, all_of(names(item_labels_ess))) %>%
  pivot_longer(all_of(names(item_labels_ess)), names_to = "item", values_to = "value") %>%
  filter(!is.na(value)) %>%
  group_by(round, item) %>%
  summarise(
    avg = sum(anweight * value) / sum(anweight),
    se  = sqrt(sum(anweight^2 * (value - avg)^2)) / sum(anweight),
    n   = n(),
    .groups = "drop"
  ) %>%
  mutate(
    lo   = avg - 1.96 * se,
    hi   = avg + 1.96 * se,
    item = factor(item, levels = names(item_labels_ess), labels = item_labels_ess)
  )

# -----------------------------------------------------------------------------
# 5. Panel b data: LiTS IV, ten South-East European countries
#    q829a/q829b: 1 = strongly disagree ... 4 = strongly agree
#    q827aa:      1 = very widespread ... 4 = very rare (reversed below so that
#                 higher = more widespread)
#    -99 (refused) and -97 (don't know) are set to missing.
# -----------------------------------------------------------------------------

lits_iv <- read_dta(file.path(data_dir, "lits_iv.dta"))

see_codes <- c(1, 5, 6, 7, 13, 17, 24, 26, 28, 30)

lits_see <- lits_iv %>%
  filter(country %in% see_codes) %>%
  mutate(country = droplevels(as_factor(country)))

lits_see <- lits_see %>%
  mutate(country_short = dplyr::recode(country,
                                       "Bosnia and Herzegovina" = "BiH",
                                       "North Macedonia"        = "N. Macedonia"))

stopifnot(nlevels(lits_see$country_short) == 10,
          "Serbia" %in% levels(lits_see$country_short))




clean_item <- function(x) {
  x <- as.numeric(zap_labels(x))
  replace(x, x %in% c(-99, -97), NA)
}

lits_see <- lits_see %>%
  mutate(ss_marry     = clean_item(q829a),
         ss_adopt     = clean_item(q829b),
         disc_sexor_r = 5 - clean_item(q827aa))

item_labels_lits <- c(
  ss_marry     = "Same-sex couples should have the right to marry",
  ss_adopt     = "Same-sex couples should have the right to adopt",
  disc_sexor_r = "Discrimination based on sexual orientation is widespread"
)

# Countries ordered by weighted support for same-sex marriage (lowest first,
# so the most supportive country is at the top of each facet)
country_order <- lits_see %>%
  group_by(country_short) %>%
  summarise(avg = weighted.mean(ss_marry, w = weight, na.rm = TRUE)) %>%
  arrange(avg) %>%
  pull(country_short) %>%
  as.character()

# Weighted country means (LiTS weight) with 95% CIs, same SE formula as ESS
means_ci <- lits_see |>
  select(country_short, weight, all_of(names(item_labels_lits))) |>
  pivot_longer(all_of(names(item_labels_lits)), names_to = "item", values_to = "value") |>
  filter(!is.na(value)) |>
  group_by(country_short, item) |>
  summarise(avg = sum(weight * value) / sum(weight),
            se  = sqrt(sum(weight^2 * (value - avg)^2)) / sum(weight),
            .groups = "drop") |>
  mutate(lo = avg - 1.96 * se,
         hi = avg + 1.96 * se,
         item = factor(item, levels = names(item_labels_lits), labels = item_labels_lits),
         country_short = factor(country_short, levels = country_order))

# -----------------------------------------------------------------------------
# 6. Figure 3 plot
# -----------------------------------------------------------------------------

col_serbia <- "#2a78d6"                            # Serbia highlight in panel b
col_other  <- "#1f1f1d"                            # other countries
col_trend  <- c("#1baf7a", "#eb6834", "#4a3aa7")   # panel a, one per ESS item
shp_trend  <- c(16, 15, 17)                        # circle, square, triangle

# Short labels for the figure; full question wording in the appendix
short_lits <- c(
  "Same-sex couples should have the right to marry"           = "Same-sex couples right to marry",
  "Same-sex couples should have the right to adopt"           = "Same-sex couples right to adopt",
  "Discrimination based on sexual orientation is widespread"  = "Sexual-orientation discrimination widespread"
)
short_ess <- c(
  "Gays and lesbians should be free to live their own life as they wish" =
    "Gays and lesbians free to live as they wish",
  "Gay and lesbian couples should have the same rights to adopt children as straight couples" =
    "Gay and lesbian couples same adoption rights as straight couples",
  "If a close family member was gay or lesbian, I would feel ashamed" =
    "Would feel ashamed of a gay or lesbian family member"
)

theme_att <- function() {
  theme_minimal(base_size = base_pt, base_family = font_family) +
    theme(
      text               = element_text(colour = ink),
      axis.text          = element_text(colour = ink_soft),
      axis.title         = element_text(colour = ink_soft, size = base_pt - 0.5),
      panel.grid.minor   = element_blank(),
      panel.grid.major   = element_line(colour = grid_col, linewidth = 0.3),
      strip.text         = element_text(face = "bold", hjust = 0, colour = ink,
                                        size = base_pt - 0.5,
                                        margin = margin(b = 3, t = 2)),
      plot.title         = element_text(face = "bold", size = base_pt + 1.5,
                                        margin = margin(b = 2)),
      plot.subtitle      = element_text(colour = ink_soft, size = base_pt - 0.5,
                                        margin = margin(b = 6)),
      plot.title.position = "plot",
      legend.position    = "none"
    )
}

# ---- Panel a: Serbia over ESS rounds ------------
dat_a <- trends |>
  mutate(item_short = factor(short_ess[as.character(item)], levels = unname(short_ess)),
         x = as.numeric(round))

# Spread end labels vertically so they never overlap. line_h is the height
# of one text line in y units; raise it if labels still touch.
spread_labels <- function(y, n_lines, line_h = 0.3, pad = 0.12) {
  o <- order(y); ys <- y[o]; nl <- n_lines[o]
  for (i in seq_along(ys)[-1]) {
    need <- (nl[i - 1] + nl[i]) / 2 * line_h + pad
    ys[i] <- max(ys[i], ys[i - 1] + need)
  }
  ys <- ys - (mean(ys) - mean(y[o]))   # keep the group centred on the data
  out <- y; out[o] <- ys; out
}
ends <- dat_a |>
  filter(x == max(x)) |>
  mutate(label   = str_wrap(as.character(item_short), 30),
         n_lines = lengths(strsplit(label, "\n")),
         label_y = spread_labels(avg, n_lines))

dodge <- position_dodge(width = 0.18)

p_a <- ggplot(dat_a, aes(x = x, y = avg, colour = item_short, shape = item_short,
                         group = item_short)) +
  # "Neither" reference line stops at the last round so it doesn't cross the labels
  annotate("segment", x = 0.8, xend = length(levels(trends$round)) + 0.1, y = 3, yend = 3,
           colour = ink_soft, linewidth = 0.3, linetype = "22") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.06, linewidth = 0.4,
                position = dodge) +
  geom_line(linewidth = 0.7, position = dodge) +
  geom_point(size = 2.2, position = dodge) +
  geom_text(data = ends, aes(x = x + 0.2, y = label_y, label = label), hjust = 0,
            family = font_family, size = (base_pt - 0.5) / .pt,
            lineheight = 0.9, show.legend = FALSE) +
  scale_colour_manual(values = col_trend) +
  scale_shape_manual(values = shp_trend) +
  scale_x_continuous(breaks = seq_along(levels(trends$round)),
                     labels = levels(trends$round),
                     expand = expansion(add = c(0.2, 0))) +
  scale_y_continuous(breaks = 1:5, limits = c(1, 5)) +
  coord_cartesian(xlim = c(0.8, length(levels(trends$round)) + 1.5), clip = "off") +
  labs(title = "a   Serbia over time (ESS Waves 9\u201311)",
       subtitle = "Mean agreement (1 = disagree strongly, 5 = agree strongly) with 95% confidence intervals. Dashed line = neither.",
       x = NULL, y = "Mean agreement") +
  theme_att() +
  theme(panel.grid.major.x = element_blank())

# ---- Panel b: SEE countries, one small panel per item ------------------------
dat_b <- means_ci |>
  mutate(item_short = factor(short_lits[as.character(item)], levels = unname(short_lits)),
         is_serbia  = country_short == "Serbia")

p_b <- ggplot(dat_b, aes(x = avg, y = country_short, colour = is_serbia)) +
  geom_linerange(aes(xmin = lo, xmax = hi), linewidth = 0.5) +
  geom_point(size = 1.9) +
  scale_colour_manual(values = c(`TRUE` = col_serbia, `FALSE` = col_other)) +
  scale_x_continuous(limits = c(1, 4), breaks = 1:4, expand = expansion(add = 0.08)) +
  facet_wrap(~ item_short, ncol = 3,
             labeller = labeller(item_short = label_wrap_gen(28))) +
  labs(title = "b   South-East Europe (Life in Transition Survey IV, 2022\u201323)",
       subtitle = "Weighted country means (1\u20134) with 95% confidence intervals. Serbia in blue.",
       x = "Mean (1 = strongly disagree / very rare, 4 = strongly agree / very widespread)",
       y = NULL) +
  theme_att() +
  theme(panel.grid.major.y = element_blank(),
        panel.spacing.x    = unit(22, "pt"),
        axis.text.y        = element_text(colour = ink))

# ---- Combine and export -----------------------------------------------------
fig3 <- p_a / p_b +
  plot_layout(heights = c(0.85, 1.15)) +
  plot_annotation(theme = theme(plot.margin = margin(4, 4, 4, 4)))

save_fig(fig3, "figure3_attitudes", height = 14)

# Numbers behind Figure 3
print(trends |> select(round, item, avg, lo, hi, n), width = Inf)
print(means_ci, n = Inf, width = Inf)
