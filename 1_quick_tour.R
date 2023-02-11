# .........................................................
# A quick Momocs tour
# presented for the Workshop on good statistical practice in archaeology
# organized by ARCAS, Feb 2023
#
#
# I'm available for collaboration and consulting
#  <bonhomme.vincent@gmail.com>
# .........................................................
# dependencies
library(tidyverse)
library(Momocs)

# load data
load("pips.rda")

# Family picture -----
pips %>% panel()
pips %>% filter(view=="VD") %>% panel(fac=~status)

# let's retain only the pips on lateral view
VL <- pips %>% filter(view=="VL")
VL %>% stack()

# Geometrical tools ----------------
## for standardizing geometries ----
VL %>% coo_center() %>% stack() # centering
VL %>% coo_center() %>% coo_align() %>% stack() # + aligning
VL %>% coo_center() %>% coo_align() %>% coo_scale() %>% stack() # + scaling
VL %>% coo_center() %>% coo_align() %>% coo_scale() %>%
  coo_slidedirection("right") %>% stack() # + 1st point

# measurement tools ----------------
VL %>% coo_length() %>% tibble(length=.) %>% bind_cols(VL$fac) %>%
  ggplot() +
  aes(length, fill=status) +
  geom_density()

# Morphometrics -----
# morphometrics now
vitis_pipe <- function(x) {
  # first get VL length
  L <- x %>% filter(view=="VL") %>% coo_align() %>% coo_center() %>% coo_length()

  # now morphometrics
  x %>%
    # the pipe used in Scientific Reports (2021)
    coo_scale() %>% coo_align %>% coo_center() %>%
    coo_slidedirection("right") %>%
    coo_sample(360) %>%
    def_ldk_direction("left") %>%
    def_ldk_direction("right") %>%
    coo_slide(ldk=2) %>%
    coo_bookstein() %>%
    chop(~view) %>%
    efourier(5, norm=FALSE) %>%
    combine() %>%
    select(-view) %>%
    # finally add L
    mutate(L=L)
}

# apply it on our Out
pips_eft <- pips %>% vitis_pipe()

# we now have an OutCoe(fficients)
pips_eft

# Analyses -----
# you can escape Momocs whenever you want:
pips_eft %>% as_df()

# or you can continue with Momocs that has helpers for
# the most commont stat tools, eg
## PCA -----
pips_pca <- pips_eft %>% PCA()
pips_pca %>% plot_PCA(~status)
pips_pca %>% plot_PCA(~status+accession)

# again, you can escape Momocs for a while and use ggplot2
pips_pca %>% as_df(retain=2) %>%
  ggplot() +
  aes(PC1, PC2, col=status) +
  geom_point(aes(size=L), alpha=0.8) +
  scale_size_continuous(range=(c(0.2, 4))) +
  geom_density_2d(aes(PC1, PC2, col=status)) +
  coord_equal() +
  labs(title="Length and shape ~ domestication status") + theme_minimal()

pips_pca %>% as_df(retain=2) %>%
  ggplot() +
  aes(x=PC1, y=L, col=status) +
  geom_point() +
  # scale_color_viridis_c() +
  xlab("Shape (PC1)") + ylab("pip length (pixels)") +
  geom_smooth(method="lm", formula="y~x") +
  labs(title="Length and shape ~ domestication status") + theme_minimal()

# visualize Principal component contributions
pips_pca %>% PCcontrib(nax=1:2)

## LDA -------
# Linear discriminant analyses
pips_eft %>% PCA %>% plot_PCA(~status)
pips_eft %>% LDA(~status)

# LDA at the accession level
pips_eft %>% LDA(~accession)
pips_eft %>% LDA(~accession) %>% plot_LDA(morphospace = T)

## Mean shapes -----
# visualize MeanShapes
pips_eft %>% dissolve(1) %>% MSHAPES(~accession) %>% plot_MSHAPES()

## Thin Plate Splines ----
ms <- pips_eft %>% dissolve(1) %>% MSHAPES(~status)
tps_iso(ms$shp$w, ms$shp$c)




