# combined_plot
library(patchwork)

source("elections/house_age_in_office.r")
source("elections/senators_age_in_office.r")


p1 <-
    mem_plot +
    labs(title = "House of Representatives")

p2 <-
    sen_plot +
    labs(title = "Senate") +
        geom_vline(
        xintercept = 39, color = "gray60",
        alpha = .1, linewidth = 3
    ) +
    theme(
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank()
    )

p_combined <-
    p1 + p2 +
    plot_annotation(
        caption =
            glue::glue(
                "Age of Congressmen and women from their assumption ",
                "until today.\nAs of ",
                format(today(), format = "%B %d, %Y")
            ),
        title = "Are you older than a Congressperson?"
    ) +
    theme(
        plot.caption = element_text(hjust = 0),
        plot.title = element_text(hjust = .5),
        plot.caption.position = "plot"
    )

ggsave("elections/both.png",
    width = 12, height = 12,
    plot = p_combined
)
