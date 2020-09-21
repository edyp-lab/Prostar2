
dreamRs <- create_theme(
  theme = "default",
  bs_vars_global(
    grid_columns = 30
  ),
  bs_vars_navbar(
    default_bg = "#34949A",
    default_color = "#FFFFFF",
    default_link_color = "#FFFFFF",
    default_link_active_color = "#FFFFFF",
    default_link_hover_color = "#A4A4A4"
  ),
  bs_vars_button(
    font_weight = 500,
    border_radius_base = 0,
    default_color = "#112446",
    default_border = "#112446",
    primary_color = "#FFF",
    primary_bg = "#112446",
    primary_border = "#112446"
  ),
  bs_vars_color(
    brand_primary = "#112446",
    brand_success = "#7bc043",
    brand_info = "#0392cf",
    brand_warning = "#f37736",
    brand_danger = "#ee4035"
  ),
  bs_vars_state(
    success_text = "#FFF",
    success_bg = "#c9d175",
    success_border = "#c9d175",
    info_text = "#FFF",
    info_bg = "#3f2d54",
    info_border = "#3f2d54",
    danger_text = "#FFF",
    danger_bg = "#d175b8",
    danger_border = "#d175b8"
  ),
  bs_vars_wells(
    bg = "#FFF",
    border = "#3f2d54"
  ),
  bs_vars_nav(link_padding = "5px 25px"),
  bs_vars_tabs(
    border_color = "#112446",
    active_link_hover_bg = "#FFF",
    active_link_hover_color = "#112446",
    active_link_hover_border_color = "#112446",
    link_hover_border_color = "#112446"
  )
)