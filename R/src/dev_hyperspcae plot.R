# test Reaver on privots for frist look on equal plots
# source Function
source(file.path(envrmt$path_REAVER_hyperspace,"dev_plot_hyperspace.r"))

# for interpretation of nmds stress level:
# over 0.2 coution, over 0.3 highly suspect

# Explore Hyperspace with cover values
Reaver_plot_hyperspace(SL_p,6)
Reaver_plot_hyperspace(DW_p,4)
Reaver_plot_hyperspace(EP_p,4)
Reaver_plot_hyperspace(MP_p,4)
Reaver_plot_hyperspace(SU_p,3)

# Explore Hyperspace with only appeariance
Reaver_plot_hyperspace(SL_c,6)
Reaver_plot_hyperspace(DW_c,4)
Reaver_plot_hyperspace(EP_c,4)
Reaver_plot_hyperspace(MP_c,5)
Reaver_plot_hyperspace(SU_c,3)
dev.off()