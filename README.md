PCPopTree is a software contains tools to reconstruct hierarchical population structure tree from individual files extracted from PCHCluster, and the individual files are named with suffix '.ind'.

To use this software, we recommend you to use the bash script "gen_tree.sh", which can easily manipulate lots of files simutaneously.

The usage of gen_tree is pretty easy:

in Unix/Linux platform, just type the following command:

./gen_tree.sh ind_dir bin_file out_file

where ind_dir contains all the ".ind" files encoded with binary number,and bin_file is actually bin/PCPopTree which you locate in your personaly directory.


The out_file contains information of reconstructed tree with Newick format,and can be easily read by several Graphic programs, e.g Philip, R(ape)

In addition, another script cluster_plot_color.r can implement the visualization of the reconstructed population tree, and it can provide either the topology or the proportion information for each hierarchical cluster.

The output images can be categrized into two groups, overlap plot or opposite plot, which can just specify the 4th arguments of the script, the default option is overlap, and you can set the 4th arguments as "opposite" to plot the opposite plot

the overall command is:

Rscript cluster_plot_color.r ind_dir figure_name layer_num which >Rlog 2>&1

the 1st argument is the same as gen_tree.sh above, and the 2nd argument is the name of output figure, the third argument specify the resolution of the color image, the lager the thrid argument, the higher of resolution for the image in the figure.
