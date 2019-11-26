
# Overview

The capriR package was developed for the Common Agricultural Policy Regionalized Impacts (CAPRI) modelling system, a large-scale economic model with a particular focus on agriculture and food markets. capriR is a tool for processing, visualizing and analysing both the databases and simulation results of CAPRI. 


## Installation from source

Download or build the source package and then execute:


``` r

install.packages("caprir_0.1.0.tar.gz",repos=NULL,type="source")

```

# Background

Large scale economic models naturally produce large datasets of simulated results. Analysing large amount of data is often beyond the capacity of the human mind, at least without the help of specific software tools designed for filtering, transforming and visually presenting the datasets. Many economic models also offer visual aid and easy data access possibilities for their users via graphical user interfaces (GUI). Most GUIs still require the user to do numerous and time consuming interactions with the software, slowing down the analysis of simulation results, and sometimes even hindering model users to find the relevant drivers and other causality chains in model results. The R programming language provides complementary data exploitation possibilities with its command line interface (CUI) and with a large number of optional packages for analysing and visualizing large datasets. As simulation exercises and the related reporting and data analysis need to be repeated several times during the lifetime of a typical research project, a clear advantage of user-created R scripts emerges: scripts can be executed repeatedly with a minimum effort for interaction with the software interface, each time simulation results have been updated. Data visualizations, statistical and econometric analyses based on of third-party R packages can be easily replicated and repeated in this manner.


The capriR package was developed for the Common Agricultural Policy Regionalized Impacts (CAPRI) modelling system, a large-scale economic model with a particular focus on agriculture and food markets. capriR includes functions for processing, visualizing and analysing both the model databases and simulation results. capriR has been designed to complement (rather than to replace) already existing GUIs for CAPRI. Those GUIs will probably still remain the preferred options for quick analysis of model results and for steering CAPRI from database preparation until simulation runs in the future. The relative advantages of capriR compared to graphical User Interface options include 

- dissemination of model-databases and simulation results; 
- automated reporting requiring additional (post-model) calculations; 
- creating publication-quality maps and other data visualizations. 

As CAPRI covers EU agricultural production activities with fine geographical detail, spatial data analysis and visualization are of a particular importance for capriR. CAPRI results are linked to commonly used spatial data packages thus enabling the user to create high-resolution static or interactive maps. With capriR you can also access rapidly the databases and simulation results of the CAPRI modelling system. The modularity of the R programming language allows for directly applying advanced econometric and statistical techniques from other (open-source) R packages on the data sets retrieved from CAPRI.


Although capriR is only directly useful for the relatively small user base of CAPRI, some of the general strategies for rapid package development might be re-used for similar, large scale economic models. What makes the capriR approach potentially interesting for a wider economic modelling community is that model-specific R packages can be built with limited efforts, and under limited time. Such R packages can be developed for dissemination of results, visualization and complex post-model data analysis. Model linkages (interlinked quantitative models) can benefit a lot from model-specific rapid package development. The need for exchanging large amounts of data (model inputs and outputs) between different model architectures poses a practical challenge to modelling groups. Model-specific R packages for data exchange offer a common software platform. The large user-base of the R programming language in the broader scientific community makes such packages efficient in disseminating model results for a general scientific audience, increasing at the same time the transparency of modelling exercises. Opening the black box of complex large-scale models by making databases and results easily accessible can lead to huge gains in credibility for the modelling community, and is also an essential part in evidence based policy making.



