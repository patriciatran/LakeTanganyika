# Lake Tanganyika
This is the code used to generate analyses and figures from my preprint on Lake Tanganyika genomes.

Thank you for citing our pre-print:

Patricia Q Tran, Peter B McIntyre, Benjamin M Kraemer, Yvonne Vadeboncoeur, Ismael A Kimirei, Rashid Tamatamah, Katherine D McMahon, Karthik Anantharaman. "Depth-discrete eco-genomics of Lake Tanganyika reveals roles of diverse microbes, including candidate phyla, in tropical freshwater nutrient cycling" bioRxiv 834861; doi: https://doi.org/10.1101/834861

# Genomes
I've uploaded the metagenome-assembled-genomes (MAGs) on the Open Science Framework for viewing and downloading while the preprint is up, and during the peer-review process.
However, the genomes have been uploaded to NCBI Genbank and will be officially released with Accessed ID on the real publication date.

Link: https://osf.io/pmhae/

# Note
January 20, 2020: I am currently editing figures and content as per reviewer's comments. the contents of this directory might change.

October/November 2020: I am currently going through a round of revisions.

# Github Folder Organization

## rp16_HMM

This is the folder containing the HMM for the 16 ribosomal proteins that were used to search against MAGs and reference genomes. The result were used to create the concatenated gene phylogeny. the naming "_bact" or "_arch" signifies whether this HMM is built for Archaea or Bacteria. Hmmsearch with the --cut_tc was used. 

## Script: Combining-JGI-data.R

We annotated the genomes using custome HMM (METABOLIC : https://github.com/AnantharamanLab/METABOLIC), and manual curation of certain genes such as nxrA and amoA (more details are provided in the paper). As an extra layer of evidence, because the 24 metagenomes were sequenced as part of a JGI CSP (Gold Study ID: Gs0129147 "Freshwater microbial communities from Lake Tanganyika, Tanzania"), and processed through the IMG/M annotation pipeline. You can download all the IMG/M annotation using the JGI Globus (https://genome.jgi.doe.gov/portal/help/download.jsf#/globus). This script that I made combines all IMG/M tables into a single table and summary figures for each folder (each folder = IMG results for 1 metagenome). 

## Figures

### Figures > Other
This folder contains the scripts to generate additional figures (e.g. additional analyses that didn't make it to the paper, or versions of previous figures, data exploration).

### Figures > Paper
This folder contains the scripts to generate the main and supplementary figures of the manuscript.

|     Directory Name    | Figure                                          |
|-----------------------|-------------------------------------------------|
| Abundance-Plot        | Main Figure 3 and Supplementary Figure 7        |
| Cazyme                | Supplementary Figure 9                          |
| Compare-Baikal        | Main Figure 5 and Supplementary Figure 8        |
| Comparison-Taxonomy   | Supplementary Figure 6                          |
| Environmentaldata     | Main Figure 1 and Supplementary Figures 2,3,4,5 |
| MAG_Characteristics   |                                                 |
| METABOLIC-results     |                                                 |
| Metabolism-Summary    |                                                 |
| Samples               | Supplementary Figure 1                          |
