# Lake Tanganyika
This is the code used to generate analyses and figures from my paper on Lake Tanganyika metagenome-assembled genomes.

:newspaper: Our paper is now published! Here is the citation:

**Tran, P.Q.**, Bachand, S.C., McIntyre, P.B. et al. Depth-discrete metagenomics reveals the roles of microbes in biogeochemical cycling in the tropical freshwater Lake Tanganyika. ISME J 15, 1971–1986 (2021). https://doi.org/10.1038/s41396-021-00898-x

 :email: _Contact information_: Patricia Tran: ptran5 [at] wisc [dot] edu

Please feel free to email me!

# Repository Notes
- Feb 16, 2021: Our BioProject is now publicly available at NCBI GenBank, I have updated the information in the "Genomes" section below.

- Feb 9, 2021: Our paper is now live. The BioProject will be released on NCBI on 02-18. Until then, all the MAGs can be found on the OSF server, linked below.

- November 18, 2020: I have updated the manuscript and files on BioRxiv, and submitted my rounds of revisions.

- October/November 2020: I am currently going through a round of revisions.

- July 2020: Manuscript has been revised and resubmitted. 

- January 20, 2020: I am currently editing figures and content as per reviewer's comments. The contents of this directory might change.

# Genomes
Our genomes are available and citable at NCBI GenBank at using the **BioProject PRJNA523022** https://www.ncbi.nlm.nih.gov/bioproject/PRJNA523022
If you use these genomes in your analyses, please cite our ISME J paper above. Thank you!

**In this project, we resolved 523 medium- to high-quality metagenome-assembled genomes (MAGs), belonging to both Bacteria and Archaea.**

The full [NCBI Genome Accession numbers](https://www.ncbi.nlm.nih.gov/genome/) are in Supplementary Table 2, and also [here](https://github.com/patriciatran/LakeTanganyika/blob/master/NCBI_Accessions_LakeTanganyika_MAGs.txt).

Some highlights
- Two comammox genome (belonging to clade II-A)
- A full circular and complete genome of Sacharibacteria (a.k.a candidate division TM7)
- A clade of non-photosynthetic Sericytochromatia belonging to freshwater lakes
- Several Candidate Phyla Radiation organisms from a freshwater lake
- Several Archaeal (including DPANN) genomes from a freshwater lake

...Among others. I invite you to read the paper and explore the data associated with the paper!

## Metagenomes on JGI (IMG/M):
The 24 raw, assembled and annotated metagenomes are available on the Integrated Microbial Genomes & Microbiomes (IMG/M) portal using the following IMG Genome ID’s: 3300020220, 3300020083, 3300020183, 3300020200, 3300021376, 3300021093, 3300021091, 3300020109, 3300020074, 3300021092, 3300021424, 3300020179, 3300020193, 3300020204, 3300020221, 3300020196, 3300020190, 3300020197, 3300020222, 3300020214, 3300020084, 3300020198, 3300020603, 3300020578. 

# Github Folder Organization

## HMM
This folder contains the 16 ribosomal proteins (Bacteria and Archaea) and the custom metabolic HMMs. 

### HMM > rp16_HMM
This is the folder containing the HMM for the 16 ribosomal proteins that were used to search against MAGs and reference genomes. The result were used to create the concatenated gene phylogeny. the naming `_bact` or `_arch` signifies whether this HMM is built for Archaea or Bacteria. [Hmmsearch](http://hmmer.org/download.html) with the `--cut_tc` was used. 

### HMM > Metabolic_hmms
This contains the HMMs for the metabolic genes. Use `hmmsearch` with the `--cut_tc` option to search them against your datasets.


## Script: Combining-JGI-data.R

We annotated the genomes using custome HMM (METABOLIC : https://github.com/AnantharamanLab/METABOLIC), and manual curation of certain genes such as nxrA and amoA (more details are provided in the paper). As an extra layer of evidence, because the 24 metagenomes were sequenced as part of a JGI CSP (Gold Study ID: Gs0129147 "Freshwater microbial communities from Lake Tanganyika, Tanzania"), and processed through the IMG/M annotation pipeline. You can download all the IMG/M annotation using the JGI Globus (https://genome.jgi.doe.gov/portal/help/download.jsf#/globus). This script that I made combines all IMG/M tables into a single table and summary figures for each folder (each folder = IMG results for 1 metagenome). Because these are large raw files, you can download the proceesed tabled here: https://osf.io/pmhae/ 

## Figures

### Figures > Other
This folder contains the scripts to generate additional figures (e.g. additional analyses that didn't make it to the paper, or versions of previous figures, data exploration).

### Figures > Paper
This folder contains the scripts to generate the main and supplementary figures of the manuscript.

|     Directory Name    | Figure                                          |
|-----------------------|-------------------------------------------------|
| Abundance-Plot        | Main Figure 4                                   |
| Cazyme                | Supplementary Figure 13                         |
| Compare-Baikal        | Main Figure 5 and Supplementary Figure 8        |
| Comparison-Taxonomy   | Supplementary Figure 6                          |
| Environmentaldata     | Main Figure 1 and Supplementary Figures 2,3,4,5 |
| MAG_Characteristics   |                                                 |
| METABOLIC-results     |                                                 |
| Metabolism-Summary    | Supplementary Figures 9,10,11,12                |
| Photosystems          |                                                 |
| Samples               | Supplementary Figure 1                          |
