# Identification of tissue specific dependencies between cancer driver gene mutations and their interactors abundances

- AUTHOR: [Márcia F. S. Vital](https://github.com/MarciaFSVital)<sup>1</sup>
- SUPERVISOR: [Francisco R. Pinto](https://github.com/frpinto)<sup>1</sup>
- CONTACT: [mfvital@ciencias.ulisboa.pt](mfvital@ciencias.ulisboa.pt)

<sup>1</sup> [RNA Systems Biology Lab](https://github.com/GamaPintoLab)

Code to reproduce methods & results from my Master's Thesis Project. I will add a link to the final document as soon as it is officially published.

To reproduce this project just clone or download this repository and create a Python virtual environment. To learn how to create a virtual environment refer to [creating a virtual environment](https://packaging.python.org/en/latest/guides/installing-using-pip-and-virtual-environments/#creating-a-virtual-environment).

If you use our data or analysis in your research, please cite us!

## System Requirements

Our code was run using the following software:
-	Python version 3.10.14
-	R version 4.3.1

**Python packages to install:**

See requirements.txt

To install all packages using this file just type

```
pip install requirements.txt
```

inside your python virtual environment.

**R packages to install:**

Required packages can be installed using script 5.

## Analysis notebooks:

The notebooks contain all the necessary data and code to allow others to reproduce the results presented in this thesis and to recreate all associated figures. To ensure consistent results, run the notebooks in the following order:

- *get_PPIs.ipynb*: this Python notebook covers Results and Discussion section 3.1. 
- *preprocessing.ipynb*: this Python notebook covers Results and Discussion section 3.1. 
- *driver_neighbours_analysis.ipynb*: this Python notebook covers Results and Discussion section 3.2 and subsection 3.2.1. 
- *DINTs_analysis.ipynb*: this Python notebook covers …
- *fishers_enrichment.R*: this R script covers …
- *machine_learning.ipynb*: this Python notebook covers Results and Discussion section 3.5 and subsections 3.5.1 - 3.5.2. 
