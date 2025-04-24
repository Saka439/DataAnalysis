# DataAnalysis - MTH2302D

## 📘 Contexte

Ce projet est une étude de cas portant sur l’analyse de données issues d’une expérience visant l’amélioration du procédé d’assemblage par rivetage de panneaux d’avion.

### Objectif de l’étude
L’expérience porte sur le perçage de trous dans les panneaux du fuselage d’un avion, un aspect crucial pour la solidité de l’assemblage. Le but est :

1. D’analyser et d’évaluer l’effet de différents facteurs sur la qualité du perçage ;
2. De construire un modèle mathématique reliant ces facteurs à l’indice de qualité ;
3. De déterminer, si possible, des valeurs optimales de ces facteurs pour un perçage optimal.

### Description des données

L’expérience a été réalisée sur des coupons métalliques (échantillons), avec 210 observations. Quatre variables sont étudiées :

| Colonne | Nom (Symbole) | Description |
|--------|----------------|-------------|
| 1      | Matériau (M)   | Type de matériau utilisé (0 ou 1) |
| 2      | Vitesse (V)    | Vitesse de rotation de la perceuse (en milliers de tours/minute) |
| 3      | Température (T)| Température de l’air injecté (en °F) |
| 4      | Indice (IR)    | Indice de rugosité des trous (qualité du perçage) |

## ⚙️ Configuration de l’environnement de développement (VS Code pour R)

### 1. Prérequis

Assurez-vous d'avoir installé :

- **R** (version ≥ 4.0.0) : [https://cran.r-project.org/](https://cran.r-project.org/)
- **RStudio** (facultatif, pour gérer les packages) : [https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/)
- **Visual Studio Code** : [https://code.visualstudio.com/](https://code.visualstudio.com/)

### 2. Extensions à installer dans VS Code

Ouvrez la section Extensions (`Ctrl+Shift+X`) et installez :

- [R](https://marketplace.visualstudio.com/items?itemName=REditorSupport.r) : support de base pour R
- [R Debugger](https://marketplace.visualstudio.com/items?itemName=RDebugger.r-debugger) : pour le débogage

### 3. Cloner et configurer le projet

```bash
git clone https://github.com/saka439/Devoir_mth2302d.git
cd Devoir_mth2302d
