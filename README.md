Projet MTH2302D - Configuration VS Code pour R
Ce guide explique comment configurer VS Code pour travailler avec R sur ce projet, afin de reproduire l'environnement de développement utilisé.

1. Prérequis
Avant de commencer, assurez-vous d'avoir installé :

R (version ≥ 4.0.0):  https://cran.r-project.org/

RStudio (optionnel, pour la gestion des packages) : https://posit.co/download/rstudio-desktop/

Visual Studio Code

2. Configuration de Visual Studio Code pour R
Extensions nécessaires
Installez ces extensions dans VS Code (Ctrl+Shift+X ou Extensions dans la barre latérale) :

R (support de base pour R): https://marketplace.visualstudio.com/items?itemName=REditorSupport.r

R Debugger (pour le débogage): https://marketplace.visualstudio.com/items?itemName=RDebugger.r-debugger


3. Cloner et configurer le projet
Cloner le dépôt
bash
Copy
git clone https://github.com/saka439/Devoir_mth2302d.git
cd Devoir_mth2302d

Installer les packages R nécessaires
Ouvrez un terminal R dans VS Code (`Ctrl+Shift+``) et exécutez :
install.packages("languageserver")

4. Pour tester le code on doit le faire ligne par ligne en utilisant les commandes : 
        """Ctrl et Enter simultanément"""
