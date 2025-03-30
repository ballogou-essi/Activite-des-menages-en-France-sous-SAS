/*     Nom des membres du groupe
    Nom1 : BALLOGOU Essi Carole Claudia
       Année: 2024-2025
*/

/************************************************************************** PARTIE 1 **********************************************************************/

/* Définition de la bibliothèque "MYDATA" (chemin à adapter) */
LIBNAME MYDATA "/home/u64135306/sasuser.v94/Cours Variables Qualitatives/Exercice 1"; /*MYDATA, est un
raccourci pour aller vers le dossier Exercice1, en plus MYDATA contiendra toutes mes tables que j'aurai à créer*/

/*1. Importation des données CSV en table SAS*/
PROC IMPORT DATAFILE="/home/u64135306/sasuser.v94/Cours Variables Qualitatives/Exercice 1/Panel95light.csv"
    OUT=MYDATA.panel95_table 
    DBMS=CSV REPLACE; GETNAMES=YES; GUESSINGROWS=MAX; 
RUN;

PROC CONTENTS DATA=MYDATA.panel95_table;
RUN;

/*2. Distributions des variables */
/*Distribution du Log-Salaire (lw)*/
PROC UNIVARIATE DATA=MYDATA.panel95_table; 
    VAR lw; HISTOGRAM lw / NORMAL KERNEL; PROBPLOT lw / NORMAL(MU=EST SIGMA=EST);
    TITLE "Distribution du Log-Salaire (lw)";
RUN;

/*Distribution de l'expérience en Année*/
DATA mydata.panel95_table;
    SET mydata.panel95_table;
    exper_years = exper / 12;  
RUN;
PROC UNIVARIATE DATA=mydata.panel95_table;
    VAR exper_years; HISTOGRAM exper_years / NORMAL KERNEL;
    TITLE "Distribution de l'expérience en Année";
RUN;

/*Distribution des niveaux d'étude*/
ODS GRAPHICS ON; 
PROC SGPLOT DATA=MYDATA.panel95_table;
    VBAR etudes / DATALABEL FILLATTRS=(COLOR=STEELBLUE) 
                  OUTLINEATTRS=(COLOR=BLACK THICKNESS=1.5); STYLEATTRS WALLCOLOR=LIGHTGRAY; 
    XAXIS LABEL="Niveau d'étude" DISPLAY=(NOLABEL); YAXIS LABEL="Effectif" GRID; 
    TITLE "Distribution des niveaux d'étude";
RUN;
ODS GRAPHICS OFF;

/*Distribution des observations par mois*/
PROC SGPLOT DATA=MYDATA.panel95_table;
    VBAR mois / FILLATTRS=(COLOR=skyblue); XAXIS LABEL="Mois"; YAXIS LABEL="Fréquence";
    TITLE "Distribution des observations par mois";
RUN;

/*2.5 Distribution de sexe */
PROC FREQ DATA=MYDATA.panel95_table; TABLES sexe; 
RUN;

GOPTIONS RESET=ALL CTEXT=BLACK; 
PATTERN1 VALUE=SOLID COLOR=PINK; PATTERN2 VALUE=SOLID COLOR=STEELBLUE;

PROC GCHART DATA=MYDATA.panel95_table;
    PIE sexe / VALUE=INSIDE PERCENT=OUTSIDE COUTLINE=BLACK NOLEGEND;
    TITLE "Répartition des sexes"; 
RUN; QUIT;

/*3.*/
DATA MYDATA.panel95_table;
   SET MYDATA.panel95_table;
       /* Création des indicatrices pour le niveau d'études */
    Primaire       = (etudes = "primaire");
    Secondaire     = (etudes = "secondaire");
    Cycle2 = (etudes = "deuxieme cycle");
    Cycle3 = (etudes = "troisieme cycle");
    Professionnel_court  = (etudes = "professionnel court");
    Professionnel_long = (etudes = "professionnel long");
      /* Création de la variable numérique sexeN */
    IF sexe = "Homme" THEN sexeN = 1;
    ELSE IF sexe = "Femme" THEN sexeN = 2;
      /* Création des indicatrices de sexe */
    Homme = (sexeN = 1);
    Femme = (sexeN = 2);
RUN;


/* 4. Calcul du log-salaire moyen par niveau d'étude et dans l'échantillon */
PROC MEANS DATA=MYDATA.panel95_table; CLASS etudes; VAR lw; RUN;  /*Calcul des statistiques du log-salaire par niveau d'étude*/
PROC MEANS DATA=MYDATA.panel95_table ; VAR lw; RUN; /*Calcul du log-salaire moyen*/

/*5. Etimation de différents modèles de regression du log-salaraire*/

/* Modèle (a) : Tous les niveaux d’étude, "sans précaution" */
PROC REG DATA=MYDATA.panel95_table;
    MODEL lw = primaire secondaire professionnel_court professionnel_long cycle2 cycle3;
    OUTPUT OUT=MYDATA.panel95_table PREDICTED=lw_pred_A;
    ODS OUTPUT ParameterEstimates=Coef_A FitStatistics=Stats_A;
    TITLE "Régression du log-salaire sur tous les niveaux d'études (sans précaution)";
RUN;

/* Modèle (b) : Tous les niveaux d’étude, sans constante */
PROC REG DATA=MYDATA.panel95_table;
    MODEL lw = primaire secondaire professionnel_court professionnel_long cycle2 cycle3 / NOINT;
    OUTPUT OUT=MYDATA.panel95_table PREDICTED=lw_pred_B;
    ODS OUTPUT ParameterEstimates=Coef_B FitStatistics=Stats_B;
    TITLE "Régression du log-salaire sur tous les niveaux d'études (sans constante)";
RUN;

/* Modèle (c) : Tous les niveaux d’étude sauf "primaire" (référence) */
PROC REG DATA=MYDATA.panel95_table;
    MODEL lw = secondaire professionnel_court professionnel_long cycle2 cycle3;
    OUTPUT OUT=MYDATA.panel95_table PREDICTED=lw_pred_C;
    ODS OUTPUT ParameterEstimates=Coef_C FitStatistics=Stats_C;
    TITLE "Régression du log-salaire en prenant 'Primaire' comme référence";
RUN;

/* Modèle (d) : Tous les niveaux d’étude sauf "cycle3" (référence) */
PROC REG DATA=MYDATA.panel95_table;
    MODEL lw = primaire secondaire professionnel_court professionnel_long cycle2;
    OUTPUT OUT=MYDATA.panel95_table PREDICTED=lw_pred_D;
    ODS OUTPUT ParameterEstimates=Coef_D FitStatistics=Stats_D;
    TITLE "Régression du log-salaire en prenant 'cycle3' comme référence";
RUN;

/* Modèle (e) : Tous les niveaux d'étude avec contrainte en imposant la nullité de la moyenne des coefficients pondérée par les effectifs */
PROC REG DATA=MYDATA.panel95_table;
    MODEL lw = primaire secondaire professionnel_court professionnel_long cycle2 cycle3;
    RESTRICT 924 * primaire +
            1308 * secondaire +
            2664  * professionnel_court +
             732 * professionnel_long +
             2292 * cycle2 +
             936* cycle3 = 0;
    OUTPUT OUT=MYDATA.panel95_table PREDICTED=lw_pred_E;
    ODS OUTPUT ParameterEstimates=Coef_E FitStatistics=Stats_E;
    TITLE "Régression avec contrainte sur la moyenne des coefficients";
RUN;

/* Modèle (f) : Tous les niveaux d’études avec contrainte sur la moyenne NON pondérée */
PROC REG DATA=MYDATA.panel95_table;
    MODEL lw = primaire secondaire professionnel_court professionnel_long cycle2 cycle3;
    RESTRICT primaire + secondaire + professionnel_court + professionnel_long + cycle2 + cycle3 = 0;
    OUTPUT OUT=MYDATA.panel95_table PREDICTED=lw_pred_F;
    ODS OUTPUT ParameterEstimates=Coef_F FitStatistics=Stats_F;
    TITLE "Régression avec contrainte sur la moyenne NON pondérée des coefficients";
RUN;


/* 6. Ajout et Calcul de la corrélation entre les log-salaires observés et prédits */
PROC CORR DATA=MYDATA.panel95_table;
    VAR lw lw_pred_A lw_pred_B lw_pred_C lw_pred_D lw_pred_E lw_pred_F;
    TITLE "Corrélation entre le log-salaire observé et les log-salaires prédits";
RUN;


/* 7. ############## Contruction du TABLEAU DE SYNTHESE ###############*/
/* Création d'une table contenant tous les coefficients */
DATA MYDATA.Coef_All;
    LENGTH Model $150;
    SET Coef_A (IN=A) Coef_B (IN=B) Coef_C (IN=C) Coef_D (IN=D) Coef_E (IN=E) Coef_F (IN=F);
    IF A THEN Model = "A: Tous les niveaux d’étude, 'sans précaution'";
    ELSE IF B THEN Model = "B: Tous les niveaux d’étude, sans constante";
    ELSE IF C THEN Model = "C: Tous les niveaux d’étude sauf 'primaire'";
    ELSE IF D THEN Model = "D: Tous les niveaux d’étude sauf 'cycle3'";
    ELSE IF E THEN Model = "E: Nullité de la moyenne des coefs pondérée par les effectifs";
    ELSE IF F THEN Model = "F: Nullité de la la moyenne NON pondérée";
RUN;
PROC TRANSPOSE DATA=MYDATA.Coef_All OUT=Coef_T; BY Model; ID Variable; VAR Estimate; RUN;


/* Création d'une table contenant les statistiques de régression */
DATA MYDATA.Stats_All;
    LENGTH Model $150;
    SET Stats_A (IN=A) Stats_B (IN=B) Stats_C (IN=C) Stats_D (IN=D) Stats_E (IN=E) Stats_F (IN=F);
    IF A THEN Model = "A: Tous les niveaux d’étude, 'sans précaution'";
    ELSE IF B THEN Model = "B: Tous les niveaux d’étude, sans constante";
    ELSE IF C THEN Model = "C: Tous les niveaux d’étude sauf 'primaire'";
    ELSE IF D THEN Model = "D: Tous les niveaux d’étude sauf 'cycle3'";
    ELSE IF E THEN Model = "E: Nullité de la moyenne des coefs pondérée par les effectifs";
    ELSE IF F THEN Model = "F: Nullité de la la moyenne NON pondérée";
RUN;
PROC TRANSPOSE DATA=MYDATA.Stats_All OUT=Stats_T1; BY Model; ID Label1; VAR NValue1; RUN;
PROC TRANSPOSE DATA=MYDATA.Stats_All OUT=Stats_T2; BY Model; ID Label2; VAR NValue1; RUN;
DATA MYDATA.Stats_Final; MERGE Stats_T1 Stats_T2; BY Model; RUN;

/* Jointure des statistiques et des coefficients transposés */
PROC SQL;
    CREATE TABLE MYDATA.Synthese_Regression AS
    SELECT S.Model, S."Root MSE"n AS Root_MSE, S."Dependent Mean"n AS Dep_Mean, S."R-Square"n AS R2, S."Adj R-Sq"n AS Adj_R2,
           C.Intercept, C.Primaire, C.Secondaire, C.Professionnel_court, C.Professionnel_long, C.Cycle2, C.Cycle3
    FROM MYDATA.Stats_Final AS S 
    LEFT JOIN Coef_T AS C ON S.Model = C.Model;
QUIT;

PROC PRINT DATA=MYDATA.Synthese_Regression LABEL NOOBS; TITLE "Synthèse des résultats des régressions"; RUN;

/* ************************************************************************************ PARTIE 2 ***************************************************************************/
/* (Chemin à adapter) */
LIBNAME MYDATA2 "/home/u64135306/sasuser.v94/Cours Variables Qualitatives/Exercice2";
LIBNAME MYDATA "/home/u64135306/sasuser.v94/Cours Variables Qualitatives/Exercice 1";

PROC IMPORT DATAFILE="/home/u64135306/sasuser.v94/Cours Variables Qualitatives/Exercice2/ColSelected.csv"
    OUT=MYDATA2.Colselected
    DBMS=CSV
    REPLACE;
    GETNAMES=YES;
    GUESSINGROWS=MAX; 
RUN;


/* QUESTION 1: Fusion */
PROC SORT DATA=MYDATA2.Colselected; BY mident mois; RUN;
PROC SORT DATA=MYDATA.panel95_table; BY mident mois; RUN;

DATA MYDATA2.fusion_table; MERGE MYDATA2.Colselected (IN=a) MYDATA.panel95_table (IN=b); BY mident mois; IF a AND b; RUN;

/* QUESTION 2. Regression (MCO) 'actifs' sur les différentes indicatrice d'éducation*/
PROC REG DATA=MYDATA2.fusion_table OUTEST=estimates;
    MODEL actifs = primaire secondaire professionnel_court professionnel_long cycle2 cycle3;
    OUTPUT OUT=MYDATA2.fusion_table PREDICTED=p1reg RESIDUAL=residual;
    TITLE "Régression MCO de la probabilité d'activité secondaire (actifs) sur l'éducation";
RUN;

/* QUESTION3:Ajoute à la table de travail, la probabilité d'activité secondaire ("actifs") qui a été prédite par le modèle MCO, noté p1reg, ainsi qu'un estimateur de l'écart-type du résidu, noté 'sigmalreg'. 
             Représenter graphiquement la distribution de p1reg */

/*ajout de la prédiction p1reg à la table => cf question2*/
DATA MYDATA2.fusion_table; SET MYDATA2.fusion_table; sigmalreg = SQRT(p1reg * (1 - p1reg)); RUN;

/**Représentation graphique de la distribution p1reg*/
PROC SGPLOT DATA=MYDATA2.fusion_table;
    HISTOGRAM p1reg / FILLATTRS=(COLOR=skyblue TRANSPARENCY=0.3);
    DENSITY p1reg / TYPE=KERNEL LINEATTRS=(COLOR=green THICKNESS=2) LEGENDLABEL="Kernel";
    DENSITY p1reg / TYPE=NORMAL LINEATTRS=(COLOR=red PATTERN=DASH) LEGENDLABEL="Normale";
    KEYLEGEND / LOCATION=INSIDE POSITION=TOPRIGHT;
    TITLE "Distribution de la probabilité d'activité secondaire prédite p1reg ";
RUN;


/*QUESTION 4 : Régresser (MCP) actifs sur les différentes indicatrices d'éducation*/
                   /*Calcul du poids */
DATA MYDATA2.fusion_table; SET MYDATA2.fusion_table; poids_mcp = 1 / sigmalreg; RUN;
         /*Regression MCP (pour corriger l'hétéroscédasticité)*/
PROC REG DATA=MYDATA2.fusion_table;
    MODEL actifs = primaire secondaire professionnel_court professionnel_long cycle2 cycle3;
    WEIGHT poids_mcp;
    TITLE "Régression MCP de la probabilité d'activité secondaire (actifs) sur l'éducation ";
RUN;

/*QUESTION 5: créer les variables : agea2 = agea² et agea = agea au cube*/
DATA MYDATA2.fusion_table; SET MYDATA2.fusion_table; agea2 = agea**2; agea3 = agea**3; RUN;

/*Question 6 : Regresser actifs sur agea, agea2, agea3 ET les différentes indicatrices d'éducation Et l'indicatrice de sexe féminin*/
PROC REG DATA=MYDATA2.fusion_table;
    MODEL actifs = agea agea2 agea3 primaire secondaire professionnel_court professionnel_long cycle2 cycle3 Femme;
    OUTPUT OUT=MYDATA2.fusion_table PREDICTED=p2reg;
    TITLE "Régression MCO de la probabilité d'activité secondaire sur l'éducation, l'âge et le sexe (Femme)";
RUN;

/*QUESTION 7: Ajouter à la table de travail la propabilité d'activité secondaire prédite par ce nouveau modèle notée p2reg. Représenter graphiquement la distribution de p2reg*/
/*Ajout des proba prédit cf question 6*/

PROC SGPLOT DATA=MYDATA2.fusion_table;
    HISTOGRAM p2reg / FILLATTRS=(COLOR=CX5A9BD4 TRANSPARENCY=0.3);
    DENSITY p2reg / TYPE=NORMAL LINEATTRS=(COLOR=red PATTERN=DASH) LEGENDLABEL="Normale";
    DENSITY p2reg / TYPE=KERNEL LINEATTRS=(COLOR=CX3CB371 THICKNESS=2) LEGENDLABEL="Kernel";
    TITLE "Distribution de la probabilité d'activité secondaire prédite (p2reg)";
RUN;


/*QUESTION 9: Comparer les résultats obtenus en remplacant l'indicatrice de sexe féminin par l'indicatrice de sexe masculin, puis par la variable sexeN. Commenter précisement chacune des différences entre ces trois modèles*/

/*Modèle avec Homme*/
PROC REG DATA=MYDATA2.fusion_table;
    MODEL actifs = agea agea2 agea3 primaire secondaire professionnel_court professionnel_long cycle2 cycle3 Homme;
    TITLE "Régression avec l'indicatrice Homme";
RUN;

/* Modèle avec sexeN */
PROC REG DATA=MYDATA2.fusion_table;
    MODEL actifs = agea agea2 agea3 primaire secondaire professionnel_court professionnel_long cycle2 cycle3 sexeN;
    TITLE "Régression avec sexeN";
RUN;


