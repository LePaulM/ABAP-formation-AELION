# Layouts ALV en ABAP - Tutoriel Complet

## 📋 Table des matières

1. [Qu'est-ce qu'un layout ALV ?](#quest-ce-quun-layout-alv)
2. [Types de layouts](#types-de-layouts)
3. [Création d'un layout utilisateur](#création-dun-layout-utilisateur)
4. [Gestion des layouts en programmation](#gestion-des-layouts-en-programmation)
5. [Layout par défaut](#layout-par-défaut)
6. [Variantes de sélection](#variantes-de-sélection)
7. [Bonnes pratiques](#bonnes-pratiques)
8. [Exemples pratiques](#exemples-pratiques)

---

## Qu'est-ce qu'un layout ALV ?

Un **layout ALV** (SAP List Viewer) est une configuration personnalisée de l'affichage d'un rapport qui permet de :

- 🎨 **Personnaliser** l'ordre et la visibilité des colonnes
- 📊 **Définir** des tris et des filtres
- 💾 **Sauvegarder** ces préférences pour une réutilisation ultérieure
- 👥 **Partager** les configurations entre utilisateurs

### Avantages

✅ Gain de temps : réutilisation des configurations  
✅ Personnalisation : chaque utilisateur adapte l'affichage à ses besoins  
✅ Standardisation : layouts partagés au niveau équipe/société  
✅ Productivité : accès rapide aux données pertinentes  

---

## Types de layouts

### 1. Layout Utilisateur (User-Specific)

**Caractéristiques :**
- Créé et modifiable uniquement par l'utilisateur
- Visible uniquement pour son créateur
- Stocké avec le nom d'utilisateur

**Icône :** 👤

### 2. Layout Global (All Users)

**Caractéristiques :**
- Créé par un super-utilisateur ou développeur
- Visible par tous les utilisateurs
- Peut être défini comme layout par défaut
- Nécessite des autorisations spéciales

**Icône :** 🌍

### 3. Layout Standard

**Caractéristiques :**
- Fourni par SAP ou le développeur
- Configuration de base du rapport
- Point de départ pour les personnalisations

**Icône :** ⚙️

---

## Création d'un layout utilisateur

### Étape 1 : Exécuter le rapport

```abap
" J'exécute mon programme ALV
SE38 → Z_MON_PROGRAMME → F8
```

### Étape 2 : Personnaliser l'affichage

**Actions disponibles dans l'ALV :**

1. **Modifier les colonnes** :
   - Clic droit sur en-tête de colonne → Masquer/Afficher
   - Glisser-déposer pour réorganiser
   - Double-clic sur séparateur pour ajuster la largeur

2. **Appliquer des filtres** :
   - Menu → Filtre → Filtre par colonne
   - Clic sur l'icône entonnoir 🔍

3. **Trier les données** :
   - Clic sur en-tête de colonne (tri croissant/décroissant)
   - Tri multiple : maintenir Ctrl + clic

4. **Sous-totaux** :
   - Menu → Sous-totaux → Définir

### Étape 3 : Sauvegarder le layout

**Méthode 1 : Bouton "Sauvegarder layout"**

```
1. Clic sur l'icône disquette 💾 (ou F4)
2. Nommer le layout : "MON_LAYOUT_COMMANDES"
3. Cocher "Par défaut" si souhaité
4. Sauvegarder
```

**Méthode 2 : Menu**

```
Menu → Layout → Sauvegarder
```

### Étape 4 : Réutiliser le layout

```
1. Exécuter le rapport
2. Clic sur liste déroulante "Layout"
3. Sélectionner "MON_LAYOUT_COMMANDES"
4. ✅ L'affichage se charge avec vos préférences
```

---

## Gestion des layouts en programmation

### Structure du layout ALV

```abap
" Je déclare la structure de layout
DATA: ls_layout TYPE lvc_s_layo.

" ===== CONFIGURATION DE BASE =====

ls_layout-zebra      = 'X'.        " Lignes alternées (zébrées)
ls_layout-cwidth_opt = 'X'.        " Optimisation largeur colonnes
ls_layout-no_toolbar = space.      " Afficher la toolbar
ls_layout-sel_mode   = 'A'.        " Mode sélection (A=ligne+cellule, D=cellule)
ls_layout-grid_title = 'Liste des commandes'.  " Titre de la grille
ls_layout-smalltitle = 'X'.        " Titre en petit
```

### Options de sélection

**Modes de sélection (sel_mode) :**

| Code | Description | Usage |
|------|-------------|-------|
| `' '` | Pas de sélection | Affichage simple |
| `'A'` | Ligne et cellule | Standard |
| `'B'` | Bloc | Sélection rectangle |
| `'C'` | Colonne | Sélection par colonne |
| `'D'` | Cellule uniquement | Édition cellule par cellule |

### Options d'affichage avancées

```abap
" ===== APPARENCE =====

ls_layout-stylefname = 'STYLE'.     " Colonne contenant les styles de cellule
ls_layout-ctab_fname = 'CELLCOLOR'. " Colonne contenant les couleurs de cellule
ls_layout-info_fname = 'ROWCOLOR'.  " Colonne contenant la couleur de ligne
ls_layout-excp_fname = 'EXCEPTION'. " Colonne contenant les exceptions (feux tricolores)

" ===== FONCTIONNALITÉS =====

ls_layout-no_merging = 'X'.         " Pas de fusion de cellules identiques
ls_layout-box_fname  = 'CHECKBOX'.  " Colonne contenant une checkbox
ls_layout-edit       = 'X'.         " ALV éditable
ls_layout-no_rowmark = space.       " Autoriser les marques de ligne
```

### Layout avec gestion de variantes

```abap
" ===== DÉCLARATION =====

DATA: ls_layout  TYPE lvc_s_layo,
      ls_variant TYPE disvariant.

" ===== CONFIGURATION VARIANTE =====

" Je spécifie le programme de référence
ls_variant-report = sy-repid.

" Je définis un layout par défaut (optionnel)
ls_variant-variant = '/DEFAULT'.

" ===== APPEL ALV AVEC VARIANTE =====

CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
  EXPORTING
    i_callback_program      = sy-repid
    is_layout_lvc           = ls_layout
    i_save                  = 'A'         " A=tous, U=user, X=global
    is_variant              = ls_variant
    it_fieldcat_lvc         = lt_fieldcat
  TABLES
    t_outtab                = gt_data
  EXCEPTIONS
    program_error           = 1
    OTHERS                  = 2.
```

### Options de sauvegarde (i_save)

| Valeur | Description | Utilisateurs concernés |
|--------|-------------|------------------------|
| `'X'` | Global uniquement | Administrateurs |
| `'U'` | Utilisateur uniquement | Utilisateur courant |
| `'A'` | Les deux (ALL) | Tous |
| `' '` | Pas de sauvegarde | Aucun |

---

## Layout par défaut

### Définir un layout par défaut côté utilisateur

**Depuis l'ALV :**

```
1. Créer/Sélectionner le layout souhaité
2. Menu → Layout → Administration
3. Cocher "Par défaut"
4. Sauvegarder
```

### Définir un layout par défaut en code

```abap
" Je force un layout spécifique au démarrage
DATA: ls_variant TYPE disvariant.

ls_variant-report  = sy-repid.
ls_variant-variant = 'LAYOUT_STANDARD'.  " Nom du layout à charger

" Option 1 : Layout suggéré (utilisateur peut changer)
CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
  EXPORTING
    is_variant = ls_variant
    " ...

" Option 2 : Layout forcé (utilisateur ne peut pas changer)
ls_layout-no_varia = 'X'.  " Désactive la sélection de layout
```

### Récupérer le layout par défaut

```abap
" Je récupère le layout par défaut de l'utilisateur
DATA: ls_variant TYPE disvariant.

ls_variant-report = sy-repid.

CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
  EXPORTING
    i_save        = 'A'
  CHANGING
    cs_variant    = ls_variant
  EXCEPTIONS
    wrong_input   = 1
    not_found     = 2
    program_error = 3
    OTHERS        = 4.

IF sy-subrc = 0.
  WRITE: / 'Layout par défaut:', ls_variant-variant.
ELSE.
  WRITE: / 'Pas de layout par défaut'.
ENDIF.
```

---

## Variantes de sélection

### Différence Layout vs Variante

| Aspect | Layout ALV | Variante de sélection |
|--------|------------|------------------------|
| **Portée** | Affichage des résultats | Critères de sélection |
| **Contenu** | Colonnes, tris, filtres | Valeurs des paramètres |
| **Transaction** | Gestion dans l'ALV | SE38/SA38 |
| **Utilisation** | Après exécution | Avant exécution |

### Créer une variante de sélection

**Étape 1 : Définir les critères**

```
1. SE38 → Programme → F8
2. Remplir les critères de sélection
3. Menu → Goto → Variantes → Sauvegarder comme variante
4. Nom : VENTE_JANVIER_2025
5. Description : Ventes de janvier 2025
6. Sauvegarder
```

**Étape 2 : Protéger des champs (optionnel)**

```
Menu → Goto → Variantes → Attributs de variante
- Cocher "Protégé" pour bloquer un champ
- Cocher "Invisible" pour masquer un champ
- Cocher "Obligatoire" pour forcer la saisie
```

**Étape 3 : Utiliser la variante**

```
SE38 → F8 (ou F5)
Clic sur "Obtenir variante" 
→ Sélectionner VENTE_JANVIER_2025
→ F8
```

### Gérer les variantes en code

```abap
" ===== ÉCRAN DE SÉLECTION AVEC VARIANTE =====

PARAMETERS: p_bukrs TYPE bukrs DEFAULT '1000'.
SELECT-OPTIONS: s_datum FOR sy-datum.

" Je propose une variante par défaut
INITIALIZATION.
  " Code d'initialisation si besoin

" ===== CRÉER UNE VARIANTE PROGRAMMATIQUEMENT =====

DATA: lt_vari_desc TYPE TABLE OF rsvar,
      ls_vari_desc TYPE rsvar,
      lt_vari_cont TYPE TABLE OF rsvarc,
      ls_vari_cont TYPE rsvarc.

" Je définis les valeurs de la variante
ls_vari_cont-vari = 'MA_VARIANTE'.
ls_vari_cont-type = 'P'.           " P=Parameter, S=Select-option
ls_vari_cont-field = 'P_BUKRS'.
ls_vari_cont-low = '1000'.
APPEND ls_vari_cont TO lt_vari_cont.

" Je crée la variante
CALL FUNCTION 'RS_CREATE_VARIANT'
  EXPORTING
    program         = sy-repid
  TABLES
    variant_desc    = lt_vari_desc
    variant_content = lt_vari_cont
  EXCEPTIONS
    OTHERS          = 1.
```

---

## Bonnes pratiques

### ✅ Nomenclature des layouts

**Convention recommandée :**

```
[DOMAINE]_[FONCTION]_[USER/TEAM]

Exemples :
- VENTES_DETAILS_USER      (layout personnel)
- VENTES_SYNTHESE_TEAM     (layout équipe)
- COMPTA_BALANCE_MENSUEL   (layout comptabilité)
- RH_ABSENCES_MANAGER      (layout manager RH)
```

### ✅ Documentation

Toujours ajouter une description claire :

```abap
" Bon
ls_variant-text = 'Commandes clients avec montants > 1000€'.

" Mauvais
ls_variant-text = 'Layout1'.
```

### ✅ Gestion des autorisations

```abap
" Je vérifie les autorisations avant de permettre la sauvegarde
AUTHORITY-CHECK OBJECT 'S_ALV_LAYO'
  ID 'ACTVT' FIELD '02'.  " 02 = Modifier

IF sy-subrc = 0.
  " J'autorise la sauvegarde de layouts
  lv_save = 'A'.
ELSE.
  " Je limite aux layouts utilisateur
  lv_save = 'U'.
ENDIF.
```

### ✅ Layouts globaux : validation

Avant de créer un layout global :

1. ✅ Tester avec plusieurs jeux de données
2. ✅ Valider avec les utilisateurs finaux
3. ✅ Documenter l'usage prévu
4. ✅ Former les équipes

### ❌ À éviter

```abap
" ❌ Désactiver complètement la sauvegarde
i_save = ' '.  " L'utilisateur ne peut pas sauvegarder ses préférences

" ❌ Forcer un layout sans possibilité de changement
ls_layout-no_varia = 'X'.  " Blocage total

" ❌ Nom de layout non explicite
ls_variant-variant = 'A1'.  " Incompréhensible
```

---

## Exemples pratiques

### Exemple 1 : ALV simple avec sauvegarde de layout

```abap
*&---------------------------------------------------------------------*
*& Report Z_ALV_LAYOUT_DEMO_01
*&---------------------------------------------------------------------*
REPORT z_alv_layout_demo_01.

" ===== DÉCLARATIONS =====

TYPES: BEGIN OF ty_ventes,
         vbeln TYPE vbeln,     " Numéro commande
         kunnr TYPE kunnr,     " Client
         netwr TYPE netwr,     " Montant net
         waerk TYPE waerk,     " Devise
       END OF ty_ventes.

DATA: gt_ventes   TYPE TABLE OF ty_ventes,
      gs_ventes   TYPE ty_ventes,
      gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_fieldcat TYPE slis_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      gs_variant  TYPE disvariant.

" ===== ÉCRAN DE SÉLECTION =====

SELECT-OPTIONS: s_vbeln FOR gs_ventes-vbeln.

" ===== TRAITEMENT =====

START-OF-SELECTION.

  " Je sélectionne les données (exemple simplifié)
  SELECT vbeln kunnr netwr waerk
    FROM vbak
    INTO TABLE gt_ventes
    UP TO 100 ROWS
    WHERE vbeln IN s_vbeln.

  IF gt_ventes IS INITIAL.
    MESSAGE 'Aucune donnée trouvée' TYPE 'S'.
    RETURN.
  ENDIF.

  " Je configure le field catalog
  PERFORM f_build_fieldcat.

  " Je configure le layout
  PERFORM f_build_layout.

  " Je configure la variante
  gs_variant-report = sy-repid.

  " J'affiche l'ALV
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = gs_layout
      it_fieldcat        = gt_fieldcat
      i_save             = 'A'           " Sauvegarde autorisée
      is_variant         = gs_variant
    TABLES
      t_outtab           = gt_ventes
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

*&---------------------------------------------------------------------*
*& Form f_build_fieldcat
*&---------------------------------------------------------------------*
FORM f_build_fieldcat.
  " Je crée le catalogue de champs

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'VBELN'.
  gs_fieldcat-seltext_l = 'Commande'.
  gs_fieldcat-col_pos   = 1.
  gs_fieldcat-key       = 'X'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'KUNNR'.
  gs_fieldcat-seltext_l = 'Client'.
  gs_fieldcat-col_pos   = 2.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'NETWR'.
  gs_fieldcat-seltext_l = 'Montant net'.
  gs_fieldcat-col_pos   = 3.
  gs_fieldcat-do_sum    = 'X'.         " Somme automatique
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'WAERK'.
  gs_fieldcat-seltext_l = 'Devise'.
  gs_fieldcat-col_pos   = 4.
  APPEND gs_fieldcat TO gt_fieldcat.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_build_layout
*&---------------------------------------------------------------------*
FORM f_build_layout.
  " Je configure l'apparence de l'ALV

  gs_layout-zebra         = 'X'.       " Lignes zébrées
  gs_layout-colwidth_optimize = 'X'.   " Optimiser largeur
  gs_layout-grid_title    = 'Liste des ventes'.
  gs_layout-smalltitle    = 'X'.
ENDFORM.
```

### Exemple 2 : ALV OO avec variante et layout par défaut

```abap
*&---------------------------------------------------------------------*
*& Report Z_ALV_LAYOUT_DEMO_02
*&---------------------------------------------------------------------*
REPORT z_alv_layout_demo_02.

" ===== DÉCLARATIONS =====

DATA: go_alv       TYPE REF TO cl_gui_alv_grid,
      go_container TYPE REF TO cl_gui_custom_container,
      gt_data      TYPE TABLE OF sflight,
      gs_layout    TYPE lvc_s_layo,
      gs_variant   TYPE disvariant,
      gt_fieldcat  TYPE lvc_t_fcat.

" ===== ÉCRAN DE SÉLECTION =====

SELECT-OPTIONS: s_carrid FOR sflight-carrid.

" ===== TRAITEMENT =====

START-OF-SELECTION.
  CALL SCREEN 100.

*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_100'.
  SET TITLEBAR 'TITLE_100'.

  IF go_alv IS INITIAL.
    PERFORM f_display_alv.
  ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module USER_COMMAND_0100 INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      IF go_alv IS BOUND.
        CALL METHOD go_alv->free.
      ENDIF.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form f_display_alv
*&---------------------------------------------------------------------*
FORM f_display_alv.
  " Variables locales
  DATA: ls_fieldcat TYPE lvc_s_fcat.

  " Je charge les données
  SELECT * FROM sflight
    INTO TABLE gt_data
    UP TO 50 ROWS
    WHERE carrid IN s_carrid.

  " Je crée le container
  CREATE OBJECT go_container
    EXPORTING
      container_name = 'CUSTOM_CONTAINER'.

  " Je crée l'ALV
  CREATE OBJECT go_alv
    EXPORTING
      i_parent = go_container.

  " Je configure le layout
  gs_layout-zebra      = 'X'.
  gs_layout-cwidth_opt = 'X'.
  gs_layout-grid_title = 'Vols disponibles'.
  gs_layout-sel_mode   = 'A'.

  " Je configure la variante
  gs_variant-report = sy-repid.
  gs_variant-username = sy-uname.

  " Je récupère le layout par défaut si existe
  CALL FUNCTION 'LVC_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = 'A'
    CHANGING
      cs_variant = gs_variant
    EXCEPTIONS
      OTHERS     = 1.

  " Je construis le field catalog automatiquement
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'SFLIGHT'
    CHANGING
      ct_fieldcat      = gt_fieldcat
    EXCEPTIONS
      OTHERS           = 1.

  " J'affiche l'ALV
  CALL METHOD go_alv->set_table_for_first_display
    EXPORTING
      is_variant      = gs_variant
      i_save          = 'A'
      is_layout       = gs_layout
    CHANGING
      it_outtab       = gt_data
      it_fieldcatalog = gt_fieldcat.
ENDFORM.
```

### Exemple 3 : Aide à la sélection de variante

```abap
*&---------------------------------------------------------------------*
*& Report Z_ALV_LAYOUT_DEMO_03
*&---------------------------------------------------------------------*
REPORT z_alv_layout_demo_03.

" ===== DÉCLARATIONS =====

PARAMETERS: p_vari TYPE slis_vari.

" ===== ÉVÉNEMENTS =====

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  " Je propose l'aide F4 pour les variantes
  PERFORM f_variant_f4.

START-OF-SELECTION.
  " Je charge et affiche avec la variante sélectionnée
  PERFORM f_display_with_variant.

*&---------------------------------------------------------------------*
*& Form f_variant_f4
*&---------------------------------------------------------------------*
FORM f_variant_f4.
  " Variables locales
  DATA: ls_variant TYPE disvariant,
        lv_exit    TYPE c.

  " Je configure la recherche de variantes
  ls_variant-report = sy-repid.

  " J'affiche l'aide F4 pour les variantes
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = ls_variant
      i_save     = 'A'
    IMPORTING
      e_exit     = lv_exit
      es_variant = ls_variant
    EXCEPTIONS
      not_found  = 1
      OTHERS     = 2.

  IF sy-subrc = 0 AND lv_exit = space.
    " J'ai sélectionné une variante
    p_vari = ls_variant-variant.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_display_with_variant
*&---------------------------------------------------------------------*
FORM f_display_with_variant.
  " Variables locales
  DATA: lt_data     TYPE TABLE OF mara,
        lt_fieldcat TYPE slis_t_fieldcat_alv,
        ls_layout   TYPE slis_layout_alv,
        ls_variant  TYPE disvariant.

  " Je charge des données d'exemple
  SELECT * FROM mara
    INTO TABLE lt_data
    UP TO 100 ROWS.

  " Je configure la variante à utiliser
  ls_variant-report  = sy-repid.
  ls_variant-variant = p_vari.

  " Je vérifie l'existence de la variante
  CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
    EXPORTING
      i_save     = 'A'
    CHANGING
      cs_variant = ls_variant
    EXCEPTIONS
      not_found  = 1
      OTHERS     = 2.

  IF sy-subrc <> 0.
    MESSAGE 'Variante introuvable' TYPE 'W'.
    CLEAR ls_variant-variant.
  ENDIF.

  " Je configure le layout
  ls_layout-zebra      = 'X'.
  ls_layout-cwidth_opt = 'X'.

  " Je construis le field catalog
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'MARA'
    CHANGING
      ct_fieldcat      = lt_fieldcat
    EXCEPTIONS
      OTHERS           = 1.

  " J'affiche l'ALV avec la variante
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = ls_layout
      it_fieldcat        = lt_fieldcat
      i_save             = 'A'
      is_variant         = ls_variant
    TABLES
      t_outtab           = lt_data
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
ENDFORM.
```

### Exemple 4 : Layout avec couleurs et styles personnalisés

```abap
*&---------------------------------------------------------------------*
*& Report Z_ALV_LAYOUT_DEMO_04
*&---------------------------------------------------------------------*
REPORT z_alv_layout_demo_04.

" ===== DÉCLARATIONS =====

TYPES: BEGIN OF ty_commande,
         vbeln     TYPE vbeln,
         kunnr     TYPE kunnr,
         netwr     TYPE netwr,
         status    TYPE char1,
         " Champs techniques pour l'affichage
         row_color TYPE lvc_t_scol,   " Couleur de ligne
         cell_color TYPE lvc_t_scol,  " Couleur de cellule
       END OF ty_commande.

DATA: gt_commandes TYPE TABLE OF ty_commande,
      gs_commande  TYPE ty_commande,
      gt_fieldcat  TYPE lvc_t_fcat,
      gs_fieldcat  TYPE lvc_s_fcat,
      gs_layout    TYPE lvc_s_layo,
      go_alv       TYPE REF TO cl_gui_alv_grid,
      go_container TYPE REF TO cl_gui_custom_container.

" ===== TRAITEMENT =====

START-OF-SELECTION.
  PERFORM f_get_data.
  CALL SCREEN 100.

*&---------------------------------------------------------------------*
*& Form f_get_data
*&---------------------------------------------------------------------*
FORM f_get_data.
  " Variables locales pour les couleurs
  DATA: ls_color TYPE lvc_s_scol.

  " Je crée des données de test
  gs_commande-vbeln = '0000000001'.
  gs_commande-kunnr = '0000100001'.
  gs_commande-netwr = '1000.00'.
  gs_commande-status = 'A'.  " Actif = vert

  " Je définis la couleur de la ligne (vert pour actif)
  ls_color-fname = ''.
  ls_color-color-col = 5.  " Vert
  ls_color-color-int = 0.
  ls_color-color-inv = 0.
  APPEND ls_color TO gs_commande-row_color.

  APPEND gs_commande TO gt_commandes.

  CLEAR: gs_commande, ls_color.
  gs_commande-vbeln = '0000000002'.
  gs_commande-kunnr = '0000100002'.
  gs_commande-netwr = '500.00'.
  gs_commande-status = 'B'.  " Bloqué = rouge

  " Je définis la couleur de la ligne (rouge pour bloqué)
  ls_color-fname = ''.
  ls_color-color-col = 6.  " Rouge
  ls_color-color-int = 0.
  ls_color-color-inv = 0.
  APPEND ls_color TO gs_commande-row_color.

  APPEND gs_commande TO gt_commandes.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  IF go_alv IS INITIAL.
    PERFORM f_display_alv.
  ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Form f_display_alv
*&---------------------------------------------------------------------*
FORM f_display_alv.
  " Je crée le container
  CREATE OBJECT go_container
    EXPORTING
      container_name = 'CUSTOM_CONTAINER'.

  " Je crée l'ALV
  CREATE OBJECT go_alv
    EXPORTING
      i_parent = go_container.

  " Je construis le field catalog
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'VBELN'.
  gs_fieldcat-scrtext_l = 'Commande'.
  gs_fieldcat-col_pos   = 1.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'KUNNR'.
  gs_fieldcat-scrtext_l = 'Client'.
  gs_fieldcat-col_pos   = 2.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'NETWR'.
  gs_fieldcat-scrtext_l = 'Montant'.
  gs_fieldcat-col_pos   = 3.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'STATUS'.
  gs_fieldcat-scrtext_l = 'Statut'.
  gs_fieldcat-col_pos   = 4.
  APPEND gs_fieldcat TO gt_fieldcat.

  " Je configure le layout avec couleurs
  gs_layout-zebra      = 'X'.
  gs_layout-cwidth_opt = 'X'.
  gs_layout-ctab_fname = 'ROW_COLOR'.  " Colonne contenant les couleurs
  gs_layout-grid_title = 'Commandes avec couleurs'.

  " J'affiche l'ALV
  CALL METHOD go_alv->set_table_for_first_display
    EXPORTING
      is_layout       = gs_layout
    CHANGING
      it_outtab       = gt_commandes
      it_fieldcatalog = gt_fieldcat.
ENDFORM.
```

---

## Codes couleur ALV

### Couleurs de ligne (row_color)

| Code | Couleur | Usage recommandé |
|------|---------|------------------|
| 1 | Bleu clair | Information |
| 2 | Gris clair | Désactivé/Inactif |
| 3 | Jaune | Attention/Warning |
| 4 | Bleu | Sélection |
| 5 | Vert | Succès/Validé |
| 6 | Rouge | Erreur/Bloqué |
| 7 | Orange | En cours |

### Intensité (int)

- `0` : Normal
- `1` : Intensifié (plus foncé)

### Inverse (inv)

- `0` : Normal
- `1` : Inversé (texte clair sur fond foncé)

---

## Récapitulatif des fonctions principales

| Fonction | Usage |
|----------|-------|
| `REUSE_ALV_VARIANT_DEFAULT_GET` | Récupère le layout par défaut |
| `REUSE_ALV_VARIANT_EXISTENCE` | Vérifie l'existence d'une variante |
| `REUSE_ALV_VARIANT_F4` | Aide F4 pour sélectionner une variante |
| `LVC_VARIANT_DEFAULT_GET` | Variante par défaut (ALV OO) |
| `RS_CREATE_VARIANT` | Crée une variante programmatiquement |

---

## Checklist finale

Avant de mettre en production un rapport avec layouts :

- ✅ Tester avec plusieurs utilisateurs
- ✅ Vérifier la sauvegarde des layouts (i_save = 'A')
- ✅ Documenter les layouts standards/globaux
- ✅ Nommer clairement les layouts
- ✅ Tester les variantes de sélection
- ✅ Vérifier les autorisations
- ✅ Former les utilisateurs finaux

---

## Ressources complémentaires

**Transactions utiles :**
- `SE80` : Développement
- `SE38` : Éditeur de programmes
- `STVARV` : Gestion des variantes
- `SUIM` : Gestion des autorisations

**Tables système :**
- `LT14D` : Layouts ALV sauvegardés
- `VARID` : Variantes de sélection
- `TVARC` : Contenu des variantes

---

**Version :** 1.0  
**Date :** Janvier 2026  
**Auteur :** Cours ABAP Formation
