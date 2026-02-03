# DYNPRO - Écrans de Dialogue

## Vue d'ensemble

Les Dynpro (Dynamic Program) sont des écrans de dialogue interactifs permettant la saisie et l'affichage de données. Ils constituent l'interface utilisateur classique SAP.

---

## Architecture d'un Dynpro

### Composants essentiels

```
Programme ABAP (PBO/PAI)
         ↓
    Screen Painter (SE51)
         ↓
    Écran (Layout + Attributs)
         ↓
    Flow Logic (PBO/PAI)
```

**PBO** : Process Before Output - Avant affichage  
**PAI** : Process After Input - Après saisie utilisateur

---

## Création d'un Dynpro

### 1. Créer l'écran (SE51 ou SE80)

**Transaction :** SE51 ou SE80 → Clic droit programme → Créer → Écran

**Numérotation standard :**
- 0100-0999 : Écrans normaux
- 1000-8999 : Écrans de sélection
- 9000-9999 : Écrans modaux (popups)

**Attributs écran :**
```abap
" Type d'écran
" Normal : Écran plein
" Modal : Popup bloquant
" Subscreen : Sous-écran intégrable
```

### 2. Dessiner le layout (Screen Painter)

**Éléments disponibles :**
- Champs de saisie (Input/Output)
- Cases à cocher (Checkbox)
- Boutons radio (Radio Button)
- Boutons (Push Button)
- Zones de texte (Text Field)
- Tables de contrôle (Table Control)
- Subscreens

**Propriétés champ :**
- Nom technique (lié à variable ABAP)
- Format (CHAR, NUMC, DATS, etc.)
- Longueur
- Input/Output/Display
- Obligatoire (Required)
- Aide à la recherche (Search Help)

---

## Flow Logic

### Structure type

```abap
PROCESS BEFORE OUTPUT.
  MODULE status_0100.
  MODULE init_data.

PROCESS AFTER INPUT.
  MODULE user_command_0100.
  MODULE check_data.
```

### Modules obligatoires

**PBO - Initialisation :**
```abap
MODULE status_0100 OUTPUT.
  " Je définis le GUI status (menus, boutons)
  SET PF-STATUS 'STATUS_0100'.
  " Je définis le titre de l'écran
  SET TITLEBAR 'TITLE_0100'.
ENDMODULE.

MODULE init_data OUTPUT.
  " Je charge les données à afficher
  " Uniquement si première fois (sy-ucomm IS INITIAL)
  IF sy-ucomm IS INITIAL.
    " Je remplis mes variables globales
    gv_matnr = 'MAT001'.
    gv_maktx = 'Description article'.
  ENDIF.
ENDMODULE.
```

**PAI - Traitement saisie :**
```abap
MODULE user_command_0100 INPUT.
  " Je traite les actions utilisateur
  CASE sy-ucomm.
    WHEN 'SAVE'.
      PERFORM save_data.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'SEARCH'.
      PERFORM search_material.
  ENDCASE.
  
  " Je vide le code commande pour éviter retraitement
  CLEAR sy-ucomm.
ENDMODULE.

MODULE check_data INPUT.
  " Je valide les données saisies
  IF gv_matnr IS INITIAL.
    MESSAGE 'Le numéro article est obligatoire' TYPE 'E'.
  ENDIF.
ENDMODULE.
```

---

## Variables globales écran

### Déclaration TOP

```abap
" Variables liées aux champs écran
DATA: gv_matnr TYPE matnr,    " Champ écran : GV_MATNR
      gv_maktx TYPE maktx,    " Champ écran : GV_MAKTX
      gv_meins TYPE meins,    " Champ écran : GV_MEINS
      gv_check TYPE xfeld.    " Checkbox : GV_CHECK

" Variables internes
DATA: gt_mara TYPE TABLE OF mara,
      gs_mara TYPE mara.

" Variables système
" sy-ucomm : Code fonction (bouton cliqué)
" sy-dynnr : Numéro écran courant
" sy-repid : Programme courant
```

**Important :** Les noms de variables écran doivent correspondre EXACTEMENT aux noms des champs dans le Screen Painter (insensible à la casse).

---

## GUI Status

### Création (SE41 ou SE80)

**Menu Painter :**
1. Clic droit programme → Créer → GUI Status
2. Nom : STATUS_0100
3. Type : Dialog Box

**Éléments :**

**Barre d'application (Toolbar) :**
```abap
" Codes fonction standard
BACK  : Retour
EXIT  : Sortir
CANCEL: Annuler

" Codes fonction personnalisés
SAVE  : Sauvegarder
SEARCH: Rechercher
DELETE: Supprimer
```

**Barre de menus :**
```
Menu 1: Fichier
  - SAVE  : Sauvegarder
  - EXIT  : Quitter
  
Menu 2: Édition
  - SEARCH: Rechercher
  - DELETE: Supprimer
```

**Touches de fonction (F-keys) :**
```
F3  → BACK
F12 → CANCEL
F8  → SAVE
```

---

## Appel d'écran

### Méthodes d'appel

**1. Écran unique :**
```abap
CALL SCREEN 0100.
```

**2. Enchaînement d'écrans :**
```abap
" Depuis PBO ou PAI
SET SCREEN 0200.  " Je prépare l'écran suivant
LEAVE SCREEN.     " Je quitte l'écran courant
```

**3. Écran modal (popup) :**
```abap
CALL SCREEN 9000 STARTING AT 10 10
                 ENDING AT 70 20.
```

**4. Quitter tous les écrans :**
```abap
LEAVE TO SCREEN 0.  " Retour au programme appelant
```

---

## Contrôles dynamiques

### Modification champs depuis ABAP

**PBO - Avant affichage :**
```abap
MODULE modify_screen OUTPUT.
  " Je boucle sur tous les champs de l'écran
  LOOP AT SCREEN.
    " Je rends le champ GV_MATNR non modifiable si mode affichage
    IF screen-name = 'GV_MATNR' AND gv_mode = 'D'.
      screen-input = 0.  " Pas de saisie
      MODIFY SCREEN.
    ENDIF.
    
    " Je cache le champ GV_MEINS si vide
    IF screen-name = 'GV_MEINS' AND gv_meins IS INITIAL.
      screen-active = 0.  " Champ invisible
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDMODULE.
```

**Attributs SCREEN :**
- `screen-name` : Nom du champ
- `screen-input` : 1 = modifiable, 0 = lecture seule
- `screen-output` : 1 = visible, 0 = invisible
- `screen-active` : 1 = actif, 0 = inactif (grisé)
- `screen-required` : 1 = obligatoire, 0 = optionnel

---

## Validation données

### Contrôle PAI

```abap
MODULE check_matnr INPUT.
  " Je vérifie que l'article existe
  IF gv_matnr IS NOT INITIAL.
    SELECT SINGLE @abap_true
      FROM mara
      WHERE matnr = @gv_matnr
      INTO @DATA(lv_exists).
    
    IF sy-subrc NE 0.
      " J'affiche erreur et je bloque l'écran
      MESSAGE 'Article inexistant' TYPE 'E'.
    ENDIF.
  ENDIF.
ENDMODULE.
```

**Types message :**
- `E` : Erreur (bloque l'écran)
- `W` : Warning (confirmation nécessaire)
- `I` : Information (popup)
- `S` : Succès (barre de statut)

### Contrôle avec FIELD

```abap
PROCESS AFTER INPUT.
  " Je contrôle GV_MATNR dès sa saisie
  FIELD gv_matnr MODULE check_matnr.
```

**Avec ON INPUT :**
```abap
" Contrôle uniquement si le champ a été modifié
FIELD gv_matnr MODULE check_matnr ON INPUT.
```

---

## Subscreens

### Principe

Permet d'intégrer un écran dans un autre (modularité, réutilisabilité).

**Écran principal (0100) :**
```abap
" Layout : Créer une zone SUBSCREEN (Screen Painter)
" Nom : SUB_AREA

PROCESS BEFORE OUTPUT.
  CALL SUBSCREEN sub_area INCLUDING sy-repid '0110'.

PROCESS AFTER INPUT.
  CALL SUBSCREEN sub_area.
```

**Sous-écran (0110) :**
```abap
" Type écran : Subscreen
" Contient ses propres champs et Flow Logic

PROCESS BEFORE OUTPUT.
  MODULE init_subscreen.

PROCESS AFTER INPUT.
  MODULE process_subscreen.
```

---

## Table Control

### Affichage table interne

**Layout :**
1. Insérer Table Control (Screen Painter)
2. Nom : TC_MARA
3. Définir colonnes (noms = champs structure)

**Flow Logic :**
```abap
PROCESS BEFORE OUTPUT.
  " Je boucle sur la table control
  LOOP AT gt_mara INTO gs_mara
       WITH CONTROL tc_mara
       CURSOR tc_mara-current_line.
    " Je passe les données de la ligne courante
    MODULE fill_tc_mara.
  ENDLOOP.

PROCESS AFTER INPUT.
  " Je récupère les modifications
  LOOP AT gt_mara.
    " Je marque la ligne pour modification
    MODULE modify_tc_mara.
  ENDLOOP.
```

**Modules associés :**
```abap
MODULE fill_tc_mara OUTPUT.
  " Je remplis les champs de la ligne
  " Les variables écran sont automatiquement remplies
  " depuis gs_mara (correspondance noms)
ENDMODULE.

MODULE modify_tc_mara INPUT.
  " Je récupère les modifications utilisateur
  " gs_mara est automatiquement mis à jour
  " Je modifie la table interne
  MODIFY gt_mara FROM gs_mara INDEX sy-tabix.
ENDMODULE.
```

---

## Bonnes pratiques

### Organisation code

```abap
*&---------------------------------------------------------------------*
*& Report Z_EXEMPLE_DYNPRO
*&---------------------------------------------------------------------*
REPORT z_exemple_dynpro.

" ======================================================================
" DÉCLARATIONS GLOBALES
" ======================================================================
TABLES: mara.  " Si utilisation de champs TABLES pour écran de sél.

" Variables écran
DATA: gv_matnr TYPE matnr,
      gv_maktx TYPE maktx.

" Tables internes
DATA: gt_mara TYPE TABLE OF mara.

" ======================================================================
" START-OF-SELECTION
" ======================================================================
START-OF-SELECTION.
  " J'appelle mon écran principal
  CALL SCREEN 0100.

" ======================================================================
" MODULES PBO
" ======================================================================
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'TITLE_0100'.
ENDMODULE.

MODULE init_data OUTPUT.
  IF sy-ucomm IS INITIAL.
    PERFORM init_screen_data.
  ENDIF.
ENDMODULE.

" ======================================================================
" MODULES PAI
" ======================================================================
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'SAVE'.
      PERFORM save_data.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
  CLEAR sy-ucomm.
ENDMODULE.

MODULE check_data INPUT.
  PERFORM validate_input_data.
ENDMODULE.

" ======================================================================
" FORMES
" ======================================================================
FORM init_screen_data.
  " Je charge les données initiales
ENDFORM.

FORM save_data.
  " Je sauvegarde les données
ENDFORM.

FORM validate_input_data.
  " Je valide les saisies
ENDFORM.
```

### Règles essentielles

**Variables :**
- Toujours globales pour champs écran
- Noms identiques Screen Painter / ABAP
- Préfixe `gv_` pour variables écran globales

**PBO :**
- Initialisation données une seule fois (IF sy-ucomm IS INITIAL)
- SET PF-STATUS et TITLEBAR toujours en premier
- Modifications dynamiques (LOOP AT SCREEN) après

**PAI :**
- CASE sy-ucomm pour dispatcher actions
- CLEAR sy-ucomm en fin de traitement
- Validation avant traitement métier
- Messages d'erreur bloquants (TYPE 'E')

**Performance :**
- Éviter SELECT dans PBO/PAI
- Charger données une fois, stocker en global
- Table Control : limiter lignes affichées

**Maintenance :**
- Un module = une responsabilité
- Commentaires en français, première personne
- Factoriser logique en FORMs
- Nommage cohérent (status_XXXX, user_command_XXXX)

---

## Erreurs fréquentes

### 1. Variable écran non reconnue
```abap
" ❌ ERREUR : Nom différent
DATA: v_material TYPE matnr.  " Écran : GV_MATNR

" ✅ CORRECT
DATA: gv_matnr TYPE matnr.    " Écran : GV_MATNR
```

### 2. sy-ucomm non vidé
```abap
" ❌ ERREUR : Retraitement en boucle
MODULE user_command_0100 INPUT.
  IF sy-ucomm = 'SAVE'.
    PERFORM save_data.
  ENDIF.
  " Manque CLEAR sy-ucomm
ENDMODULE.

" ✅ CORRECT
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'SAVE'.
      PERFORM save_data.
  ENDCASE.
  CLEAR sy-ucomm.  " J'évite le retraitement
ENDMODULE.
```

### 3. Initialisation à chaque PBO
```abap
" ❌ ERREUR : Perd les saisies utilisateur
MODULE init_data OUTPUT.
  gv_matnr = 'MAT001'.  " Écrase à chaque affichage
ENDMODULE.

" ✅ CORRECT
MODULE init_data OUTPUT.
  IF sy-ucomm IS INITIAL.  " Uniquement première fois
    gv_matnr = 'MAT001'.
  ENDIF.
ENDMODULE.
```

### 4. SELECT dans PBO
```abap
" ❌ ERREUR : Requête à chaque affichage
MODULE init_data OUTPUT.
  SELECT * FROM mara INTO TABLE gt_mara.  " Coûteux
ENDMODULE.

" ✅ CORRECT
MODULE init_data OUTPUT.
  IF sy-ucomm IS INITIAL AND gt_mara IS INITIAL.
    SELECT matnr maktx FROM mara
      INTO TABLE gt_mara
      UP TO 100 ROWS.  " Je limite le volume
  ENDIF.
ENDMODULE.
```

---

## Checklist validation

- [ ] Écran créé avec numérotation standard
- [ ] Layout propre et ergonomique
- [ ] Variables globales = noms champs écran
- [ ] GUI Status défini (menu, toolbar, F-keys)
- [ ] PBO : SET PF-STATUS + TITLEBAR
- [ ] PBO : Initialisation avec IF sy-ucomm IS INITIAL
- [ ] PAI : CASE sy-ucomm pour actions
- [ ] PAI : CLEAR sy-ucomm en fin
- [ ] Validation données avec messages appropriés
- [ ] Pas de SELECT dans boucles
- [ ] Commentaires en français première personne
- [ ] Test complet toutes actions utilisateur
- [ ] Code ATC compliant

---

## Ressources

→ Transaction SE51 : Screen Painter  
→ Transaction SE41 : Menu Painter  
→ Clean ABAP : SAP GitHub

---

**Conseil Tech Lead :** Les Dynpro sont l'ancienne génération UI (remplacés par Fiori), mais restent omniprésents en maintenance. Maîtrise-les, mais privilégie les approches modernes (SAPUI5/Fiori) pour nouveaux développements. En ESN, tu dois savoir faire les deux.
