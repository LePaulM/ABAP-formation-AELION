# TVARV - Variables de Sélection Globales

## 📋 Table des Matières
1. [Qu'est-ce que TVARV ?](#quest-ce-que-tvarv-)
2. [Types de Variables](#types-de-variables)
3. [Création d'une Variable](#création-dune-variable)
4. [Lecture dans un Programme](#lecture-dans-un-programme)
5. [Utilisation avec SELECTION-SCREEN](#utilisation-avec-selection-screen)
6. [Cas d'Usage Réels](#cas-dusage-réels)
7. [Bonnes Pratiques](#bonnes-pratiques)
8. [Alternatives](#alternatives)

---

## Qu'est-ce que TVARV ?

TVARV est une **table système SAP** qui stocke des **variables de sélection globales**.

### Utilité
- Stocker des **valeurs de configuration** modifiables sans transport
- Partager des paramètres entre plusieurs programmes
- Permettre aux Key Users de modifier des valeurs techniques

### Exemples d'Usage
- Dates de clôture comptable
- Liste de sociétés autorisées
- Paramètres techniques (devise par défaut, usine, etc.)
- Plages de valeurs pour traitements batch

---

## Types de Variables

TVARV gère **2 types** de variables :

| Type | Description | Utilisation |
|------|-------------|-------------|
| **P** | Paramètre | 1 seule valeur |
| **S** | Sélection | Plage de valeurs (LOW-HIGH) avec options |

### Structure Table TVARV
```abap
" Champs principaux :
" NAME  : Nom de la variable (25 caractères max)
" TYPE  : P ou S
" NUMB  : Numéro de ligne (pour type S)
" SIGN  : I (Include) ou E (Exclude)
" OPTI  : EQ, BT, CP, etc. (pour type S)
" LOW   : Valeur basse
" HIGH  : Valeur haute (pour type S avec option BT)
```

---

## Création d'une Variable

### Méthode 1 : Transaction STVARV (Recommandée)

#### Étape 1 : Accéder à STVARV
```
Transaction : STVARV
```

#### Étape 2 : Créer une Variable Type P
```
Clic sur "Nouvelles entrées"

NAME     : Z_WAERS_DEFAULT
TYPE     : P
LOW      : EUR
HIGH     : (vide)
SIGN     : (vide)
OPTI     : (vide)
Description : Devise par défaut
```

#### Étape 3 : Créer une Variable Type S
```
Clic sur "Nouvelles entrées"

NAME     : Z_BUKRS_AUTORISE
TYPE     : S
NUMB     : 0001
LOW      : 1000
HIGH     : 1999
SIGN     : I
OPTI     : BT
Description : Sociétés autorisées

" Ajouter une autre ligne :
NUMB     : 0002
LOW      : 3000
HIGH     : (vide)
SIGN     : I
OPTI     : EQ
```

### Méthode 2 : SM30 sur TVARV
```
Transaction : SM30
Table       : TVARV
Action      : Maintain
```

---

## Lecture dans un Programme

### 1. Lire une Variable Type P (Paramètre Simple)
```abap
REPORT z_demo_tvarv_p.

DATA: lv_waers TYPE waers.

START-OF-SELECTION.

  " Je lis la devise par défaut depuis TVARV
  SELECT SINGLE low
    FROM tvarv
    INTO @lv_waers
    WHERE name = 'Z_WAERS_DEFAULT'
      AND type = 'P'.

  IF sy-subrc = 0.
    WRITE: / 'Devise configurée :', lv_waers.
  ELSE.
    WRITE: / 'Variable Z_WAERS_DEFAULT non trouvée'.
  ENDIF.
```

### 2. Lire une Variable Type S (Sélection)
```abap
REPORT z_demo_tvarv_s.

DATA: lt_tvarv TYPE TABLE OF tvarv,
      lt_bukrs TYPE TABLE OF t001.

START-OF-SELECTION.

  " Je lis toutes les lignes de la variable de sélection
  SELECT *
    FROM tvarv
    INTO TABLE @lt_tvarv
    WHERE name = 'Z_BUKRS_AUTORISE'
      AND type = 'S'.

  IF sy-subrc = 0.
    " Je récupère les sociétés correspondantes
    SELECT bukrs, butxt
      FROM t001
      INTO TABLE @lt_bukrs
      WHERE bukrs IN @lt_tvarv.

    " J'affiche les résultats
    LOOP AT lt_bukrs INTO DATA(ls_bukrs).
      WRITE: / ls_bukrs-bukrs, ls_bukrs-butxt.
    ENDLOOP.
  ELSE.
    WRITE: / 'Variable Z_BUKRS_AUTORISE non trouvée'.
  ENDIF.
```

### 3. Utilisation avec FOR ALL ENTRIES
```abap
REPORT z_demo_tvarv_fae.

DATA: lt_tvarv TYPE TABLE OF tvarv,
      lt_bkpf  TYPE TABLE OF bkpf.

PARAMETERS: p_gjahr TYPE gjahr DEFAULT sy-datum(4).

START-OF-SELECTION.

  " Je lis la variable de sélection
  SELECT *
    FROM tvarv
    INTO TABLE @lt_tvarv
    WHERE name = 'Z_BUKRS_AUTORISE'
      AND type = 'S'.

  CHECK lt_tvarv IS NOT INITIAL.

  " Je sélectionne les documents comptables des sociétés autorisées
  SELECT *
    FROM bkpf
    INTO TABLE @lt_bkpf
    FOR ALL ENTRIES IN @lt_tvarv
    WHERE bukrs = @lt_tvarv-low
      AND gjahr = @p_gjahr.

  IF sy-subrc = 0.
    WRITE: / sy-dbcnt, 'documents trouvés'.
  ENDIF.
```

---

## Utilisation avec SELECTION-SCREEN

### 1. Pré-remplir un PARAMETERS
```abap
REPORT z_demo_tvarv_param.

PARAMETERS: p_waers TYPE waers.

INITIALIZATION.
  " Je charge la valeur par défaut depuis TVARV
  SELECT SINGLE low
    FROM tvarv
    INTO @p_waers
    WHERE name = 'Z_WAERS_DEFAULT'
      AND type = 'P'.

START-OF-SELECTION.
  WRITE: / 'Devise sélectionnée :', p_waers.
```

### 2. Pré-remplir un SELECT-OPTIONS
```abap
REPORT z_demo_tvarv_selopt.

TABLES: t001.
SELECT-OPTIONS: s_bukrs FOR t001-bukrs.

INITIALIZATION.
  DATA: lt_tvarv TYPE TABLE OF tvarv.

  " Je charge les valeurs de sélection depuis TVARV
  SELECT *
    FROM tvarv
    INTO TABLE @lt_tvarv
    WHERE name = 'Z_BUKRS_AUTORISE'
      AND type = 'S'
    ORDER BY numb.

  " Je remplis le SELECT-OPTIONS
  LOOP AT lt_tvarv INTO DATA(ls_tvarv).
    APPEND VALUE #(
      sign   = ls_tvarv-sign
      option = ls_tvarv-opti
      low    = ls_tvarv-low
      high   = ls_tvarv-high
    ) TO s_bukrs.
  ENDLOOP.

START-OF-SELECTION.
  " Les valeurs sont déjà pré-remplies dans s_bukrs
  SELECT bukrs, butxt
    FROM t001
    INTO TABLE @DATA(lt_bukrs)
    WHERE bukrs IN @s_bukrs.

  LOOP AT lt_bukrs INTO DATA(ls_bukrs).
    WRITE: / ls_bukrs-bukrs, ls_bukrs-butxt.
  ENDLOOP.
```

### 3. Pré-remplir avec Possibilité de Modification
```abap
REPORT z_demo_tvarv_modif.

TABLES: bkpf.
SELECT-OPTIONS: s_bukrs FOR bkpf-bukrs,
                s_budat FOR bkpf-budat.

INITIALIZATION.
  DATA: lt_tvarv TYPE TABLE OF tvarv.

  " Je charge les sociétés autorisées
  SELECT *
    FROM tvarv
    INTO TABLE @lt_tvarv
    WHERE name = 'Z_BUKRS_AUTORISE'
      AND type = 'S'.

  LOOP AT lt_tvarv INTO DATA(ls_tvarv).
    APPEND VALUE #(
      sign   = ls_tvarv-sign
      option = ls_tvarv-opti
      low    = ls_tvarv-low
      high   = ls_tvarv-high
    ) TO s_bukrs.
  ENDLOOP.

  " Je charge la date de clôture
  SELECT SINGLE low
    FROM tvarv
    INTO @DATA(lv_close_date)
    WHERE name = 'Z_CLOSE_DATE'
      AND type = 'P'.

  IF sy-subrc = 0.
    " Je pré-remplis avec une plage jusqu'à la date de clôture
    s_budat = VALUE #(
      sign   = 'I'
      option = 'BT'
      low    = '20240101'
      high   = lv_close_date
    ).
    APPEND s_budat.
  ENDIF.

START-OF-SELECTION.
  " L'utilisateur peut modifier les valeurs avant exécution
  SELECT bukrs, belnr, budat
    FROM bkpf
    INTO TABLE @DATA(lt_bkpf)
    WHERE bukrs IN @s_bukrs
      AND budat IN @s_budat.

  WRITE: / sy-dbcnt, 'documents trouvés'.
```

---

## Cas d'Usage Réels

### Cas 1 : Date de Clôture Comptable

#### Configuration TVARV
```
NAME     : Z_CLOSE_DATE
TYPE     : P
LOW      : 20241231
```

#### Code Programme
```abap
REPORT z_compta_cloture.

PARAMETERS: p_budat TYPE budat.

AT SELECTION-SCREEN ON p_budat.
  DATA: lv_close_date TYPE datum.

  " Je vérifie que la date n'est pas après la clôture
  SELECT SINGLE low
    FROM tvarv
    INTO @lv_close_date
    WHERE name = 'Z_CLOSE_DATE'
      AND type = 'P'.

  IF sy-subrc = 0 AND p_budat > lv_close_date.
    MESSAGE 'Période comptable clôturée' TYPE 'E'.
  ENDIF.

START-OF-SELECTION.
  WRITE: / 'Traitement autorisé pour la date :', p_budat.
```

### Cas 2 : Liste de Sociétés Autorisées pour un Traitement

#### Configuration TVARV
```
NAME     : Z_BUKRS_INTERFACE
TYPE     : S
NUMB     : 0001
SIGN     : I
OPTI     : EQ
LOW      : 1000

NUMB     : 0002
SIGN     : I
OPTI     : EQ
LOW      : 2000
```

#### Code Programme
```abap
REPORT z_interface_compta.

DATA: lt_tvarv     TYPE TABLE OF tvarv,
      lt_documents TYPE TABLE OF bkpf.

PARAMETERS: p_gjahr TYPE gjahr DEFAULT sy-datum(4).

START-OF-SELECTION.

  " Je charge les sociétés autorisées pour l'interface
  SELECT *
    FROM tvarv
    INTO TABLE @lt_tvarv
    WHERE name = 'Z_BUKRS_INTERFACE'
      AND type = 'S'.

  CHECK lt_tvarv IS NOT INITIAL.

  " Je sélectionne uniquement les documents de ces sociétés
  SELECT *
    FROM bkpf
    INTO TABLE @lt_documents
    FOR ALL ENTRIES IN @lt_tvarv
    WHERE bukrs = @lt_tvarv-low
      AND gjahr = @p_gjahr.

  IF sy-subrc = 0.
    WRITE: / 'Traitement de', sy-dbcnt, 'documents'.
    " ... suite du traitement
  ELSE.
    WRITE: / 'Aucun document à traiter'.
  ENDIF.
```

### Cas 3 : Paramètres Multiples pour un Job Batch

#### Configuration TVARV
```
" Devise de référence
NAME     : Z_REF_CURRENCY
TYPE     : P
LOW      : EUR

" Taux de conversion min
NAME     : Z_MIN_RATE
TYPE     : P
LOW      : 0.01

" Sociétés à exclure
NAME     : Z_BUKRS_EXCLUDE
TYPE     : S
NUMB     : 0001
SIGN     : E
OPTI     : EQ
LOW      : 9999
```

#### Code Programme
```abap
REPORT z_batch_conversion.

DATA: lv_ref_curr  TYPE waers,
      lv_min_rate  TYPE p DECIMALS 5,
      lt_bukrs_excl TYPE TABLE OF tvarv.

START-OF-SELECTION.

  " Je charge tous les paramètres depuis TVARV
  SELECT SINGLE low
    FROM tvarv
    INTO @lv_ref_curr
    WHERE name = 'Z_REF_CURRENCY'
      AND type = 'P'.

  SELECT SINGLE low
    FROM tvarv
    INTO @lv_min_rate
    WHERE name = 'Z_MIN_RATE'
      AND type = 'P'.

  SELECT *
    FROM tvarv
    INTO TABLE @lt_bukrs_excl
    WHERE name = 'Z_BUKRS_EXCLUDE'
      AND type = 'S'.

  " J'utilise ces paramètres dans mon traitement
  WRITE: / 'Devise de référence :', lv_ref_curr.
  WRITE: / 'Taux minimum      :', lv_min_rate.
  WRITE: / 'Sociétés exclues  :', lines( lt_bukrs_excl ).
```

### Cas 4 : Contrôle de Plage de Valeurs

#### Configuration TVARV
```
NAME     : Z_MONTANT_MAX
TYPE     : P
LOW      : 1000000
```

#### Code Programme
```abap
REPORT z_controle_montant.

PARAMETERS: p_dmbtr TYPE dmbtr.

AT SELECTION-SCREEN ON p_dmbtr.
  DATA: lv_max_amount TYPE dmbtr.

  " Je vérifie que le montant ne dépasse pas le maximum configuré
  SELECT SINGLE low
    FROM tvarv
    INTO @lv_max_amount
    WHERE name = 'Z_MONTANT_MAX'
      AND type = 'P'.

  IF sy-subrc = 0 AND p_dmbtr > lv_max_amount.
    MESSAGE e001(z_msg) WITH 'Montant maximum autorisé :' lv_max_amount.
  ENDIF.

START-OF-SELECTION.
  WRITE: / 'Montant validé :', p_dmbtr.
```

---

## Bonnes Pratiques

### ✅ À FAIRE

#### 1. Nommage
```abap
" Préfixe namespace obligatoire
Z_WAERS_DEFAULT     " ✅ Bon
Y_CLOSE_DATE        " ✅ Bon (namespace client Y)
WAERS_DEFAULT       " ❌ Mauvais (pas de namespace)
```

#### 2. Documentation
```
Toujours documenter dans la fiche technique :
- Nom de la variable
- Type (P ou S)
- Valeur par défaut
- Qui peut la modifier
- Impact sur les programmes
```

#### 3. Gestion des Erreurs
```abap
" Toujours vérifier sy-subrc
SELECT SINGLE low
  FROM tvarv
  INTO @DATA(lv_value)
  WHERE name = 'Z_MY_VAR'
    AND type = 'P'.

IF sy-subrc <> 0.
  " Je définis une valeur par défaut OU je lève une erreur
  MESSAGE 'Variable Z_MY_VAR non configurée' TYPE 'E'.
ENDIF.
```

#### 4. Type Approprié
```abap
" Type P : 1 seule valeur fixe
Z_CURRENCY_DEFAULT → Type P

" Type S : plages, listes, exclusions
Z_BUKRS_AUTHORIZED → Type S (plusieurs sociétés)
Z_DATE_RANGE       → Type S (du...au)
```

#### 5. Valeurs par Défaut
```abap
" Prévoir toujours un fallback si TVARV vide
SELECT SINGLE low
  FROM tvarv
  INTO @DATA(lv_waers)
  WHERE name = 'Z_WAERS_DEFAULT'
    AND type = 'P'.

IF sy-subrc <> 0.
  lv_waers = 'EUR'. " Valeur par défaut en dur
ENDIF.
```

### ❌ À ÉVITER

#### 1. Données Métier
```abap
" ❌ Ne PAS utiliser TVARV pour stocker des données métier
" Exemple : liste de clients, tarifs, stock
" → Utiliser une table Z dédiée
```

#### 2. Données Sensibles
```abap
" ❌ Ne PAS stocker de mots de passe, clés API, etc.
" → Utiliser des tables sécurisées avec autorisation
```

#### 3. Trop de Variables
```abap
" ❌ Plus de 10 variables pour 1 programme
" → Créer une table de paramétrage Z
```

#### 4. Valeurs en Dur dans le Code
```abap
" ❌ Mauvais
IF p_bukrs = '1000'.

" ✅ Bon : utiliser TVARV
SELECT SINGLE low
  FROM tvarv
  INTO @DATA(lv_bukrs_ref)
  WHERE name = 'Z_BUKRS_REFERENCE'
    AND type = 'P'.

IF p_bukrs = lv_bukrs_ref.
```

---

## Alternatives

### Quand NE PAS utiliser TVARV

| Besoin | Alternative | Raison |
|--------|-------------|--------|
| > 10 paramètres | Table Z custom | Plus structuré |
| Historisation | Table Z avec dates | TVARV non historisé |
| Données multilingues | Table Z + textes | TVARV mono-langue |
| Données métier | Table Z dédiée | TVARV = config technique |
| Validations complexes | Classe de paramétrage | Logique centralisée |

### Exemple : Table de Paramétrage Custom
```abap
" Table ZPARAM (SE11)
" PARAM_ID  : CHAR10  (Clé)
" PARAM_CAT : CHAR4   (Catégorie)
" VALUE1    : CHAR50  (Valeur 1)
" VALUE2    : CHAR50  (Valeur 2)
" DATE_FROM : DATUM   (Valide du)
" DATE_TO   : DATUM   (Valide au)
" ACTIF     : CHAR1   (X ou '')

" Classe de gestion
CLASS zcl_param_manager DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS get_param
      IMPORTING iv_param_id    TYPE zparam-param_id
      RETURNING VALUE(rv_value) TYPE zparam-value1.
ENDCLASS.

CLASS zcl_param_manager IMPLEMENTATION.
  METHOD get_param.
    SELECT SINGLE value1
      FROM zparam
      INTO @rv_value
      WHERE param_id = @iv_param_id
        AND actif = 'X'
        AND date_from <= @sy-datum
        AND date_to >= @sy-datum.
  ENDMETHOD.
ENDCLASS.

" Utilisation
DATA(lv_currency) = zcl_param_manager=>get_param( 'CURRENCY' ).
```

---

## Checklist Avant Production

- [ ] Nom de variable avec namespace Z_ ou Y_
- [ ] Type P ou S approprié
- [ ] Documentation dans fiche technique
- [ ] Gestion des erreurs (sy-subrc)
- [ ] Valeur par défaut en cas d'absence
- [ ] Tests avec variable existante
- [ ] Tests avec variable manquante
- [ ] Validation par Key User
- [ ] Note de mise en production (création variable)

---

## Ressources

- **Transaction** : STVARV (création/modification)
- **Table** : TVARV (lecture en ABAP)
- **Transaction alternative** : SM30 (maintenance table)
- **Référence** : [06_-___DBTABLES.md](./06_-___DBTABLES.md) pour création tables custom

---

## Conseil Tech Lead

> TVARV est **parfait pour 5-10 paramètres techniques simples**.  
> Au-delà, ou si besoin d'historisation, multilingue, validations complexes →  
> **Créer une table Z de paramétrage + classe de gestion.**  
>  
> En mission ESN, **toujours documenter** où sont les variables TVARV  
> et qui est responsable de leur maintenance (Key User, Admin SAP).

---

**Version** : 1.0  
**Dernière MAJ** : Janvier 2025  
**Auteur** : Formation ABAP Intensive
