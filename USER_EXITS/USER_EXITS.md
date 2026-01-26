# Guide User Exits ABAP

## Introduction

Les **User Exits** sont des points d'extension fournis par SAP permettant de personnaliser le comportement standard sans modifier le code SAP d'origine. Ils garantissent la maintenabilité lors des upgrades.

---

## Typologie des User Exits

### 1. User Exits classiques (Function Module)
- **Type** : `CALL CUSTOMER-FUNCTION`
- **Localisation** : Module pools, Function modules SAP
- **Implémentation** : Transaction `CMOD` / `SMOD`
- **Format** : `EXIT_<programme>_<numéro>`

### 2. Customer Exits (Enhancement SAP)
- **Type** : Points d'amélioration structurés
- **Composants** :
  - Menu exits
  - Screen exits
  - Function module exits
  - Field exits

### 3. BAdI (Business Add-Ins)
- **Type** : Technologie OOP moderne
- **Implémentation** : Transaction `SE18` / `SE19`
- **Avantages** : Multi-implémentations possibles

### 4. Implicit/Explicit Enhancements
- **Type** : Points d'amélioration implicites/explicites
- **Outil** : Enhancement Framework
- **Transaction** : `SE80` → Edit → Enhancement Operations

---

## Transactions Clés

| Transaction | Utilité |
|-------------|---------|
| `SMOD` | Rechercher les User Exits disponibles |
| `CMOD` | Créer/Modifier des projets d'amélioration |
| `SE18` | Définition BAdI |
| `SE19` | Implémentation BAdI |
| `SE80` | Enhancement Spots/Sections |

---

## Processus d'Implémentation

### Étape 1 : Identifier le User Exit

#### Méthode 1 : Via SMOD

1. Lancer la transaction **SMOD**
2. Saisir le programme SAP concerné (ex: `SAPMM06E` pour ME21N)
3. Appuyer sur **F4** pour lister tous les exits disponibles
4. Analyser la documentation de chaque exit

#### Méthode 2 : Via Code Source

1. Ouvrir **SE38** avec le programme SAP concerné
2. Rechercher le pattern `CALL CUSTOMER-FUNCTION`
3. Noter le nom du Function Module `EXIT_*`

#### Méthode 3 : Debugger

1. Activer le debugger sur la transaction concernée
2. Chercher les appels `CALL CUSTOMER-FUNCTION`
3. Placer un breakpoint conditionnel si nécessaire

---

### Étape 2 : Créer le Projet d'Amélioration

#### Transaction CMOD

**1. Créer le projet**
   - Lancer **CMOD** → Créer
   - Nom projet : `Z_<MODULE>_<FONCTION>`
   - Description : Personnalisation <Transaction> - <Objectif>

**2. Assigner les composants**
   - Onglet **Composants** → Insérer
   - Sélectionner le User Exit identifié
   - Sauvegarder

**3. Activer le projet**
   - Statut : **Actif**
   - Sauvegarder

---

### Étape 3 : Implémenter le Code

#### Structure Standard

```abap
" Module Function EXIT_<PROG>_<NUM>
FUNCTION z_exit_implementation.
*"----------------------------------------------------------------------
*" User Exit : EXIT_SAPMM06E_012
*" Transaction : ME21N
*" Objectif : Validation prix unitaire commande
*" Auteur : [Nom]
*" Date : [JJ/MM/AAAA]
*"----------------------------------------------------------------------

  " Je déclare toutes mes variables locales en tête
  DATA: lv_max_price TYPE netpr,
        lv_message    TYPE string.

  " Je récupère le prix maximum autorisé depuis une table custom
  SELECT SINGLE max_price
    FROM ztab_price_limit
    INTO lv_max_price
    WHERE matnr = ekpo-matnr.

  " Je vérifie si le prix dépasse la limite
  IF sy-subrc = 0 AND ekpo-netpr > lv_max_price.
    
    " Je construis le message d'erreur
    MESSAGE e001(z_msg_class) WITH ekpo-matnr lv_max_price INTO lv_message.
    
    " Je lève l'exception pour bloquer la sauvegarde
    RAISE price_exceeds_limit.
    
  ENDIF.

ENDFUNCTION.
```

---

## Bonnes Pratiques

### Code

```abap
" ✅ BON
DATA: lt_data TYPE TABLE OF mara,
      ls_data TYPE mara.

SELECT matnr, mtart
  FROM mara
  INTO TABLE lt_data
  WHERE matnr IN s_matnr.

LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
  " Je traite chaque matière
ENDLOOP.

" ❌ MAUVAIS
SELECT * FROM mara.
  " Traitement direct sans structure
ENDSELECT.
```

### Performance

```abap
" ✅ Éviter SELECT dans LOOP
SELECT matnr, matkl
  FROM mara
  INTO TABLE @DATA(lt_mara)
  FOR ALL ENTRIES IN @lt_input
  WHERE matnr = @lt_input-matnr.

" ❌ SELECT imbriqué
LOOP AT lt_input INTO DATA(ls_input).
  SELECT SINGLE matkl FROM mara INTO lv_matkl
    WHERE matnr = ls_input-matnr.
ENDLOOP.
```

### Gestion Erreurs

```abap
" ✅ Exceptions class-based
TRY.
    " Je tente l'opération critique
    DATA(lv_result) = calculate_price( iv_matnr = lv_matnr ).
    
  CATCH cx_calculation_error INTO DATA(lo_error).
    " Je logue l'erreur de manière appropriée
    MESSAGE lo_error->get_text( ) TYPE 'E'.
    
ENDTRY.

" ❌ Pas de gestion d'erreur
DATA(lv_result) = calculate_price( iv_matnr = lv_matnr ).
```

---

## User Exits Fréquents

### MM - Achats

| Exit | Transaction | Usage |
|------|-------------|-------|
| `EXIT_SAPMM06E_012` | ME21N/ME22N | Validation commande |
| `EXIT_SAPMM06E_013` | ME21N/ME22N | Modification données commande |
| `EXIT_SAPLMEKO_001` | ME2* | Modification sélection |

### SD - Ventes

| Exit | Transaction | Usage |
|------|-------------|-------|
| `EXIT_SAPLV09A_001` | VA01/VA02 | Validation commande client |
| `EXIT_SAPLV09A_002` | VA01/VA02 | Modification prix |
| `EXIT_SAPFV45K_001` | VL01N | Validation livraison |

### FI - Comptabilité

| Exit | Transaction | Usage |
|------|-------------|-------|
| `EXIT_SAPFB04E_001` | FB01 | Validation pièce comptable |
| `EXIT_SAPFB09E_001` | FB60/FB65 | Validation facture |

---

## Recherche User Exit par Transaction

### Méthode Programmatique

```abap
REPORT z_find_user_exits.

" Je récupère les user exits d'une transaction
PARAMETERS: p_tcode TYPE tcode OBLIGATORY.

DATA: lt_exits TYPE TABLE OF modsap,
      ls_exit  TYPE modsap.

" Je cherche via la table système
SELECT *
  FROM modsap
  INTO TABLE lt_exits
  WHERE member IN (
    SELECT obj_name
      FROM tadir
      WHERE pgmid = 'R3TR'
        AND object = 'PROG'
        AND devclass LIKE 'V%'
  ).

" J'affiche les résultats
LOOP AT lt_exits INTO ls_exit.
  WRITE: / ls_exit-name, ls_exit-member.
ENDLOOP.
```

---

## Validation & Tests

### Checklist

- [ ] User Exit documenté (objectif, inputs/outputs)
- [ ] Code conforme Clean ABAP
- [ ] Pas de SELECT dans LOOP
- [ ] Gestion erreurs via exceptions
- [ ] Messages via classe SE91
- [ ] Pas de WRITE ni BREAK-POINT
- [ ] Tests unitaires créés
- [ ] Tests en environnement DEV validés
- [ ] Fiche de tests complétée
- [ ] Peer review effectué
- [ ] Transport créé et documenté

### Script de Test

```abap
" Template test unitaire User Exit
CLASS ltc_test_user_exit DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      test_price_validation FOR TESTING,
      setup,
      teardown.

    DATA: mo_cut TYPE REF TO zcl_exit_handler.

ENDCLASS.

CLASS ltc_test_user_exit IMPLEMENTATION.

  METHOD setup.
    " Je prépare les données de test
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD test_price_validation.
    " Je teste la validation du prix
    TRY.
        mo_cut->validate_price( iv_price = '1000.00' ).
        cl_abap_unit_assert=>fail( 'Exception attendue' ).
      CATCH cx_price_error.
        " Je confirme que l'exception est bien levée
        cl_abap_unit_assert=>assert_true( abap_true ).
    ENDTRY.
  ENDMETHOD.

  METHOD teardown.
    " Je nettoie après le test
    CLEAR mo_cut.
  ENDMETHOD.

ENDCLASS.
```

---

## Documentation Projet

### Template CMOD

```
╔════════════════════════════════════════════════════════════════╗
║ PROJET D'AMÉLIORATION : Z_MM_PO_VALIDATION                    ║
╠════════════════════════════════════════════════════════════════╣
║ Module        : MM - Achats                                    ║
║ Transaction   : ME21N, ME22N                                   ║
║ User Exit     : EXIT_SAPMM06E_012                             ║
║ Objectif      : Validation prix unitaire selon limites métier ║
║ Auteur        : [Nom]                                         ║
║ Date création : [JJ/MM/AAAA]                                  ║
║ Transport     : [DEVK######]                                  ║
╠════════════════════════════════════════════════════════════════╣
║ DESCRIPTION                                                    ║
║ Bloque la création de commande si le prix unitaire dépasse    ║
║ la limite définie dans la table ZTAB_PRICE_LIMIT              ║
╠════════════════════════════════════════════════════════════════╣
║ IMPACTS                                                        ║
║ - Table custom : ZTAB_PRICE_LIMIT                             ║
║ - Classe message : Z_MSG_MM                                    ║
║ - Rôles affectés : Acheteurs                                  ║
╠════════════════════════════════════════════════════════════════╣
║ HISTORIQUE                                                     ║
║ [Date] - [Auteur] - [Modification]                           ║
╚════════════════════════════════════════════════════════════════╝
```

---

## Dépannage

### Exit non déclenché

1. Vérifier que le projet CMOD est **ACTIF**
2. Vérifier l'ordre de transport
3. Debugger : poser un breakpoint dans l'exit
4. Contrôler les conditions d'appel dans le code SAP standard

### Performance dégradée

**Optimisations recommandées :**
- Limiter les `SELECT` au strict nécessaire
- Utiliser `FOR ALL ENTRIES` au lieu de `SELECT` imbriqués
- Privilégier les tables `HASHED` pour les recherches
- Déplacer les traitements lourds hors des boucles

### Conflits entre projets

1. Lister tous les projets actifs via **CMOD**
2. Identifier les doublons d'exit
3. Fusionner dans un seul projet si possible
4. Sinon, gérer la priorité d'exécution

---

## Ressources

- **SAP Help** : [User Exits Documentation](https://help.sap.com)
- **Transaction** : `SMOD` → Documentation de chaque exit
- **GitHub** : ABAP Socle – Formation 2025 / Section Enhancements
- **Clean ABAP** : Standards de développement

---

**→ Voir également** : [03 - PROGRAM.md](03_-___PROGRAM.md) | [OOP Best Practices](GitHub)
