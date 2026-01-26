# IDOC - INTERFACE DOCUMENT

## 1. DÉFINITION & CONCEPT

### Qu'est-ce qu'un IDOC ?

Un **IDOC (Intermediate Document)** est un conteneur standardisé SAP pour échanger des données entre systèmes (SAP ↔ SAP ou SAP ↔ non-SAP).

**Structure :**
- **Header** : informations de contrôle (expéditeur, destinataire, type de message)
- **Segments** : données métier structurées hiérarchiquement
- **Status** : traçabilité complète du traitement

**Analogie :** Un IDOC = Une lettre recommandée avec accusé de réception
- L'enveloppe contient les infos d'acheminement
- Le contenu est organisé en sections structurées
- Le suivi garantit la traçabilité de bout en bout

---

### Types d'IDOC

| Type | Description | Direction |
|------|-------------|-----------|
| **INBOUND** | Réception de données externes vers SAP | Entrant ← |
| **OUTBOUND** | Envoi de données SAP vers l'externe | Sortant → |

**Exemples courants :**
- `MATMAS` : Données Article (Material Master)
- `DEBMAS` : Données Client (Customer Master)
- `ORDERS` : Commandes de vente
- `INVOIC` : Factures


---

## 2. STRUCTURE TECHNIQUE

### Architecture 3 couches

L'architecture IDOC repose sur trois composants clés :

| Couche | Rôle | Transaction |
|--------|------|-------------|
| **TYPE IDOC** | Définit la structure des segments et leur hiérarchie | `WE30` |
| **MESSAGE TYPE** | Définit l'objet métier (Client, Article, Commande...) | `WE81` |
| **PROCESS CODE** | Définit le traitement à appliquer à l'IDOC | `WE41` (Inbound) / `WE42` (Outbound) |

---

### Tables principales

**Tables de données :**
- `EDIDC` : Header IDOC (informations de contrôle)
- `EDID2`, `EDID3`, `EDID4` : Segments de données IDOC
- `EDIDS` : Status IDOC (historique de traçabilité)

**Tables de configuration :**
- `EDIFCT` : Mapping Message Type → Type IDOC
- `TBDME` : Mapping Message Type → Process Code
- `TEDE1` : Ports de communication


---

## 3. TRANSACTION WE02 - MONITORING

**WE02** est la console centrale de suivi des IDOC.

### Fonctionnalités principales

- Rechercher les IDOC selon différents critères
- Visualiser le détail complet (header, segments, status)
- Relancer les IDOC en erreur
- Analyser les logs et messages d'erreur
- Tracer l'historique de traitement

### Critères de recherche courants

- Numéro IDOC
- Type IDOC
- Message Type
- Date de création
- Direction (Inbound/Outbound)
- Status (51 = Erreur applicative, 53 = Traité avec succès, etc.)

---

## 4. CRÉATION IDOC OUTBOUND

### Méthode standard : Function Module
```abap
" Je crée un IDOC pour envoyer des données Article

DATA: lt_edidd TYPE TABLE OF edidd,
      ls_edidc TYPE edidc,
      ls_edidd TYPE edidd,
      lv_docnum TYPE edi_docnum.

" 1. Je prépare le header IDOC
ls_edidc-mestyp = 'MATMAS'.     " Message Type
ls_edidc-idoctp = 'MATMAS05'.   " Type IDOC
ls_edidc-rcvprn = 'PARTNER01'.  " Partenaire destinataire
ls_edidc-rcvprt = 'LS'.         " Type partenaire (LS = Logical System)
ls_edidc-sndprn = 'SAPDEV'.     " Système émetteur
ls_edidc-sndprt = 'LS'.

" 2. Je construis les segments de données
" Segment E1MARAM (données générales article)
CLEAR ls_edidd.
ls_edidd-segnam = 'E1MARAM'.
ls_edidd-sdata  = 'MATNR=00000000000001234567MATKL=001'.
APPEND ls_edidd TO lt_edidd.

" Segment E1MAKTM (désignation article)
CLEAR ls_edidd.
ls_edidd-segnam = 'E1MAKTM'.
ls_edidd-sdata  = 'SPRAS=FRMAKTX=Article de test'.
APPEND ls_edidd TO lt_edidd.

" 3. J'appelle le FM de création IDOC
CALL FUNCTION 'MASTER_IDOC_DISTRIBUTE'
  EXPORTING
    master_idoc_control = ls_edidc
  IMPORTING
    communication_idoc_control = ls_edidc
    master_idoc_number = lv_docnum
  TABLES
    communication_idoc_data = lt_edidd
  EXCEPTIONS
    error_in_idoc_control = 1
    error_writing_idoc_status = 2
    error_in_idoc_data = 3
    sending_logical_system_unknown = 4
    OTHERS = 5.

IF sy-subrc = 0.
  " Je valide la création
  COMMIT WORK.
  WRITE: / 'IDOC créé avec succès :', lv_docnum.
ELSE
  " Je gère l'erreur
  ROLLBACK WORK.
  MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.
```

---

## 5. TRAITEMENT IDOC INBOUND

### Création d'un Function Module de traitement
```abap
FUNCTION z_idoc_input_matmas.
*"----------------------------------------------------------------------
*"*"Interface locale :
*"  IMPORTING
*"     VALUE(INPUT_METHOD) LIKE  BDWFAP_PAR-INPUTMETHD
*"     VALUE(MASS_PROCESSING) LIKE  BDWFAP_PAR-MASS_PROC
*"  EXPORTING
*"     VALUE(WORKFLOW_RESULT) LIKE  BDWFAP_PAR-RESULT
*"     VALUE(APPLICATION_VARIABLE) LIKE  BDWFAP_PAR-APPL_VAR
*"     VALUE(IN_UPDATE_TASK) LIKE  BDWFAP_PAR-UPDATETASK
*"     VALUE(CALL_TRANSACTION_DONE) LIKE  BDWFAP_PAR-CALLTRANS
*"  TABLES
*"      IDOC_CONTRL STRUCTURE  EDIDC
*"      IDOC_DATA STRUCTURE  EDIDD
*"      IDOC_STATUS STRUCTURE  BDIDOCSTAT
*"      RETURN_VARIABLES STRUCTURE  BDWFRETVAR
*"      SERIALIZATION_INFO STRUCTURE  BDI_SER
*"----------------------------------------------------------------------

  DATA: lt_mara TYPE TABLE OF mara,
        ls_mara TYPE mara,
        ls_status TYPE bdidocstat.

  " Je boucle sur les IDOC reçus
  LOOP AT idoc_contrl INTO DATA(ls_control).

    " Je récupère les segments de données pour cet IDOC
    LOOP AT idoc_data INTO DATA(ls_data)
                      WHERE docnum = ls_control-docnum.

      CASE ls_data-segnam.
        WHEN 'E1MARAM'.
          " Je traite le segment données article
          " J'extrais les données du segment (parsing)
          ls_mara-matnr = ls_data-sdata+6(18).  " Position 6, longueur 18
          ls_mara-matkl = ls_data-sdata+29(9).

          " Je crée ou modifie l'article en base
          MODIFY mara FROM ls_mara.

        WHEN 'E1MAKTM'.
          " Je traite le segment désignation
          " (logique similaire)

      ENDCASE.

    ENDLOOP.

    " Je prépare le status de succès
    CLEAR ls_status.
    ls_status-docnum = ls_control-docnum.
    ls_status-status = '53'.  " Traité avec succès
    ls_status-msgty  = 'S'.
    ls_status-msgid  = 'Z_IDOC'.
    ls_status-msgno  = '001'.
    ls_status-msgv1  = 'Article créé'.
    APPEND ls_status TO idoc_status.

    " Je valide les modifications
    COMMIT WORK.

  ENDLOOP.

  " Je définis le résultat du traitement
  workflow_result = '0000000000'.  " Succès

ENDFUNCTION.
```

### Configuration du Process Code

**Étapes de configuration :**

1. Transaction `WE41` : Accéder à la configuration des Process Codes Inbound
2. Créer un nouveau Process Code (exemple : `Z_MATMAS`)
3. Associer le Function Module de traitement : `Z_IDOC_INPUT_MATMAS`
4. Activer le Process Code
5. Transaction `WE20` : Configuration partenaire
   - Définir quel Process Code utiliser pour quel partenaire
   - Spécifier le Message Type concerné

---

## 6. GESTION DES ERREURS

### Status IDOC principaux

| Status | Signification | Action |
|--------|---------------|--------|
| `51` | Erreur applicative | Données incorrectes, échec de validation |
| `53` | Traité avec succès | Traitement terminé sans erreur |
| `64` | En cours de traitement | IDOC en cours d'exécution |
| `68` | Traité avec avertissement | Traitement OK mais avec alertes |

### Processus de résolution d'erreur (Status 51)

1. **WE02** : Double-clic sur l'IDOC en erreur
2. Onglet **Status** : Consulter le message d'erreur détaillé
3. **WE19** : Tester et corriger les données si nécessaire
4. **Relancer** : Via BD87 (masse) ou WE19 (unitaire)

---

### Relance des IDOC en erreur

**Transaction BD87** : Relance massive
- Sélectionner les IDOC en erreur par critères
- Corriger les données si besoin
- Lancer le traitement en masse

**Transaction WE19** : Relance unitaire avec modification
- Charger l'IDOC spécifique
- Modifier les segments interactivement
- Tester le traitement en environnement isolé

---

## 7. PROGRAMME COMPLET - EXEMPLE RÉEL

```abap
*&---------------------------------------------------------------------*
*& Report Z_DEMO_IDOC_OUTBOUND
*&---------------------------------------------------------------------*
*& Création IDOC sortant pour envoi données article
*&---------------------------------------------------------------------*
REPORT z_demo_idoc_outbound.

TYPES: BEGIN OF ty_article,
         matnr TYPE mara-matnr,
         matkl TYPE mara-matkl,
         maktx TYPE makt-maktx,
       END OF ty_article.

DATA: lt_articles TYPE TABLE OF ty_article,
      ls_article  TYPE ty_article,
      lt_edidd    TYPE TABLE OF edidd,
      ls_edidc    TYPE edidc,
      ls_edidd    TYPE edidd,
      lv_docnum   TYPE edi_docnum,
      lv_sdata    TYPE edi_sdata.

" Je récupère les articles à envoyer
SELECT m~matnr, m~matkl, t~maktx
  FROM mara AS m
  INNER JOIN makt AS t ON m~matnr = t~matnr
  INTO TABLE @lt_articles
  UP TO 10 ROWS
  WHERE t~spras = 'FR'.

" Je boucle sur chaque article pour créer un IDOC
LOOP AT lt_articles INTO ls_article.

  CLEAR: lt_edidd, ls_edidc.

  " Je prépare le header
  ls_edidc-mestyp = 'MATMAS'.
  ls_edidc-idoctp = 'MATMAS05'.
  ls_edidc-rcvprn = 'PARTNER01'.
  ls_edidc-rcvprt = 'LS'.
  ls_edidc-sndprn = sy-sysid.
  ls_edidc-sndprt = 'LS'.

  " Segment E1MARAM
  CLEAR ls_edidd.
  ls_edidd-segnam = 'E1MARAM'.
  CONCATENATE 'MATNR=' ls_article-matnr 'MATKL=' ls_article-matkl
              INTO lv_sdata.
  ls_edidd-sdata = lv_sdata.
  APPEND ls_edidd TO lt_edidd.

  " Segment E1MAKTM
  CLEAR ls_edidd.
  ls_edidd-segnam = 'E1MAKTM'.
  CONCATENATE 'SPRAS=FR' 'MAKTX=' ls_article-maktx
              INTO lv_sdata.
  ls_edidd-sdata = lv_sdata.
  APPEND ls_edidd TO lt_edidd.

  " Je crée l'IDOC
  CALL FUNCTION 'MASTER_IDOC_DISTRIBUTE'
    EXPORTING
      master_idoc_control            = ls_edidc
    IMPORTING
      communication_idoc_control     = ls_edidc
      master_idoc_number             = lv_docnum
    TABLES
      communication_idoc_data        = lt_edidd
    EXCEPTIONS
      error_in_idoc_control          = 1
      error_writing_idoc_status      = 2
      error_in_idoc_data             = 3
      sending_logical_system_unknown = 4
      OTHERS                         = 5.

  IF sy-subrc = 0.
    COMMIT WORK.
    WRITE: / 'IDOC', lv_docnum, 'créé pour article', ls_article-matnr.
  ELSE.
    ROLLBACK WORK.
    WRITE: / 'Erreur création IDOC pour', ls_article-matnr COLOR COL_NEGATIVE.
  ENDIF.

ENDLOOP.
```

---

## 8. BONNES PRATIQUES CONSULTANT

### Standards ESN

1. **Monitoring** : Toujours utiliser WE02 pour le suivi des flux
2. **Logging** : Tracer tous les traitements IDOC (succès et échecs)
3. **Robustesse** : Prévoir une gestion d'erreur complète et explicite
4. **Documentation** : Documenter le mapping des segments (Excel ou Wiki)
5. **Tests** : Tester en masse ET unitaire (WE19 pour tests isolés)
6. **Scénarios négatifs** : Créer une fiche de tests incluant les cas d'erreur
7. **Versioning** : Préfixer les développements custom par `Z*` ou `Y*`

---

### Optimisation des performances

- **Traitement par lots** : COMMIT WORK tous les 1000 IDOC
- **Tables HASHED** : Utiliser pour les recherches répétées (lookups)
- **SELECT hors boucle** : Ne jamais faire de SELECT dans la boucle de segments
- **Pré-chargement** : Charger toutes les données de référence avant traitement
- **Mode parallèle** : Activer via WE21 si volumes importants (> 10 000 IDOC/jour)

---

### Communication projet - Gestion d'incident IDOC

**Processus structuré en cas de ticket :**

1. **WE02** : Analyser le status et lire le message d'erreur
2. **SE80** : Consulter le code du Process Code concerné
3. **WE19** : Reproduire l'erreur en environnement de test
4. **Root Cause** : Identifier la cause racine (données, config, code)
5. **Documentation** : Rédiger cause + solution + preuve de correction
6. **Reporting** : Informer le client avec capture d'écran WE02 avant/après

---

## 9. CHECKLIST AVANT TRANSPORT

Avant de transporter un développement IDOC en production :

- [ ] Process Code créé et activé (`WE41` / `WE42`)
- [ ] Function Module testé unitairement
- [ ] Configuration partenaire validée (`WE20`)
- [ ] Tests avec `WE19` concluants
- [ ] Gestion d'erreurs complète et robuste
- [ ] Messages `SE91` créés et translatables
- [ ] Documentation technique à jour
- [ ] Peer review effectuée par un senior
- [ ] Transport ATC compliant (0 erreur, 0 warning critique)

---

## RESSOURCES & LIENS UTILES

### Documentation officielle SAP

- **[SAP Help - IDOC Interface Overview](https://help.sap.com/docs/SAP_NETWEAVER_750/8f3819b0c24149b5959ab31070b64058/4ab3f0c26e391014adc9fffe4e204223.html)** : Guide complet sur les IDOC
- **[SAP Help - ALE Technology](https://help.sap.com/docs/ABAP_PLATFORM_NEW/8928daaba97e44f99f87563e2d8ae1bc/4eba27e76e391014adc9fffe4e204223.html)** : Application Link Enabling
- **[SAP Community - IDOC Questions](https://community.sap.com/t5/technology-q-a/ct-p/technology-questions)** : Forum d'entraide

### Transactions clés

| Transaction | Description |
|-------------|-------------|
| `WE02` | Monitoring et affichage IDOC |
| `WE19` | Test et modification IDOC |
| `WE30` | Maintenance type IDOC |
| `WE41` | Process Code Inbound |
| `WE42` | Process Code Outbound |
| `WE60` | Documentation segments IDOC |
| `WE81` | Message Types |
| `WE20` | Configuration partenaire |
| `BD87` | Relance massive IDOC |
| `WE21` | Ports de communication |

### Ressources additionnelles

- **[Clean ABAP GitHub](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md)** : Standards de code ABAP moderne
