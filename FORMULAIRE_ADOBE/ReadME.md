# Tutoriel : Gestion des Formulaires ABAP

## 1. Les Types de Formulaires SAP

### SAPscript (Transaction SE71)
- Formulaires "legacy" mais encore très utilisés
- Édition via éditeur texte
- Limité mais simple
- Utilisé pour : factures, bons de commande, courriers

### Smart Forms (Transaction SMARTFORMS)
- Remplaçant moderne de SAPscript
- Interface graphique drag & drop
- Gestion multilingue native
- Plus flexible et maintenable

### Adobe Forms (Transaction SFP)
- Formulaires PDF interactifs
- Design Adobe LiveCycle Designer
- Meilleur rendu visuel
- Utilisé pour documents officiels

---

## 2. Smart Forms - Le Standard Actuel

### Architecture Smart Form

```
Programme ABAP appelant
    ↓
Récupération données (DB, calculs)
    ↓
Appel fonction générée (SSF_FUNCTION_MODULE_NAME)
    ↓
Smart Form (design graphique)
    ↓
Sortie (impression, PDF, email)
```

---

## 3. Création d'un Smart Form Complet

### Étape 1 : Création du Smart Form (SMARTFORMS)

**Nom du formulaire :** `ZSF_FACTURE_CLIENT`

#### Structure du formulaire :

```
PAGE1 (Page principale)
├── HEADER (Entête)
│   └── Logo + coordonnées société
├── MAIN (Fenêtre principale)
│   ├── Infos client
│   ├── TABLE (lignes facture)
│   └── Totaux
└── FOOTER (Pied de page)
    └── Mentions légales
```

### Étape 2 : Définition des Structures

**Dans Smart Form > Form Interface > Import :**

```abap
" Je définis les paramètres d'entrée du formulaire
" Données reçues du programme appelant

PARAMETERS:
  is_header TYPE zst_facture_header  " Entête facture
  it_items  TYPE ztt_facture_items.  " Lignes de la facture
```

**Structures à créer (SE11) :**

```abap
" ZEST_FACTURE_HEADER - Entête de facture
TYPES: BEGIN OF zst_facture_header,
         facture_id   TYPE zfacture_id,      " N° facture
         date_facture TYPE datum,             " Date
         kunnr        TYPE kunnr,              " N° client
         name1        TYPE name1_gp,           " Nom client
         street       TYPE stras_gp,           " Rue
         post_code    TYPE pstlz,              " Code postal
         city         TYPE ort01_gp,           " Ville
         total_ht     TYPE wrbtr,              " Total HT
         total_tva    TYPE wrbtr,              " TVA
         total_ttc    TYPE wrbtr,              " Total TTC
       END OF zst_facture_header.

" ZEST_FACTURE_ITEM - Ligne de facture
TYPES: BEGIN OF zst_facture_item,
         posnr       TYPE posnr,               " Position
         matnr       TYPE matnr,               " Article
         arktx       TYPE arktx,               " Description
         quantity    TYPE menge_d,             " Quantité
         unit        TYPE meins,               " Unité
         price_unit  TYPE netpr,               " Prix unitaire
         total_line  TYPE wrbtr,               " Total ligne
       END OF zst_facture_item.

" Table type pour les lignes
TYPES: ztt_facture_items TYPE STANDARD TABLE OF zst_facture_item
                              WITH DEFAULT KEY.
```

### Étape 3 : Design du Smart Form

#### A. Page et Fenêtres (Onglet Pages and Windows)

**PAGE1** :
- Format : A4 Portrait
- Orientation : Vertical
- Marges : 2cm tous côtés

**Windows** :

1. **HEADER** (Entête)
   - Position : X=2cm, Y=2cm
   - Largeur : 17cm, Hauteur : 4cm

2. **MAIN** (Corps)
   - Position : X=2cm, Y=6.5cm
   - Largeur : 17cm, Hauteur : 20cm

3. **FOOTER** (Pied)
   - Position : X=2cm, Y=27cm
   - Largeur : 17cm, Hauteur : 2cm

#### B. Contenu des Fenêtres

**Dans HEADER :**

```
Clic droit > Create > Text

Contenu texte :
================================================================================
                    MA SOCIÉTÉ SAS
            123 Rue du Commerce - 75001 PARIS
              Tél : 01.23.45.67.89
================================================================================

FACTURE N° &IS_HEADER-FACTURE_ID&
Date : &IS_HEADER-DATE_FACTURE&

Client : &IS_HEADER-KUNNR& - &IS_HEADER-NAME1&
         &IS_HEADER-STREET&
         &IS_HEADER-POST_CODE& &IS_HEADER-CITY&
```

**Dans MAIN :**

1. **Créer une Table** :
   - Clic droit > Create > Table
   - Nom : `TAB_ITEMS`
   - Data : `IT_ITEMS`
   - Structure ligne : `ZST_FACTURE_ITEM`

2. **Structure de la Table** :

```
TAB_ITEMS
├── Header (Entête colonnes)
│   └── Text: Article | Description | Qté | P.U. | Total
├── Main Area (Ligne de détail - boucle automatique)
│   └── Text: &IT_ITEMS-MATNR& | &IT_ITEMS-ARKTX& | ...
└── Footer (Vide si pas de totaux intermédiaires)
```

**Détail Table Header** :
```
Clic droit sur Header > Create > Text

Paragraphe format : Bold, fond gris
Colonnes (avec tabulations) :

Article    Description              Qté    Unité    P.U.      Total
----------------------------------------------------------------------
```

**Détail Main Area** :
```
Clic droit sur Main Area > Create > Text

Format : Normal
Données dynamiques avec colonnes alignées :

&IT_ITEMS-MATNR& &IT_ITEMS-ARKTX& &IT_ITEMS-QUANTITY& &IT_ITEMS-UNIT& &IT_ITEMS-PRICE_UNIT& &IT_ITEMS-TOTAL_LINE&
```

3. **Après la table - Totaux** :
```
Clic droit après TAB_ITEMS > Create > Text

                                    Total HT  : &IS_HEADER-TOTAL_HT&  €
                                    TVA (20%) : &IS_HEADER-TOTAL_TVA& €
                                    =============================
                                    Total TTC : &IS_HEADER-TOTAL_TTC& €
```

**Dans FOOTER :**
```
Clic droit > Create > Text

Mentions légales - Société au capital de XXX € - SIRET : XXX
TVA Intracommunautaire : XXX - RCS Paris
```

---

## 4. Programme ABAP Appelant

**Nom programme :** `ZPRINT_FACTURE`

```abap
*&---------------------------------------------------------------------*
*& Report ZPRINT_FACTURE
*&---------------------------------------------------------------------*
*& Programme d'impression de facture via Smart Form
*& Je récupère les données, les prépare, et j'appelle le formulaire
*&---------------------------------------------------------------------*
REPORT zprint_facture.

*----------------------------------------------------------------------*
* TYPES - Structures de données
*----------------------------------------------------------------------*
" Je reprends les mêmes structures que dans le Smart Form
TYPES: BEGIN OF ty_facture_header,
         facture_id   TYPE char10,
         date_facture TYPE datum,
         kunnr        TYPE kunnr,
         name1        TYPE name1_gp,
         street       TYPE stras_gp,
         post_code    TYPE pstlz,
         city         TYPE ort01_gp,
         total_ht     TYPE wrbtr,
         total_tva    TYPE wrbtr,
         total_ttc    TYPE wrbtr,
       END OF ty_facture_header,

       BEGIN OF ty_facture_item,
         posnr       TYPE posnr,
         matnr       TYPE matnr,
         arktx       TYPE arktx,
         quantity    TYPE menge_d,
         unit        TYPE meins,
         price_unit  TYPE netpr,
         total_line  TYPE wrbtr,
       END OF ty_facture_item,

       ty_t_facture_items TYPE STANDARD TABLE OF ty_facture_item
                               WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* DATA - Variables globales (uniquement si réellement partagées)
*----------------------------------------------------------------------*
" Ici, données partagées entre toutes les méthodes
DATA: gv_fm_name TYPE rs38l_fnam. " Nom fonction générée par Smart Form

*----------------------------------------------------------------------*
* SELECTION-SCREEN - Écran de sélection
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_facid TYPE char10 OBLIGATORY. " N° facture à imprimer
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* START-OF-SELECTION - Point d'entrée principal
*----------------------------------------------------------------------*
START-OF-SELECTION.
  " J'exécute le processus d'impression
  PERFORM main_process.

*&---------------------------------------------------------------------*
*& Form MAIN_PROCESS
*&---------------------------------------------------------------------*
*& Orchestration du processus complet d'impression
*&---------------------------------------------------------------------*
FORM main_process.
  " Variables locales pour ce traitement
  DATA: ls_header TYPE ty_facture_header,
        lt_items  TYPE ty_t_facture_items.

  " 1. Je récupère les données de la facture
  PERFORM get_invoice_data
    USING    p_facid
    CHANGING ls_header
             lt_items.

  " 2. Je vérifie que j'ai bien des données
  IF ls_header IS INITIAL.
    MESSAGE 'Facture non trouvée' TYPE 'E'.
    RETURN.
  ENDIF.

  " 3. Je récupère le nom de la fonction générée
  PERFORM get_function_name
    CHANGING gv_fm_name.

  " 4. J'appelle le Smart Form pour impression
  PERFORM call_smartform
    USING    ls_header
             lt_items
             gv_fm_name.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_INVOICE_DATA
*&---------------------------------------------------------------------*
*& Récupération des données de la facture depuis la base
*& En production : requêtes sur vraies tables (VBRK, VBRP, KNA1)
*&---------------------------------------------------------------------*
FORM get_invoice_data
  USING    iv_facture_id TYPE char10
  CHANGING cs_header     TYPE ty_facture_header
           ct_items      TYPE ty_t_facture_items.

  " Variables locales pour les SELECT
  DATA: lt_items_tmp TYPE ty_t_facture_items,
        ls_item      TYPE ty_facture_item,
        lv_total_ht  TYPE wrbtr.

  " Je simule la récupération des données (en prod : SELECT sur tables)
  " Ici données de test pour démonstration
  
  " Entête facture
  cs_header-facture_id   = iv_facture_id.
  cs_header-date_facture = sy-datum.
  cs_header-kunnr        = '0000100001'.
  cs_header-name1        = 'DUPONT SARL'.
  cs_header-street       = '456 Avenue des Clients'.
  cs_header-post_code    = '69000'.
  cs_header-city         = 'LYON'.

  " Lignes de facture (en prod : SELECT * FROM ztt_facture_items)
  CLEAR ls_item.
  ls_item-posnr      = '000010'.
  ls_item-matnr      = 'MAT001'.
  ls_item-arktx      = 'Produit A - Description longue'.
  ls_item-quantity   = '5.00'.
  ls_item-unit       = 'UN'.
  ls_item-price_unit = '100.00'.
  ls_item-total_line = ls_item-quantity * ls_item-price_unit.
  APPEND ls_item TO ct_items.

  CLEAR ls_item.
  ls_item-posnr      = '000020'.
  ls_item-matnr      = 'MAT002'.
  ls_item-arktx      = 'Produit B - Autre article'.
  ls_item-quantity   = '3.00'.
  ls_item-unit       = 'UN'.
  ls_item-price_unit = '250.00'.
  ls_item-total_line = ls_item-quantity * ls_item-price_unit.
  APPEND ls_item TO ct_items.

  " Je calcule les totaux
  CLEAR lv_total_ht.
  LOOP AT ct_items INTO ls_item.
    lv_total_ht = lv_total_ht + ls_item-total_line.
  ENDLOOP.

  cs_header-total_ht  = lv_total_ht.
  cs_header-total_tva = lv_total_ht * '0.20'. " TVA 20%
  cs_header-total_ttc = cs_header-total_ht + cs_header-total_tva.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_FUNCTION_NAME
*&---------------------------------------------------------------------*
*& Récupération du nom de la fonction générée par le Smart Form
*& Chaque Smart Form génère une fonction avec un nom technique
*&---------------------------------------------------------------------*
FORM get_function_name
  CHANGING cv_fm_name TYPE rs38l_fnam.

  " Variables locales
  DATA: lv_formname TYPE tdsfname.

  " Le nom du Smart Form créé
  lv_formname = 'ZSF_FACTURE_CLIENT'.

  " J'appelle la fonction qui me donne le nom technique généré
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = lv_formname
    IMPORTING
      fm_name            = cv_fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    " En cas d'erreur, je gère proprement
    MESSAGE 'Erreur récupération fonction Smart Form' TYPE 'E'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CALL_SMARTFORM
*&---------------------------------------------------------------------*
*& Appel du Smart Form pour génération et impression
*&---------------------------------------------------------------------*
FORM call_smartform
  USING is_header  TYPE ty_facture_header
        it_items   TYPE ty_t_facture_items
        iv_fm_name TYPE rs38l_fnam.

  " Variables locales pour le contrôle du formulaire
  DATA: ls_control_param TYPE ssfctrlop,  " Paramètres de contrôle
        ls_output_opt    TYPE ssfcompop,  " Options de sortie
        ls_job_output    TYPE ssfcrescl.  " Résultat du job

  " Je configure les options d'impression
  ls_control_param-no_dialog = abap_true.  " Pas de popup d'impression
  ls_control_param-preview   = 'X'.        " Aperçu avant impression
  
  ls_output_opt-tdprinter = 'LOCL'.        " Imprimante locale (ou autre)

  " J'appelle la fonction générée par le Smart Form
  " Attention : signature fonction dynamique selon le Smart Form
  CALL FUNCTION iv_fm_name
    EXPORTING
      control_parameters = ls_control_param
      output_options     = ls_output_opt
      is_header          = is_header         " Mes données entête
      it_items           = it_items          " Mes lignes
    IMPORTING
      job_output_info    = ls_job_output
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF sy-subrc = 0.
    MESSAGE 'Impression lancée avec succès' TYPE 'S'.
  ELSE.
    MESSAGE 'Erreur lors de l''impression' TYPE 'E'.
  ENDIF.

ENDFORM.
```

---

## 5. Points Clés & Bonnes Pratiques

### ✅ Nommage
- Smart Form : `Z[MODULE]_[TYPE]_[OBJET]`
- Exemple : `ZSD_INV_CUSTOMER`, `ZMM_PO_SUPPLIER`

### ✅ Performance
- **Toujours préparer les données AVANT d'appeler le formulaire**
- Pas de SELECT dans le Smart Form
- Passer des tables déjà triées/calculées

### ✅ Modularité
```abap
" Découper le programme en FORMs focalisées
PERFORM get_data.        " Une seule responsabilité
PERFORM calculate_totals.
PERFORM format_output.
PERFORM print_form.
```

### ✅ Gestion Erreurs
```abap
" Toujours vérifier sy-subrc après appels fonction
CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
  " ...
  EXCEPTIONS
    no_form = 1
    OTHERS  = 99.

CASE sy-subrc.
  WHEN 0.
    " OK, je continue
  WHEN 1.
    MESSAGE 'Smart Form introuvable' TYPE 'E'.
  WHEN OTHERS.
    MESSAGE 'Erreur technique' TYPE 'E'.
ENDCASE.
```

### ✅ Multilingue
```abap
" Dans Smart Form > Form Attributes
" Cocher : Translation Relevant
" Créer versions par langue (SE63)

" Dans le programme appelant :
ls_output_opt-tdlangu = sy-langu. " Langue utilisateur
```

---

## 6. Debugging Smart Form

### Activer le debug :

```abap
" Dans le programme appelant, avant CALL FUNCTION
ls_control_param-no_dialog = abap_false. " J'active les dialogs
ls_control_param-preview   = 'X'.
ls_control_param-getotf    = abap_true.  " Je récupère l'OTF
```

### Points d'arrêt :
- Dans Smart Form : Menu **Utilities > Activate Debugger**
- Breakpoints possibles sur textes, conditions, tables

---

## 7. Export PDF par Email

```abap
*&---------------------------------------------------------------------*
*& Form SEND_INVOICE_BY_EMAIL
*&---------------------------------------------------------------------*
*& Envoi de la facture par email au format PDF
*&---------------------------------------------------------------------*
FORM send_invoice_by_email
  USING is_header  TYPE ty_facture_header
        it_items   TYPE ty_t_facture_items
        iv_fm_name TYPE rs38l_fnam
        iv_email   TYPE ad_smtpadr.

  " Variables locales pour génération PDF
  DATA: ls_control_param TYPE ssfctrlop,
        ls_output_opt    TYPE ssfcompop,
        ls_job_output    TYPE ssfcrescl,
        lt_otf           TYPE ssfcrescl, " Table OTF
        lt_pdf           TYPE TABLE OF tline,
        lv_pdf_size      TYPE i.

  " Je configure pour récupérer l'OTF (format intermédiaire)
  ls_control_param-no_dialog = abap_true.
  ls_control_param-getotf    = abap_true. " Important pour conversion PDF
  ls_output_opt-tdprinter    = 'LOCL'.

  " J'appelle le Smart Form
  CALL FUNCTION iv_fm_name
    EXPORTING
      control_parameters = ls_control_param
      output_options     = ls_output_opt
      is_header          = is_header
      it_items           = it_items
    IMPORTING
      job_output_info    = ls_job_output
    EXCEPTIONS
      OTHERS             = 1.

  CHECK sy-subrc = 0.

  " Je convertis l'OTF en PDF
  CALL FUNCTION 'CONVERT_OTF_2_PDF'
    EXPORTING
      otf         = ls_job_output-otfdata
    IMPORTING
      pdf_table   = lt_pdf
      pdf_size    = lv_pdf_size
    EXCEPTIONS
      OTHERS      = 1.

  CHECK sy-subrc = 0.

  " J'envoie le PDF par email
  PERFORM send_email_with_pdf
    USING iv_email
          lt_pdf
          lv_pdf_size
          is_header-facture_id.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SEND_EMAIL_WITH_PDF
*&---------------------------------------------------------------------*
*& Envoi email avec pièce jointe PDF via BCS (Business Communication)
*&---------------------------------------------------------------------*
FORM send_email_with_pdf
  USING iv_email      TYPE ad_smtpadr
        it_pdf        TYPE TABLE
        iv_pdf_size   TYPE i
        iv_facture_id TYPE char10.

  " Variables locales pour l'email
  DATA: lo_send_request TYPE REF TO cl_bcs,
        lo_document     TYPE REF TO cl_document_bcs,
        lo_recipient    TYPE REF TO if_recipient_bcs,
        lv_subject      TYPE so_obj_des,
        lt_text         TYPE bcsy_text,
        ls_text         TYPE soli,
        lv_sent         TYPE abap_bool.

  TRY.
      " Je crée la requête d'envoi
      lo_send_request = cl_bcs=>create_persistent( ).

      " Je prépare le sujet
      lv_subject = |Facture { iv_facture_id }|.

      " Je crée le corps du message
      ls_text = 'Bonjour,'.
      APPEND ls_text TO lt_text.
      ls_text = 'Veuillez trouver ci-joint votre facture.'.
      APPEND ls_text TO lt_text.
      ls_text = 'Cordialement,'.
      APPEND ls_text TO lt_text.

      " Je crée le document avec la pièce jointe PDF
      lo_document = cl_document_bcs=>create_document(
        i_type    = 'RAW'
        i_text    = lt_text
        i_subject = lv_subject ).

      " J'ajoute le PDF en pièce jointe
      lo_document->add_attachment(
        i_attachment_type    = 'PDF'
        i_attachment_subject = lv_subject
        i_att_content_hex    = it_pdf ).

      " J'associe le document à la requête
      lo_send_request->set_document( lo_document ).

      " J'ajoute le destinataire
      lo_recipient = cl_cam_address_bcs=>create_internet_address( iv_email ).
      lo_send_request->add_recipient( lo_recipient ).

      " J'envoie immédiatement
      lv_sent = lo_send_request->send( ).

      IF lv_sent = abap_true.
        COMMIT WORK.
        MESSAGE 'Email envoyé avec succès' TYPE 'S'.
      ELSE.
        MESSAGE 'Erreur lors de l''envoi' TYPE 'E'.
      ENDIF.

    CATCH cx_bcs INTO DATA(lx_bcs).
      " Je gère l'exception proprement
      MESSAGE lx_bcs->get_text( ) TYPE 'E'.
  ENDTRY.

ENDFORM.
```

---

## 8. Checklist Validation ESN

Avant de livrer un formulaire :

- [ ] Smart Form activé sans erreurs
- [ ] Programme appelant testé avec données réelles
- [ ] Transport créé et documenté
- [ ] Fiche de tests complétée (cas nominaux + erreurs)
- [ ] Pas de valeurs en dur dans le code
- [ ] Messages externalisés (SE91)
- [ ] Performance validée (pas de SELECT dans formulaire)
- [ ] Multi-langue géré si applicable
- [ ] Documentation technique rédigée
- [ ] Code ATC compliant
- [ ] Peer review effectuée

---

## 9. Conseil Tech Lead

**Formulaires = vitrine du système**

Les utilisateurs jugent la qualité SAP sur ce qu'ils voient : les formulaires imprimés. Un rapport peut être moche en interne, mais une facture mal formatée = image dégradée de l'entreprise.

**Priorisation :**
1. Données correctes (calculs, totaux)
2. Format professionnel (alignements, polices)
3. Performance (pas de lenteur à l'impression)
4. Maintenabilité (code propre, commenté)

En ESN, on te demandera souvent des modifications "cosmétiques" sur des formulaires. Garde ton calme : c'est normal, c'est visible, donc c'est scruté. Documente chaque version.

---

**Prêt à pratiquer ?** Crée ton premier Smart Form de facture, teste-le, puis on passera aux Adobe Forms si nécessaire.
