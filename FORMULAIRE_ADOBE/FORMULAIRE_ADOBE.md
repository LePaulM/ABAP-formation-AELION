# Adobe Forms - Guide Complet ABAP

## Vue d'ensemble

Les Adobe Forms permettent de créer des formulaires PDF interactifs ou imprimables depuis SAP. Ils remplacent progressivement les SAPscripts et SmartForms.

**Architecture :**
- **Interface** : définit les données échangées entre ABAP et le formulaire
- **Formulaire** : contient la mise en page et la logique d'affichage
- **Programme d'impression** : pilote l'appel et l'envoi du PDF

---

## 1. Création de l'Interface

### T-Code : `SFP` > Onglet "Interface"
```abap
" Interface : ZIF_FACTURE_CLIENT
" Description : Interface pour formulaire de facture

" 1. Import Parameters (données d'entrée)
I_VBELN TYPE VBELN_VF    " Numéro de facture
I_KUNNR TYPE KUNNR       " Client

" 2. Export Parameters (données de sortie - optionnel)
" Utilisé si besoin de retourner des infos au programme appelant

" 3. Tables Parameters
" T_ITEMS TYPE TY_T_ITEMS   " Postes de facture

" 4. Global Data (variables globales accessibles partout)
" Déclarées dans l'onglet "Global Definitions"

TYPES: BEGIN OF ty_item,
         posnr TYPE posnr,
         matnr TYPE matnr,
         arktx TYPE arktx,
         kwmeng TYPE kwmeng,
         netwr TYPE netwr,
       END OF ty_item.

TYPES: ty_t_items TYPE STANDARD TABLE OF ty_item.

" 5. Code & Forms Routines
" Logique de récupération des données
DATA: lt_items TYPE ty_t_items,
      ls_vbrk  TYPE vbrk,
      ls_vbrp  TYPE vbrp.

" Je récupère l'en-tête de facture
SELECT SINGLE * FROM vbrk INTO ls_vbrk
  WHERE vbeln = i_vbeln.

IF sy-subrc = 0.
  " Je récupère les postes
  SELECT posnr matnr arktx kwmeng netwr
    FROM vbrp
    INTO CORRESPONDING FIELDS OF TABLE lt_items
    WHERE vbeln = i_vbeln.
ENDIF.
```

**Points clés :**
- L'interface = contrat entre ABAP et le formulaire
- Je définis ici TOUTES les données nécessaires à l'impression
- Le code dans "Code Initialization" s'exécute AVANT le rendu du formulaire

---

## 2. Création du Formulaire

### T-Code : `SFP` > Onglet "Form"

#### 2.1 Propriétés du formulaire
```abap
" Formulaire : ZF_FACTURE_CLIENT
" Description : Formulaire de facture client
" Interface associée : ZIF_FACTURE_CLIENT

" Onglet General Properties :
" - Master Page : définit la structure globale (en-tête, corps, pied)
" - Layout Type : ZCI ou Standard
```

#### 2.2 Context (arbre des données)

Le Context affiche automatiquement les paramètres de l'interface.

**Structure typique :**
```
Context
├── I_VBELN (Import)
├── I_KUNNR (Import)
├── LT_ITEMS (Table)
│   ├── POSNR
│   ├── MATNR
│   ├── ARKTX
│   ├── KWMENG
│   └── NETWR
├── LS_VBRK (Structure)
└── Variables locales (si définies)
```

#### 2.3 Layout (Adobe LiveCycle Designer)

**Ouvrir le Layout :** Clic sur "Layout" → Adobe LiveCycle Designer s'ouvre

**Zones principales :**
- **Master Pages** : modèles de pages (marges, en-têtes, pieds)
- **Body Pages** : contenu principal du formulaire
- **Fragments** : éléments réutilisables

**Éléments de base :**
```javascript
// 1. Afficher un champ simple
// Je glisse-dépose I_VBELN depuis la palette "Data View"
// → Crée automatiquement un champ texte

// 2. Afficher une table (Loop sur LT_ITEMS)
// a) Je glisse LT_ITEMS sur le Layout
// b) Je sélectionne "Table" dans le wizard
// c) Je choisis les colonnes à afficher

// 3. Calculs dynamiques (JavaScript dans les propriétés du champ)
// Exemple : Total TTC = HT * 1.20
this.rawValue = LS_VBRK.NETWR * 1.20;

// 4. Conditions d'affichage
// Onglet "Binding" du champ → "Presence"
// Script :
if (I_KUNNR == "0000100000") {
  this.presence = "hidden"; // Je masque ce champ pour ce client
} else {
  this.presence = "visible";
}

// 5. Formatage des nombres
// Onglet "Object" → "Patterns"
// Montant : num{z,zzz,zz9.99} (sépare les milliers, 2 décimales)
```

**Astuce Layout :**
- **Subform** : conteneur pour grouper des éléments
- **TextField** : texte statique ou dynamique
- **Table** : pour itérer sur une table interne
- **ImageField** : logo, signature...

---

## 3. Programme d'impression
```abap
REPORT z_print_facture.

" Je déclare les paramètres d'écran
PARAMETERS: p_vbeln TYPE vbeln_vf OBLIGATORY,
            p_kunnr TYPE kunnr.

START-OF-SELECTION.

  " Je déclare les variables nécessaires
  DATA: lv_fm_name     TYPE rs38l_fnam,
        ls_outputparams TYPE sfpoutputparams,
        ls_docparams    TYPE sfpdocparams,
        ls_formoutput   TYPE fpformoutput,
        lx_exception    TYPE REF TO cx_root.

  TRY.
      " 1. J'ouvre la session Adobe Forms
      ls_outputparams-dest     = 'LP01'.        " Imprimante (ou LOCL)
      ls_outputparams-nodialog = abap_true.     " Pas de dialogue d'impression
      ls_outputparams-getpdf   = abap_true.     " Je veux récupérer le PDF
      
      CALL FUNCTION 'FP_JOB_OPEN'
        CHANGING
          ie_outputparams = ls_outputparams
        EXCEPTIONS
          cancel          = 1
          usage_error     = 2
          system_error    = 3
          internal_error  = 4
          OTHERS          = 5.
      
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        RETURN.
      ENDIF.

      " 2. Je récupère le nom du module fonction généré
      CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
        EXPORTING
          i_name     = 'ZF_FACTURE_CLIENT'    " Nom de mon formulaire
        IMPORTING
          e_funcname = lv_fm_name.

      " 3. J'appelle le module fonction avec mes données
      CALL FUNCTION lv_fm_name
        EXPORTING
          /1bcdwb/docparams  = ls_docparams
          i_vbeln            = p_vbeln           " Paramètres de mon interface
          i_kunnr            = p_kunnr
        IMPORTING
          /1bcdwb/formoutput = ls_formoutput
        EXCEPTIONS
          usage_error        = 1
          system_error       = 2
          internal_error     = 3
          OTHERS             = 4.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      " 4. Je ferme la session
      CALL FUNCTION 'FP_JOB_CLOSE'
        EXCEPTIONS
          usage_error    = 1
          system_error   = 2
          internal_error = 3
          OTHERS         = 4.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      " 5. J'affiche ou je sauvegarde le PDF
      IF ls_formoutput-pdf IS NOT INITIAL.
        " Je peux sauvegarder le PDF dans un répertoire
        " ou l'afficher directement
        MESSAGE 'PDF généré avec succès' TYPE 'S'.
      ENDIF.

    CATCH cx_root INTO lx_exception.
      MESSAGE lx_exception->get_text( ) TYPE 'E'.
  ENDTRY.
```

**Architecture complète :**
```
Programme ABAP
    ↓ FP_JOB_OPEN
    ↓ FP_FUNCTION_MODULE_NAME
    ↓ /1BCDWB/SM00000123 (FM généré)
        ↓ Interface ZIF_FACTURE_CLIENT
            ↓ Code Initialization (récup données)
            ↓ Formulaire ZF_FACTURE_CLIENT
                ↓ Layout Adobe (rendu PDF)
    ↓ FP_JOB_CLOSE
    ↓ PDF généré
```

---

## 4. Bonnes pratiques

### 4.1 Performance
```abap
" ✅ BON : Je charge toutes les données AVANT le formulaire
SELECT vbeln netwr waerk
  FROM vbrk
  INTO TABLE @DATA(lt_vbrk)
  WHERE vbeln IN @s_vbeln
    AND fkart = 'F2'.

" ❌ MAUVAIS : SELECT dans le formulaire (lent)
" Ne JAMAIS faire de SELECT dans le Layout ou dans les scripts JavaScript
```

### 4.2 Modularisation
```abap
" Je crée des méthodes réutilisables dans l'interface

" Code dans l'onglet "Form Routines"
FORM calculate_total
  USING    i_netwr TYPE netwr
           i_mwskz TYPE mwskz
  CHANGING c_total TYPE netwr.

  " Je calcule le total TTC
  DATA: lv_tax_rate TYPE p DECIMALS 2.
  
  CASE i_mwskz.
    WHEN 'A1'. lv_tax_rate = '1.20'.  " TVA 20%
    WHEN 'A2'. lv_tax_rate = '1.10'.  " TVA 10%
    WHEN OTHERS. lv_tax_rate = '1.00'. " Exonéré
  ENDCASE.
  
  c_total = i_netwr * lv_tax_rate.
  
ENDFORM.
```

### 4.3 Gestion des erreurs
```abap
" Je vérifie systématiquement les CALL FUNCTION
IF sy-subrc <> 0.
  " Je logue l'erreur dans SLG1 (BAL_LOG_*)
  " et j'informe l'utilisateur
  MESSAGE e001(zfi) WITH 'Erreur génération PDF'.
  RETURN.
ENDIF.
```

### 4.4 Tests

- **Test unitaire** : données fictives complètes
- **Test intégration** : données réelles d'un environnement de DEV
- **Validation** : vérifier TOUS les cas (client standard, client VIP, montant négatif, etc.)

---

## 5. Configuration système (Basis)

### Prérequis SAP :
- **ADS** (Adobe Document Services) activé
- Service RFC `ADS` configuré (SM59)
- Licence Adobe valide
- Paramètres SSFA (SFP → Administration)

### Vérification :
```abap
" T-Code : SFP > Utilities > Test ADS Connection
" Ou report standard : FP_TEST_00
```

---

## 6. Cycle de vie
```
1. Création Interface (SFP)
   ↓
2. Création Formulaire (SFP)
   ↓
3. Design Layout (LiveCycle Designer)
   ↓
4. Activation Interface + Formulaire
   ↓
5. Programme d'impression
   ↓
6. Test en DEV
   ↓
7. Transport en QA/PROD
```

---

## 7. Comparaison avec SmartForms

| Critère | SmartForms | Adobe Forms |
|---------|-----------|-------------|
| Technologie | SAP natif | Adobe LiveCycle |
| Interface graphique | Basique | Avancé (WYSIWYG) |
| PDF interactif | Non | Oui |
| Performance | Moyenne | Bonne |
| Maintenabilité | Moyenne | Bonne |
| Future SAP | Maintenance uniquement | Recommandé |

---

## 8. Debugging
```abap
" 1. Debugger l'interface
" → Breakpoint dans "Code Initialization" de l'interface (SFP)

" 2. Debugger le programme appelant
" → Breakpoint classique avant FP_JOB_OPEN

" 3. Voir le XML généré
CALL FUNCTION 'FP_JOB_OPEN'
  CHANGING
    ie_outputparams = ls_outputparams.

" Je récupère le XML avant génération PDF
ls_outputparams-getpdf = abap_false.
ls_outputparams-getxml = abap_true.  " Active sortie XML

" Après FP_JOB_CLOSE, je consulte ls_formoutput-xml

" 4. Traces Adobe (transaction SFP > Utilities > Trace)
```

---

## 9. Exemple complet

### Structure projet :
```
/adobe-forms/
├── exemple-facture/
│   ├── ZIF_FACTURE.abap          # Interface
│   ├── ZF_FACTURE.xml            # Export formulaire
│   ├── Z_PRINT_FACTURE.abap      # Programme impression
│   └── README.md                 # Documentation
```

---

## 10. Ressources

- **Documentation SAP** : Aide F1 dans SFP
- **SCN** : SAP Community Network (forums Adobe Forms)
- **Cours interne** : Voir supports formation si disponibles

---

## Checklist finale

- [ ] Interface activée sans erreur
- [ ] Formulaire activé sans erreur
- [ ] Layout testé avec données fictives
- [ ] Programme d'impression fonctionnel
- [ ] Test avec données réelles
- [ ] Gestion erreurs implémentée
- [ ] Performance validée (<2s pour génération)
- [ ] Documentation MAJ
- [ ] Transport créé

---

## Conseil Tech Lead

Les Adobe Forms, c'est comme un bon steak : la qualité se joue dans la préparation (interface + données). Si tu charges proprement en amont, le rendu sera rapide et propre. Si tu SELECT dans tous les sens dans le formulaire, c'est la cata en prod. Pense TOUJOURS volumétrie !
