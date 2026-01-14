# Tutoriel : Ajouter du texte dans un formulaire SAP

## Vue d'ensemble

Ce guide couvre toutes les méthodes pour intégrer du texte dans un formulaire SAP (SAPscript et Smart Forms).

---

## 1. SAPscript

### 1.1 Texte direct dans la fenêtre

```abap
" Dans SE71, éditeur de formulaire
" Fenêtre MAIN par exemple :

/E TEXT
Ceci est un texte statique direct.
Il sera affiché tel quel sur le formulaire.
```

**Usage :** Texte fixe, labels, en-têtes standards.

---

### 1.2 Variables de programme (symboles)

```abap
" Programme d'impression
DATA: gv_client TYPE kunnr VALUE '100001',
      gv_date   TYPE datum VALUE sy-datum.

" Dans SE71 :
/E TEXT
Client : &gv_client&
Date   : &gv_date&
```

**Syntaxe :** `&nom_variable&`

**Formatage :**
- `&gv_date(D)&` → format date local
- `&gv_amount(C)&` → format montant avec devise

**Usage :** Afficher données variables du programme.

---

### 1.3 Textes standards (SO10)

```abap
" 1. Créer texte standard via SO10
" ID : Z_MENTIONS_LEGALES
" Langue : FR
" Contenu : mentions légales réutilisables

" 2. Dans SE71 :
/: INCLUDE Z_MENTIONS_LEGALES OBJECT TEXT ID ST LANGUAGE FR

" 3. Ou via commande ABAP :
CALL FUNCTION 'READ_TEXT'
  EXPORTING
    id       = 'ST'
    language = sy-langu
    name     = 'Z_MENTIONS_LEGALES'
    object   = 'TEXT'
  TABLES
    lines    = lt_lines.
```

**Usage :** Textes réutilisables, multilingues, mentions légales.

---

### 1.4 Textes conditionnels

```abap
" Programme
DATA: gv_country TYPE land1 VALUE 'FR'.

" Dans SE71 :
/: IF &gv_country& = 'FR'
Texte spécifique pour la France
/: ELSIF &gv_country& = 'DE'
Deutscher Text
/: ELSE
Default text in English
/: ENDIF
```

**Usage :** Adapter texte selon contexte (langue, type document, etc.).

---

### 1.5 Textes depuis table STXH (textes longs)

```abap
" Lire texte long attaché à un objet métier
DATA: lt_lines TYPE TABLE OF tline.

CALL FUNCTION 'READ_TEXT'
  EXPORTING
    id                      = '0001'
    language                = sy-langu
    name                    = '0000100001' " Numéro commande par ex.
    object                  = 'VBBK'       " Objet métier
  TABLES
    lines                   = lt_lines
  EXCEPTIONS
    id_or_name_missing      = 1
    not_found               = 2
    OTHERS                  = 3.

IF sy-subrc = 0.
  " Passer lt_lines au formulaire ou traiter
ENDIF.
```

**Usage :** Commentaires saisis par utilisateur, notes de commande, etc.

---

## 2. Smart Forms

### 2.1 Élément TEXT (statique)

```
Créer nœud TEXT dans arborescence Smart Form
→ Onglet "Texte général"
→ Saisir directement : "Facture n°"
```

**Usage :** Labels fixes, titres de sections.

---

### 2.2 Variables via symboles

```abap
" Smart Form, nœud TEXT :
Facture n° &wa_invoice-vbeln&
Date : &wa_invoice-fkdat&

" Formatage :
Montant : &wa_invoice-netwr(C)&
Date    : &wa_invoice-fkdat(D)&
```

**Syntaxe identique à SAPscript.**

---

### 2.3 Texte standard (via nœud INCLUDE)

```
1. Créer nœud "Include Text" dans arborescence
2. Paramètres :
   - Nom : Z_MENTIONS_LEGALES
   - Objet : TEXT
   - ID : ST
   - Langue : &sy-langu&
```

**Usage :** Insérer textes SO10 réutilisables.

---

### 2.4 Code ABAP pour texte dynamique

```abap
" Nœud "Code" avant nœud TEXT

DATA: lv_text TYPE string.

" Je construis le texte selon la logique métier
IF wa_invoice-netwr > 10000.
  lv_text = 'Client premium - remise applicable'.
ELSE.
  lv_text = 'Client standard'.
ENDIF.

" Puis dans nœud TEXT suivant :
&lv_text&
```

**Usage :** Logique complexe pour déterminer le texte.

---

### 2.5 Texte multiligne (Table de texte)

```abap
" Nœud "Table", ligne de données : lt_text_lines
" Chaque ligne contient un champ texte

" Programme principal :
DATA: lt_text_lines TYPE TABLE OF string.

APPEND 'Ligne 1 de texte' TO lt_text_lines.
APPEND 'Ligne 2 de texte' TO lt_text_lines.

" Passer lt_text_lines au Smart Form

" Dans Smart Form, nœud Table :
" → Main Area → Text
" → &lt_text_lines-table_line&
```

**Usage :** Afficher liste de lignes de texte variable.

---

### 2.6 Texte HTML/RTF (éditeur graphique)

```
Smart Forms supporte formatage basique via éditeur :
- Gras, italique
- Paragraphes
- Alignement

Limites : pas de HTML complet, uniquement formatage SAP
```

**Usage :** Mise en forme visuelle simple.

---

## 3. Textes conditionnels avancés

### 3.1 Conditions Smart Forms

```
Nœud "Alternative"
→ Branche 1 : Condition &wa_invoice-waerk& = 'EUR'
  → Texte : "Montant en Euros"
→ Branche 2 : Condition &wa_invoice-waerk& = 'USD'
  → Texte : "Amount in Dollars"
→ Défaut : "Montant"
```

---

### 3.2 Conditions SAPscript (rappel)

```abap
/: IF &condition&
Texte si vrai
/: ELSE
Texte sinon
/: ENDIF
```

---

## 4. Bonnes pratiques

### Standards à respecter

| Pratique | Recommandation |
|----------|----------------|
| **Textes statiques** | Directement dans formulaire si < 5 lignes |
| **Textes réutilisables** | SO10 obligatoire |
| **Textes métier longs** | Table STXH via READ_TEXT |
| **Traduction** | Toujours via SO10 ou SE63 |
| **Messages** | JAMAIS en dur, passer par SE91 |
| **Variables** | Typage fort, pas de casting implicite |

---

### Exemple complet Smart Form

```abap
" Programme principal
TYPES: BEGIN OF ty_header,
         vbeln TYPE vbeln_vf,
         fkdat TYPE fkdat,
         kunag TYPE kunag,
         netwr TYPE netwr,
       END OF ty_header.

DATA: wa_header TYPE ty_header,
      lt_text   TYPE TABLE OF string,
      lv_formname TYPE tdsfname VALUE 'Z_INVOICE_FORM',
      lv_fm_name  TYPE rs38l_fnam.

" Je récupère les données
SELECT SINGLE vbeln fkdat kunag netwr
  INTO wa_header
  FROM vbrk
  WHERE vbeln = '0090000001'.

" Je prépare un texte multiligne
APPEND 'Merci pour votre commande.' TO lt_text.
APPEND 'Cordialement,' TO lt_text.
APPEND 'Service commercial' TO lt_text.

" Je récupère le nom de la fonction générée
CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
  EXPORTING
    formname           = lv_formname
  IMPORTING
    fm_name            = lv_fm_name
  EXCEPTIONS
    no_form            = 1
    no_function_module = 2
    OTHERS             = 3.

IF sy-subrc = 0.
  " J'appelle le formulaire
  CALL FUNCTION lv_fm_name
    EXPORTING
      wa_header = wa_header
    TABLES
      it_text   = lt_text
    EXCEPTIONS
      OTHERS    = 1.
ENDIF.
```

**Smart Form (arborescence) :**
```
Z_INVOICE_FORM
├── PAGE1
│   ├── HEADER
│   │   └── TEXT: "FACTURE"
│   ├── MAIN
│   │   ├── TEXT: "N° &wa_header-vbeln&"
│   │   ├── TEXT: "Date : &wa_header-fkdat(D)&"
│   │   ├── TEXT: "Montant : &wa_header-netwr(C)&"
│   │   ├── INCLUDE: Z_MENTIONS_LEGALES
│   │   └── TABLE: it_text
│   │       └── TEXT: "&it_text-table_line&"
│   └── FOOTER
│       └── TEXT: "Page &SFSY-PAGE& / &SFSY-FORMPAGES&"
```

---

## 5. Débogage textes

### SAPscript
- SE71 → Utilitaires → Activer débogage
- Variables visibles dans debugger

### Smart Forms
- SMARTFORMS → Form → Activer débogage
- Breakpoint dans nœuds Code
- Variables visibles dans contexte

---

## 6. Checklist finale

Avant activation formulaire :

- [ ] Aucun texte en dur (sauf labels courts)
- [ ] Textes réutilisables → SO10
- [ ] Variables typées correctement
- [ ] Formatage appliqué ((D), (C), etc.)
- [ ] Traductions prévues si besoin
- [ ] Conditions testées (tous les cas)
- [ ] READ_TEXT géré (EXCEPTIONS)
- [ ] Performance : pas de SELECT dans boucle
- [ ] Tests sur plusieurs langues
- [ ] Documentation MAJ

---

## Conclusion

**Règle d'or :** Anticiper la maintenance. Un texte qui changera souvent → SO10. Un texte fixe court → direct dans formulaire. Un texte métier → table STXH.

→ Voir [04 - INCLUDE.md] pour gestion textes standards  
→ Voir [Cours Smart Forms] pour détails structure formulaires

---

**Auteur :** Tech Lead ABAP Senior  
**Version :** 1.0  
**Date :** 2025-01-14
