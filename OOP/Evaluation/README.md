# Projet POEI - Intégration et Restitution Commandes d'Achat

## 📋 Description

Projet de formation ABAP pour l'intégration et la restitution de commandes d'achat depuis un fichier externe vers SAP.

**Trigramme :** PMI  
**Version :** 01.0  
**Date :** Janvier 2026

---

## 🎯 Objectifs

- Intégrer des données de commandes d'achat depuis un fichier texte tabulé
- Détecter et gérer les doublons de postes
- Restituer les données dans une interface ALV splitée avec navigation interactive
- Respecter les standards Clean ABAP et normes ESN

---

## 📦 Livrables

### Tables SAP
- **ZEKKO_PMI** : Entêtes de commandes (EBELN, BSTYP, AEDAT, ERNAM, WAERS)
- **ZEKPO_PMI** : Postes de commandes (EBELN, EBELP, MATNR, WERKS, MENGE, NETPR, NETWR, MEINS)

### Programmes ABAP
- **Z_POEC_INTEG_PMI** : Programme d'intégration batch avec mode test/réel
  - Includes : Z_POEC_INTEG_PMI_TOP, Z_POEC_INTEG_PMI_SCR, Z_POEC_INTEG_PMI_F01
- **Z_POEC_PMI** : Programme de restitution ALV avec filtres
  - Includes : Z_POEC_PMI_TOP, Z_POEC_PMI_SCR, Z_POEC_PMI_F01
  - Screen : 100 (avec custom container)
  - GUI Status : STATUS_100
  - GUI Title : TITLE_100

### Classe Globale
- **ZCL_EVENT_HANDLER_PMI** : Gestionnaire d'événements ALV avec injection de dépendances

---

## 🚀 Installation

### 1. Créer les tables
```
SE11 → ZEKKO_PMI → Créer selon structure du PDF
SE11 → ZEKPO_PMI → Créer selon structure du PDF
```

### 2. Créer la classe
```
SE24 → ZCL_EVENT_HANDLER_PMI → Copier le code
```

### 3. Créer le programme d'intégration
```
SE38 → Z_POEC_INTEG_PMI → Type Executable
Créer les includes TOP, SCR, F01
```

### 4. Créer le programme de restitution
```
SE38 → Z_POEC_PMI → Type Executable
Créer les includes TOP, SCR, F01
Créer le screen 100 avec custom container
Créer GUI Status et Title
```

---

## 🧪 Tests

Consulter le document `POEI_Plan_Tests_PMI.pdf` pour le détail complet des 8 tests.

### Tests priorité Haute

**Intégration :**
- INT-01 : Mode test
- INT-02 : Mode réel
- INT-03 : Détection doublons

**Restitution :**
- REST-01 : Affichage complet
- REST-02 : Navigation double-clic

---

## 📁 Structure du Repository

```
projet_poei_pmi/
├── README.md
├── docs/
│   └── POEI_Plan_Tests_PMI.pdf
├── abap/
│   ├── tables/
│   │   ├── ZEKKO_PMI.txt
│   │   └── ZEKPO_PMI.txt
│   ├── classes/
│   │   └── ZCL_EVENT_HANDLER_PMI.abap
│   ├── programs/
│   │   ├── integration/
│   │   │   ├── Z_POEC_INTEG_PMI.abap
│   │   │   ├── Z_POEC_INTEG_PMI_TOP.abap
│   │   │   ├── Z_POEC_INTEG_PMI_SCR.abap
│   │   │   └── Z_POEC_INTEG_PMI_F01.abap
│   │   └── restitution/
│   │       ├── Z_POEC_PMI.abap
│   │       ├── Z_POEC_PMI_TOP.abap
│   │       ├── Z_POEC_PMI_SCR.abap
│   │       └── Z_POEC_PMI_F01.abap
│   └── screens/
│       └── SCREEN_100.txt
└── data/
    └── DATA_TEST.txt (fichier exemple)
```

---

## ⚙️ Standards Techniques

### Conventions de nommage
- Variables globales : `gt_xxx`, `gv_xxx`, `gs_xxx`, `go_xxx`
- Variables locales : `lt_xxx`, `lv_xxx`, `ls_xxx`, `lo_xxx`
- Field symbols : `<fs_xxx>` (global), `<fsl_xxx>` (local)

### Bonnes pratiques
- Variables locales en début de FORM/METHOD
- Pas de SELECT dans LOOP
- FOR ALL ENTRIES avec vérification table non vide
- BINARY SEARCH après SORT
- Pas de WRITE ni BREAK-POINT en code production
- Libération objets ALV (FREE) à la sortie

### Performance
- Références CURR/QUAN pour champs montants
- HASHED/SORTED tables selon besoins
- Limitation imbrication LOOP (max 2 niveaux)

---

## 📚 Documentation

- Spécifications fonctionnelles : `Projet_POEI-V01_0.pdf`
- Plan de tests : `POEI_Plan_Tests_PMI.pdf`
- Commentaires : Toujours en 1ère personne dans le code

---

## 👨‍💻 Auteur

Développeur Junior en formation ABAP  
Trigramme : **PMI**

---

## 📝 Licence

Projet de formation - Usage académique uniquement
