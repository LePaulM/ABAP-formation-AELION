# Exercices Pratiques - Gestion des Formulaires ABAP

## Guide d'utilisation

Chaque exercice suit le format ESN réaliste :
- **Contexte client** : situation projet réelle
- **Demande** : cahier des charges
- **Livrables** : ce que tu dois produire
- **Points de vigilance** : pièges à éviter
- **Critères de validation** : checklist qualité

---

## Exercice 1 : Bon de Commande Client (Niveau Junior)

### 📋 Contexte Client

**Entreprise** : LOGIDIS SA - Distribution de matériel industriel  
**Module** : SD (Ventes & Distribution)  
**Interlocuteur** : Marie Dubois, Responsable ADV

**Mail reçu** :
```
Bonjour,

Suite à notre migration SAP, nous avons besoin d'éditer des bons de commande 
pour nos clients. Actuellement, nous utilisons encore des modèles Word, ce 
qui nous fait perdre un temps fou.

Nous voudrions un formulaire SAP professionnel qui reprenne nos informations 
essentielles. Rien de trop compliqué pour commencer.

Merci,
Marie
```

### 🎯 Demande Fonctionnelle

**Formulaire requis** : Bon de commande client simple

**Données à afficher** :
- **Entête** :
  - Logo société (texte pour l'instant)
  - N° commande (VBELN)
  - Date commande (ERDAT)
  - Client : N° (KUNNR) + Nom (NAME1)
  - Adresse complète du client

- **Corps** :
  - Table des articles commandés :
    - N° position (POSNR)
    - Référence article (MATNR)
    - Désignation (ARKTX)
    - Quantité (KWMENG)
    - Unité (VRKME)
    - Prix unitaire HT (NETPR)
    - Total ligne HT

- **Pied** :
  - Total HT
  - TVA (20%)
  - Total TTC
  - Conditions générales de vente (texte fixe)

### 📦 Livrables Attendus

1. **Smart Form** : `ZSF_BON_COMMANDE` 
2. **Programme d'appel** : `ZPRINT_BON_COMMANDE`
3. **Structures SE11** (si nécessaires)
4. **Fiche de test** avec 3 cas :
   - Commande 1 ligne
   - Commande 10 lignes
   - Commande sans articles (erreur)

### ⚠️ Points de Vigilance

- Les données viennent de VBAK (entête commande) et VBAP (postes)
- Jointure avec KNA1 pour les infos client
- Calculer les totaux AVANT d'appeler le Smart Form
- Gérer le cas où la commande n'existe pas
- Format monétaire : 2 décimales, séparateur milliers

### ✅ Critères de Validation

- [ ] Formulaire s'affiche correctement en aperçu
- [ ] Toutes les données sont présentes et cohérentes
- [ ] Calculs des totaux corrects (vérifier manuellement)
- [ ] Gestion d'erreur si commande inexistante
- [ ] Code commenté en français
- [ ] Pas de valeurs en dur
- [ ] Variables locales privilégiées

### 💡 Astuce Tech Lead

En ESN, un "simple" bon de commande est ton premier test. Le client va scruter chaque virgule, chaque alignement. Prends 10 minutes de plus pour aligner proprement les colonnes dans le Smart Form, ça évitera 3 allers-retours.

---

## Exercice 2 : Relevé de Compte Fournisseur (Niveau Junior+)

### 📋 Contexte Client

**Entreprise** : MECANIX Industries - Fabrication de pièces automobiles  
**Module** : FI (Comptabilité Financière)  
**Interlocuteur** : Thomas Leroy, Contrôleur Financier

**Réunion de cadrage** :
```
"On a un problème avec nos fournisseurs. Ils nous réclament constamment des 
justificatifs de paiement. La compta passe son temps à faire des exports Excel 
et à les mettre en forme.

Il nous faut un relevé de compte automatique par fournisseur, sur une période 
donnée, avec le solde. Un truc propre qu'on peut envoyer directement par mail.

Ah, et il faut que ça gère le multidevise, on a des fournisseurs en EUR, USD 
et GBP."
```

### 🎯 Demande Fonctionnelle

**Formulaire requis** : Relevé de compte fournisseur

**Écran de sélection** :
- N° fournisseur (LIFNR) - obligatoire
- Date début (BUDAT) - obligatoire
- Date fin (BUDAT) - obligatoire
- Devise (WAERS) - optionnel (si vide = toutes)

**Données à afficher** :

- **Entête** :
  - Société émettrice
  - Titre : "RELEVÉ DE COMPTE FOURNISSEUR"
  - Période : du XX/XX/XXXX au XX/XX/XXXX
  - Fournisseur : N° + Nom + Adresse
  - Devise(s) traitée(s)

- **Corps - Table des écritures** :
  - Date pièce (BUDAT)
  - N° pièce (BELNR)
  - Type pièce (BLART) avec libellé
  - Référence (XBLNR)
  - Libellé (SGTXT)
  - Débit (DMBTR si SHKZG = 'S')
  - Crédit (DMBTR si SHKZG = 'H')
  - Solde cumulé

- **Pied** :
  - Solde initial période
  - Total débits période
  - Total crédits période
  - Solde final période
  - Signature digitale (date génération + utilisateur)

### 📦 Livrables Attendus

1. **Smart Form** : `ZSF_RELEVE_FOURNISSEUR`
2. **Programme** : `ZFI_RELEVE_FOURNISSEUR` (avec écran sélection)
3. **Structure custom** : `ZSTR_RELEVE_LINE` pour les lignes
4. **Fiche de test** avec 5 cas :
   - Fournisseur avec mouvements (cas nominal)
   - Fournisseur sans mouvement sur période
   - Période invalide (date fin < date début)
   - Fournisseur inexistant
   - Multi-devises (si implémenté)

### ⚠️ Points de Vigilance

- **Tables FI** : BSIK (postes ouverts), BSAK (postes soldés)
- **Calcul du solde** : cumulatif ligne par ligne (LOOP avec accumulateur)
- **Gestion devise** :
  - Si devise spécifiée : filtrer sur WAERS
  - Sinon : grouper par devise et faire un relevé par devise
- **Type de pièce** : joindre T003T pour avoir le libellé du BLART
- **Performance** : 
  - SELECT avec WHERE sur index (LIFNR, BUDAT)
  - Éviter SELECT dans LOOP
- **Tri** : ORDER BY BUDAT ASCENDING obligatoire pour le solde cumulé

### ✅ Critères de Validation

- [ ] Écran de sélection ergonomique avec F4 sur fournisseur
- [ ] Validation des dates (fin >= début)
- [ ] Solde cumulé correct (vérifier manuellement sur 3 lignes minimum)
- [ ] Totaux de pied = somme des lignes
- [ ] Gestion des écritures créditrices ET débitrices
- [ ] Format date : JJ/MM/AAAA
- [ ] Format montant : séparateur milliers + 2 décimales
- [ ] Message si aucune donnée trouvée
- [ ] Code optimisé (pas de SELECT imbriqués)

### 💡 Astuce Tech Lead

Les relevés comptables sont ultra-sensibles. Une erreur de 0,01€ et c'est la guerre avec la compta. Vérifie TOUJOURS tes calculs à la main sur un échantillon avant de livrer. Et documente ta méthode de calcul du solde dans les commentaires.

En mission, j'ai vu un consultant passer 2 semaines sur un bug de solde qui venait d'une mauvaise gestion du SHKZG (débit/crédit inversé). Teste les deux sens.

---

## Exercice 3 : Étiquettes Expédition avec Code-Barres (Niveau Intermédiaire)

### 📋 Contexte Client

**Entreprise** : PHARMALOG - Logistique pharmaceutique  
**Module** : MM/WM (Gestion d'entrepôt)  
**Interlocuteur** : Sophie Martin, Responsable Logistique

**Contexte projet** :
```
URGENT - On passe aux étiquettes code-barres pour la traçabilité réglementaire 
(pharma oblige). L'ANSM nous impose un format spécifique.

On a besoin d'imprimer des étiquettes autocollantes (format 10x5cm) lors de 
la création des ordres de transfert. Chaque palette doit avoir son étiquette 
avec code-barres EAN128.

Le prestataire d'impression nous demande un format PDF précis. C'est pour dans 
2 semaines, on a une inspection ANSM qui arrive.

Projet prioritaire, budget validé.
```

### 🎯 Demande Fonctionnelle

**Formulaire requis** : Étiquette expédition avec code-barres

**Déclencheur** : 
- Création/modification d'un ordre de transfert (TO - Transfer Order)
- Transaction LT03 - possibilité d'impression manuelle

**Format étiquette** : 
- 10cm x 5cm (paysage)
- Papier autocollant compatible imprimante Zebra

**Données à afficher** :

- **Zone 1 - Entête** (20% hauteur) :
  - Logo société
  - Texte : "EXPÉDITION PHARMALOG"

- **Zone 2 - Informations principales** (40% hauteur) :
  - N° Ordre de Transfert (TANUM)
  - Date création (ERDAT)
  - Entrepôt destination (NLPLA)
  - Article (MATNR) - en gros caractères
  - Désignation article (MAKTX)

- **Zone 3 - Quantités** (20% hauteur) :
  - Quantité (BDMNG)
  - Unité (MEINS)
  - N° Lot (CHARG) - si géré en lot
  - Date péremption (VFDAT) - pharmaceutique obligatoire

- **Zone 4 - Code-barres** (20% hauteur) :
  - Code-barres EAN128 généré avec :
    - (01) GTIN article
    - (10) N° lot
    - (17) Date péremption AAMMJJ
    - (37) Quantité

**Contraintes techniques** :
- 1 TO peut avoir plusieurs positions → 1 étiquette par position
- Impression par lot (plusieurs étiquettes d'un coup)
- Export PDF obligatoire pour archivage qualité

### 📦 Livrables Attendus

1. **Smart Form** : `ZWM_ETIQ_EXPEDITION`
   - Format custom 10x5cm
   - Gestion code-barres (via font ou BAPI)
2. **Programme** : `ZWM_PRINT_ETIQUETTES`
   - Écran sélection : N° TO ou plage de TOs
   - Option : Aperçu / Impression directe / Export PDF
3. **Module fonction** : `ZWM_GENERATE_EAN128`
   - Génération code EAN128 selon norme GS1
4. **Documentation** :
   - Format code-barres EAN128 détaillé
   - Paramétrage imprimante Zebra
   - Procédure de test

### ⚠️ Points de Vigilance

- **Tables WM** : 
  - LTAK (entête TO)
  - LTAP (postes TO)
  - MARA/MAKT (articles)
  - MCH1 (lots)
- **Code-barres** :
  - EAN128 = norme GS1 avec identifiants application (AI)
  - Checksum obligatoire
  - Font code-barres : installer sur serveur SAP
- **Format étiquette** :
  - Page custom dans Smart Form (100mm x 50mm)
  - Marges : 2mm tous côtés
  - Orientation : paysage
- **Performance** :
  - Impression en masse : générer PDF globalement, pas 1 par 1
  - Spool intelligent
- **Qualité pharma** :
  - Traçabilité : logger chaque impression (table custom)
  - Réimpression interdite (ou tracer avec motif)

### ✅ Critères de Validation

- [ ] Format étiquette exact (mesurer avec règle sur impression test)
- [ ] Code-barres lisible (tester avec douchette)
- [ ] Toutes les données obligatoires présentes
- [ ] Gestion des articles sans lot (affichage adapté)
- [ ] Gestion date péremption manquante (warning)
- [ ] Export PDF génère 1 fichier multi-pages
- [ ] Log des impressions dans table ZTLOG_ETIQ
- [ ] Test avec 50 étiquettes (performance)
- [ ] Police code-barres installée et fonctionnelle
- [ ] Documentation complète

### 💡 Astuce Tech Lead

**Projet sensible = pharma = réglementaire = ZÉRO ERREUR TOLÉRÉE**

Points critiques :
1. **Code-barres** : Ne JAMAIS improviser la norme EAN128. Utilise une BAPI ou FM existante, sinon tu vas générer des codes invalides.
2. **Traçabilité** : Table de log custom obligatoire. Date/heure/user/TO/nombre étiquettes. L'ANSM peut demander un audit.
3. **Tests** : Imprimer réellement sur l'imprimante cible (pas juste l'aperçu). Les marges peuvent être différentes.
4. **Format** : Demander UN exemple d'étiquette validée par le client AVANT de coder. Ça évitera 10 versions.

Anecdote : J'ai vu un projet bloqué 3 semaines car le code-barres ne passait pas sur les scanners du client. Cause : mauvaise font installée sur le serveur. Vérifie ça en amont.

---

## Exercice 4 : Fiche de Paie Simplifiée (Niveau Intermédiaire+)

### 📋 Contexte Client

**Entreprise** : SERVICES+ - Société de services multi-sites  
**Module** : HR (Ressources Humaines)  
**Interlocuteur** : Nathalie Rousseau, DRH

**Brief projet** :
```
Nous avons un turn-over élevé dans nos équipes terrain. Les employés perdent 
régulièrement leurs bulletins de paie et nous sollicitent pour des rééditions.

Le système actuel (prestataire externe) ne permet pas de rééditer facilement. 
On voudrait internaliser l'édition des fiches de paie dans SAP.

ATTENTION : Pas besoin d'un bulletin complet ultra-complexe pour l'instant. 
On veut juste un document PDF simple avec les éléments essentiels, légal, 
qu'on puisse envoyer par mail de manière sécurisée.

Phase pilote : 50 employés du site de Lyon.
```

### 🎯 Demande Fonctionnelle

**Formulaire requis** : Fiche de paie simplifiée

**Écran de sélection** :
- N° personnel (PERNR) - obligatoire OU
- Plage de N° personnel - pour édition en masse
- Période de paie (BEGDA/ENDDA) - obligatoire
- Mode : Aperçu / Impression / Envoi mail / Export PDF

**Données à afficher** :

- **Entête employeur** :
  - Raison sociale : SERVICES+ SAS
  - Adresse siège social
  - N° SIRET
  - Code NAF
  - N° URSSAF

- **Informations salarié** :
  - Nom prénom (PA0002)
  - N° sécurité sociale (PA0185)
  - Adresse (PA0006)
  - Date d'entrée (PA0000)
  - Qualification (PA0001)
  - Coefficient / Niveau

- **Période et emploi** :
  - Mois de paie
  - Nombre d'heures travaillées
  - Taux horaire
  - Type de contrat (CDI/CDD)

- **Éléments de rémunération** (table) :
  - Libellé (ex: Salaire de base, Heures sup 25%, Prime ancienneté...)
  - Base (nombre d'heures, de jours...)
  - Taux
  - Montant brut
  - Part patronale (pour info)

- **Totaux** :
  - Brut
  - Cotisations salariales
  - Net imposable
  - Net à payer
  - Cotisations patronales (pour info)

- **Pied** :
  - Cumuls annuels (brut, net, imposable)
  - Mentions légales obligatoires
  - Mode de paiement (virement)
  - Congés payés (solde)

**Contraintes légales** :
- Respect format article R.3243-1 du Code du Travail
- Conservation 5 ans (archivage automatique)
- Envoi sécurisé par mail (cryptage PDF optionnel)

### 📦 Livrables Attendus

1. **Smart Form** : `ZHR_FICHE_PAIE`
2. **Programme** : `ZHR_EDITION_PAIE`
   - Écran sélection multi-critères
   - Gestion édition individuelle / masse
   - Fonction envoi mail automatique
3. **Module fonction** : `ZHR_SEND_PAIE_MAIL`
   - Envoi PDF crypté
   - Objet mail paramétrable
   - Log des envois
4. **Table custom** : `ZHR_LOG_PAIE`
   - Historique éditions/envois
   - PERNR, date édition, user, date envoi mail
5. **Documentation** :
   - Mapping des infotypes PA0xxx utilisés
   - Liste des libellés de rubriques de paie
   - Procédure envoi mail
   - Conformité légale (validation DRH)

### ⚠️ Points de Vigilance

- **Infotypes RH** :
  - PA0000 : Actions
  - PA0001 : Affectation organisationnelle
  - PA0002 : Données personnelles
  - PA0006 : Adresses
  - PA0008 : Salaire de base
  - PA0185 : Données individuelles de sécurité sociale
  - PA0041 : Date (pour ancienneté)
- **Rubriques de paie** : 
  - Table RT (résultat paie)
  - Mapping avec libellés clairs (pas de codes bruts)
- **Sécurité** :
  - Autorisation HR stricte (objet P_ORGIN)
  - Cryptage PDF (mot de passe = 6 derniers chiffres sécu sociale)
  - Log exhaustif des accès
- **Performance** :
  - Optimiser si édition > 100 fiches en une fois
  - Job batch pour éditions mensuelles
- **Légal** :
  - Faire valider le format par juriste/expert-comptable
  - Mentions obligatoires complètes

### ✅ Critères de Validation

- [ ] Format conforme Code du Travail (validation DRH/juridique)
- [ ] Toutes les données affichées correctes (test sur 5 vrais salariés)
- [ ] Calculs de totaux exacts (vérifier manuellement)
- [ ] Cumuls annuels corrects
- [ ] PDF généré proprement (police, mise en page)
- [ ] Envoi mail fonctionnel avec PDF joint
- [ ] Cryptage PDF opérationnel
- [ ] Log complet dans table custom
- [ ] Gestion erreur si salarié inexistant
- [ ] Gestion erreur si période invalide
- [ ] Test édition masse (50 fiches)
- [ ] Performance < 5 secondes par fiche
- [ ] Archivage automatique dans serveur de fichiers

### 💡 Astuce Tech Lead

**ATTENTION DANGER : RH + Légal = Terrain miné**

Règles de survie :
1. **JAMAIS** valider toi-même la conformité légale. Ce n'est PAS ton rôle. Tu codes, la DRH et le juriste valident.
2. **Sécurité** : Le HR est le module le plus sensible de SAP. Triple-check les autorisations. Un bug = RGPD violation = sanction CNIL.
3. **Tests** : Utiliser UNIQUEMENT des données anonymisées en DEV. Demander un jeu de test fourni par la DRH.
4. **Communication** : Le RH est hyper-sensible. Formule bien tes mails, reste pro, pas de blagues. Tout peut être mal interprété.

Retour d'XP : J'ai vu un projet RH annulé car le consultant avait testé en DEV avec les vraies données (copiées de PROD). Violation RGPD. Il a été remercié sous 48h.

**Ne JAMAIS** :
- Accéder aux données salariales sans ticket validé DRH
- Garder des fichiers de paie sur ton poste
- Partager des infos RH, même anonymisées, en dehors du projet

---

## Exercice 5 : Certificat de Conformité Multi-Langue (Niveau Avancé)

### 📋 Contexte Client

**Entreprise** : EUROQUALITY - Fabricant de composants électroniques  
**Module** : QM (Gestion de la Qualité)  
**Interlocuteur** : Dr. Klaus Weber, Directeur Qualité (siège Allemagne)

**Contexte international** :
```
EUROQUALITY exports to 15 countries. Each customer requires a Certificate of 
Conformity (CoC) in their language when we ship products.

Currently, quality team manually creates Word documents, it's time-consuming 
and error-prone. We need an automated solution integrated with SAP QM.

Requirements:
- Multi-language support (EN, DE, FR, ES, IT mandatory)
- ISO 9001 compliant format
- Digital signature (QR code with verification URL)
- Automatic data pull from Quality Inspections (QM module)
- PDF/A format for legal archiving (10 years retention)

Critical: German automotive clients require this ASAP. BMW audit in 6 weeks.

Budget approved. This is a corporate priority project.
```

### 🎯 Demande Fonctionnelle

**Formulaire requis** : Certificat de Conformité (CoC) Multi-langue

**Déclencheur** :
- Transaction QE51N - Après validation du lot de contrôle
- Possibilité d'édition manuelle avec QA33

**Écran de sélection** :
- N° Lot de contrôle (PRUEFLOS) - obligatoire
- Langue (SPRAS) : EN / DE / FR / ES / IT
- Mode : Aperçu / Impression / PDF / Email client
- Signature digitale : OUI / NON

**Données à afficher** :

- **Entête** (adapté par langue) :
  ```
  EN: CERTIFICATE OF CONFORMITY
  DE: KONFORMITÄTSERKLÄRUNG
  FR: CERTIFICAT DE CONFORMITÉ
  ES: CERTIFICADO DE CONFORMIDAD
  IT: CERTIFICATO DI CONFORMITÀ
  ```
  - Logo EUROQUALITY
  - N° certificat unique (CERT-YYYY-XXXXXX)
  - Date d'émission
  - ISO 9001:2015 certified

- **Section 1 : Informations Produit** :
  - Référence article (MATNR)
  - Désignation (MAKTX) - traduite selon langue
  - N° Lot fabrication (CHARG)
  - Quantité (BDMNG)
  - Date fabrication
  - N° Commande client (VBELN)

- **Section 2 : Spécifications Techniques** :
  - Norme appliquée (ex: EN 60950, IEC 62368)
  - Caractéristiques contrôlées (table dynamique) :
    - Paramètre (ex: Résistance, Voltage, Température...)
    - Valeur mesurée
    - Tolérance (min/max)
    - Résultat (OK / NOK)
    - Méthode de test (norme référence)

- **Section 3 : Résultat Global** :
  - Statut : CONFORME / NON CONFORME
  - Nombre de tests : X/X réussis
  - Remarques (si anomalies détectées)

- **Section 4 : Signature & Validation** :
  - Nom du responsable qualité
  - Fonction
  - Signature (image scannée)
  - Date et lieu
  - Tampon société (image)

- **Section 5 : QR Code** :
  - QR code contenant :
    - URL de vérification : https://euroquality.com/verify/{cert_number}
    - Hash du document (SHA-256)
  - Texte : "Scan to verify authenticity"

- **Pied de page** :
  - Coordonnées société (siège social)
  - Mentions légales multi-langues
  - "This document is computer generated and valid without signature"
  - Page X/Y

**Contraintes techniques** :
- Format PDF/A-1b (archivage long terme)
- Multilangue : textes depuis SE63 (table de traduction)
- QR code : génération dynamique avec bibliothèque externe
- Signature digitale : optionnelle, si activée = PKI certificate
- Watermark "COPY" si réimpression

### 📦 Livrables Attendus

1. **Smart Form** : `ZQM_COC_CERTIFICATE`
   - Gestion 5 langues
   - Design professionnel (template validé par Marketing)
   - QR code intégré
2. **Programme** : `ZQM_PRINT_COC`
   - Écran sélection multi-critères
   - Validation données avant impression
   - Export PDF/A
3. **Module fonction** : `ZQM_GENERATE_QR_CODE`
   - Génération QR code avec données cryptées
   - Appel bibliothèque QR (ABAP2XLSX ou équivalent)
4. **Table custom** : `ZQM_COC_HEADER`
   - Stockage des certificats émis
   - N° unique, date, lot, user, langue, hash
5. **Web service** : `ZWS_COC_VERIFY` (optionnel avancé)
   - API REST pour vérification en ligne
   - Input : N° certificat
   - Output : JSON avec statut, date, produit
6. **Traductions** : SE63
   - Textes traduits dans les 5 langues
   - Validation par natifs (prestataire externe)
7. **Documentation complète** :
   - Guide utilisateur multi-langue
   - Spécifications techniques QR code
   - Procédure ISO 9001 associée
   - Mapping tables QM (QALS, QAMR, QAVE...)

### ⚠️ Points de Vigilance

- **Tables QM** :
  - QALS : Lots de contrôle (header)
  - QASR : Postes de lot de contrôle
  - QAMR : Résultats de contrôle
  - QAVE : Caractéristiques contrôlées
  - QPCD : Plan de contrôle
- **Multilangue** :
  - Textes dynamiques : table de traduction custom (ZTQM_TEXTS)
  - Textes Smart Form : utiliser éléments translatables
  - Ne JAMAIS hard-coder du texte dans une langue
- **QR Code** :
  - Bibliothèque ABAP : utiliser classe CL_2D_BARCODE (si disponible)
  - Sinon : appel RFC à système externe ou génération image
  - Taille QR : 3x3 cm minimum pour lisibilité
- **PDF/A** :
  - Contraintes : pas de transparence, fonts embarquées, métadonnées
  - Test avec validateur PDF/A (Adobe Acrobat Pro)
- **Performance** :
  - Génération QR code peut être lente (1-2 sec)
  - Optimiser si édition en masse
- **Sécurité** :
  - Hash SHA-256 du PDF complet
  - Stockage hash dans table custom pour vérification
  - Prévenir réédition frauduleuse (watermark)
- **Normes** :
  - Valider format avec organisme certification ISO (externe)
  - Logo ISO : vérifier licence d'utilisation

### ✅ Critères de Validation

- [ ] Certificat généré dans les 5 langues (test 1 lot x 5 langues)
- [ ] Traductions validées par natifs (prestataire)
- [ ] QR code fonctionnel (scanner avec smartphone)
- [ ] URL vérification accessible et fonctionnelle
- [ ] Format PDF/A validé (Adobe Acrobat Preflight)
- [ ] Toutes les données QM présentes et exactes
- [ ] Calculs de conformité corrects (si tests quantitatifs)
- [ ] Signature image bien positionnée
- [ ] Watermark "COPY" si réimpression
- [ ] Numérotation unique des certificats (pas de doublons)
- [ ] Table ZQM_COC_HEADER remplie correctement
- [ ] Performance < 10 secondes (génération + QR code)
- [ ] Archivage PDF dans système GED (optionnel)
- [ ] Log exhaustif des impressions
- [ ] Tests avec données BMW (lots client réels)

### 💡 Astuce Tech Lead

**Projet complexe multi-facettes = Gestion de projet rigoureuse**

Points critiques :

1. **Multilangue** :
   - Budget traduction : cher (100-150€/page x 5 langues)
   - Utilise un prestataire pro, pas Google Translate
   - Fais valider CHAQUE texte par un natif du métier (pas juste un traducteur)
   - Une erreur de traduction technique = crédibilité ruinée

2. **QR Code** :
   - NE PAS réinventer la roue. Cherche une bibliothèque ABAP existante (ABAP2XLSX, ZXing...)
   - Si pas de lib : RFC vers système externe (Node.js, Python...)
   - QR code DOIT être testé avec 10 smartphones différents (Android/iOS)

3. **PDF/A** :
   - Contrainte légale stricte (archivage 10 ans)
   - Génération PDF/A != PDF classique
   - Fonts : inclure les fonts dans le PDF (pas de référence externe)
   - Validateur : investir dans Adobe Acrobat Pro (300€) ou équivalent

4. **ISO / Qualité** :
   - Le Directeur Qualité est ton allié ET ton juge
   - Chaque détail compte (position logo, taille texte...)
   - Demande le template EXACT attendu (maquette Illustrator/InDesign)
   - Fais-le valider étape par étape (ne livre pas tout d'un coup)

5. **Planning** :
   - 6 semaines = court pour un projet de cette envergure
   - Découpe en sprints :
     - Semaine 1-2 : Smart Form version française uniquement
     - Semaine 3 : Multilangue (EN/DE)
     - Semaine 4 : QR code + PDF/A
     - Semaine 5 : Tests + corrections
     - Semaine 6 : UAT (User Acceptance Testing) + Go-Live
   - Prévois 20% de buffer pour les imprévus (toujours des surprises)

**Retour d'XP** : 
J'ai fait un projet similaire pour un client automobile allemand. Points de blocage rencontrés :
- QR code illisible car trop petit (2cm au lieu de 3cm)
- Traduction allemande validée par un Autrichien → refus du client (dialecte différent)
- PDF non conforme PDF/A car font non embarquée → refus archivage légal
- Performance : génération QR code = 8 secondes → inacceptable pour édition masse

Solution finale : 
- QR code en image pré-générée côté serveur (RFC Node.js)
- Traducteur technique spécialisé industrie automobile (trouvé via agence)
- Tests exhaustifs avec Adobe Preflight + validation organisme certification
- Cache pour QR codes déjà générés (table custom)

**Conseil final** : Ce type de projet = visibilité corporate. Si tu réussis, belle ligne sur ton CV. Si tu rates, fin de mission. Sois rigoureux, communique souvent, documente tout.

---

## 📊 Grille d'Évaluation Globale

Utilise cette grille pour auto-évaluer tes exercices :

| Critère | Poids | Note /5 | Commentaire |
|---------|-------|---------|-------------|
| **Fonctionnel** | | | |
| Conformité cahier des charges | 20% | | |
| Données complètes et exactes | 15% | | |
| Calculs corrects | 10% | | |
| Gestion des erreurs | 10% | | |
| **Technique** | | | |
| Performance (temps exec) | 10% | | |
| Code propre et commenté | 10% | | |
| Standards ABAP respectés | 10% | | |
| Optimisation DB (SELECT) | 5% | | |
| **Qualité ESN** | | | |
| Documentation complète | 5% | | |
| Fiche de tests détaillée | 5% | | |
| **TOTAL** | 100% | **/5** | |

**Barème** :
- < 2.5 : Non validable en client
- 2.5 - 3.5 : Acceptable mais à améliorer
- 3.5 - 4.5 : Bon niveau consultant
- > 4.5 : Excellent, niveau senior

---

## 🎯 Parcours Recommandé

**Semaine 1-2** : Exercice 1 (Bon de commande)
→ Objectif : Maîtriser les bases Smart Form + programme appelant

**Semaine 3-4** : Exercice 2 (Relevé fournisseur)
→ Objectif : Gestion données comptables + calculs complexes

**Semaine 5-6** : Exercice 3 (Étiquettes code-barres)
→ Objectif : Format spécial + contraintes techniques (imprimante)

**Semaine 7-9** : Exercice 4 (Fiche de paie)
→ Objectif : Sensibilité RH + conformité légale + sécurité

**Semaine 10-14** : Exercice 5 (Certificat conformité)
→ Objectif : Projet complexe multi-facettes (multilangue, QR, PDF/A...)

**Total** : 14 semaines de formation intensive = niveau opérationnel ESN

---

## 💼 Conseils Finaux ESN

### Communication Client

**Bon réflexe** :
```
"Bonjour Marie,

J'ai bien pris en compte votre demande de bon de commande. 

Avant de démarrer le développement, je vous propose un point de cadrage 
rapide (30 min) pour valider ensemble :
- Le format attendu (avez-vous un exemple papier ?)
- Les données prioritaires
- Le circuit de validation

Cela nous évitera des allers-retours.

Disponibilité : jeudi 14h ou vendredi 10h ?

Cordialement,"
```

**Mauvais réflexe** :
```
"OK je fais ça."
```
→ Résultat : 5 versions différentes, client insatisfait, temps perdu

### Gestion du Temps

**Règle des 3** :
- Estimation initiale x 3 = temps réel
- "Ça prend 2h" → prévoir 6h (tests, corrections, doc)

**Priorisation** :
1. Fonctionnel > Cosmétique
2. Données correctes > Beau design
3. MVP (Minimum Viable Product) > Perfection

### Documentation

**Toujours livrer** :
- [ ] Fiche technique (tables utilisées, logique métier)
- [ ] Fiche de tests (cas testés, résultats, captures)
- [ ] Guide utilisateur (si formulaire complexe)
- [ ] Instructions transport (objets inclus)

**Template fiche technique** :
```markdown
# Fiche Technique - [Nom Programme]

## Contexte
[2-3 lignes]

## Objets SAP
- Programme : ZXXX
- Smart Form : ZXXX
- Structures : ZXXX
- Tables custom : ZXXX

## Tables lues
- VBAK : Entêtes commandes
- VBAP : Postes commandes
- KNA1 : Clients

## Logique métier
1. Récupération données commande
2. Calcul totaux
3. Appel Smart Form

## Points d'attention
- Performance : éviter FOR ALL ENTRIES si > 10k lignes
- Autorisation : objet V_VBAK_VKO obligatoire

## Tests réalisés
[Voir fiche de tests séparée]
```

---

**Bonne chance dans tes exercices ! N'hésite pas si tu bloques.**
