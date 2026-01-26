# EXERCICE PRATIQUE : DISTRIBUTION D'ARTICLES AVEC IDOC MATMAS05

**Formation SAP - Module MM/ALE**

## CONTEXTE BUSINESS

Votre entreprise possède deux systèmes SAP : un système central (DEV001) qui gère la création des articles, et un système satellite (PROD002) qui doit recevoir automatiquement les données articles pour la production. Vous devez configurer et tester la distribution des données articles via IDoc MATMAS05.

## OBJECTIFS D'APPRENTISSAGE

À la fin de cet exercice, vous serez capable de :
- Comprendre la structure du type de base MATMAS05
- Configurer un scénario de distribution ALE
- Créer et analyser un IDoc MATMAS
- Diagnostiquer et résoudre des erreurs courantes
- Utiliser les change pointers pour la distribution automatique

---

## PARTIE 1 : ANALYSE DE LA STRUCTURE MATMAS05

### Exercice 1.1 - Découverte des segments

**Transaction : WE60**

1. Entrez le type de base : MATMAS05
2. Cliquez sur "HTML Format"
3. Développez l'arborescence des segments

**Questions :**

1. Combien de segments principaux contient MATMAS05 ?

Réponse : _________________________________

2. Identifiez les segments suivants et indiquez leur description :

| Segment | Description | Table SAP source |
|---------|-------------|------------------|
| E1MARAM |             |                  |
| E1MAKTM |             |                  |
| E1MARCM |             |                  |
| E1MARMM |             |                  |
| E1MBEWM |             |                  |
| E1MVKEM |             |                  |

3. Quel segment est obligatoire pour transférer un article ?

Réponse : _________________________________

### Exercice 1.2 - Analyse du segment E1MARAM

**Transaction : WE60** → Double-cliquez sur le segment E1MARAM

**Questions :**

1. Combien de champs contient le segment E1MARAM ?

Réponse : _________________________________

2. Identifiez les champs suivants :

| Nom du champ | Description | Longueur | Type |
|--------------|-------------|----------|------|
| MSGFN        |             |          |      |
| MATNR        |             |          |      |
| MTART        |             |          |      |
| MBRSH        |             |          |      |
| MEINS        |             |          |      |

3. Que signifie le champ MSGFN ? Quelles sont ses valeurs possibles ?

Réponse : ________________________________________________________________

________________________________________________________________________

---

## PARTIE 2 : CONFIGURATION DU SCÉNARIO DE DISTRIBUTION

### Exercice 2.1 - Vérification des systèmes logiques

**Transaction : BD54**

Visualisez le système logique assigné à votre système client.

**Relevé d'informations :**

| Information | Valeur |
|-------------|--------|
| Mandant actuel |      |
| Système logique assigné |  |

**Transaction : SALE** → Sending and Receiving Systems → Logical Systems

Vérifiez que les systèmes logiques suivants existent :

| Système logique | Description | Existe ? |
|-----------------|-------------|----------|
| Émetteur (source) |           | ☐ Oui ☐ Non |
| Récepteur (cible) |           | ☐ Oui ☐ Non |

**Note :** Si les systèmes logiques n'existent pas, contactez votre administrateur Basis.

### Exercice 2.2 - Création du modèle de distribution

**Transaction : BD64**

**Étape 1 : Créer ou modifier un modèle**

1. Cliquez sur "Change Model" (ou créez-en un nouveau)
2. Nom du modèle : ZMATMAS_FORMATION
3. Description : Distribution articles - Formation

**Étape 2 : Ajouter le type de message MATMAS**

1. Positionnez-vous sur votre modèle
2. Menu : Edit → Add message type
3. Renseignez les informations suivantes :

| Champ | Valeur |
|-------|--------|
| Sender | [Votre système logique source] |
| Receiver | [Système logique cible] |
| Message type | MATMAS |

4. Validez et sauvegardez

**Étape 3 : Générer les profils partenaires**

1. Menu : Environment → Generate partner profiles
2. Cochez votre modèle
3. Exécutez

**Vérification :**

☐ Le modèle de distribution est créé  
☐ La ligne MATMAS apparaît avec émetteur et récepteur  
☐ Les profils partenaires ont été générés (message de confirmation)

**Relevé d'informations :**

Système logique émetteur : ________________________________________

Système logique récepteur : ________________________________________

### Exercice 2.3 - Configuration du port (optionnel)

**Transaction : WE21**

**Note :** Cette étape n'est nécessaire que si vous devez créer un nouveau port. Sinon, passez à l'exercice suivant.

**Création d'un port tRFC :**

1. Sélectionnez "Transactional RFC" dans le menu de gauche
2. Bouton "Create"
3. Renseignez :

| Champ | Valeur |
|-------|--------|
| Port | IDOC_MATMAS_FORM |
| Description | Distribution MATMAS formation |
| RFC destination | [Votre destination RFC] |

4. Sauvegardez

### Exercice 2.4 - Vérification du profil partenaire

**Transaction : WE20**

**Étape 1 : Accéder au profil partenaire**

1. Développez "Partner type LS" (Logical System)
2. Double-cliquez sur votre système logique récepteur

**Étape 2 : Vérifier les paramètres sortants**

Développez "Outbound parameters" et vérifiez qu'il existe une ligne pour MATMAS :

**Relevé de configuration :**

| Paramètre | Valeur configurée |
|-----------|-------------------|
| Message type | MATMAS |
| Basic type | MATMAS05 |
| Port |  |
| Process immediately | ☐ Oui ☐ Non |
| Transfer IDoc immediately | ☐ Oui ☐ Non |

**Conseils pour la formation :**
- Process immediately : Coché (pour voir les résultats immédiatement)
- Transfer IDoc immediately : Coché (pour éviter d'attendre le job périodique)

**Vérification finale :**

☐ Le profil partenaire existe pour le système récepteur  
☐ Le type de message MATMAS est configuré  
☐ Le type de base MATMAS05 est renseigné  
☐ Le port est configuré (si nécessaire)

---

## PARTIE 3 : GÉNÉRATION ET TEST D'IDOCS

### Exercice 3.1 - Création d'un article test

**Transaction : MM01**

**Données générales :**

| Champ | Valeur à saisir |
|-------|-----------------|
| Material number | FORM_MATMAS_[VOS_INITIALES] |
| Material type | FERT (Finished product) |
| Industry sector | Sélectionnez un secteur actif |
| Continue | Appuyez sur Entrée |

**Sélection des vues :**

Sélectionnez les vues suivantes :

☐ Basic Data 1  
☐ Basic Data 2  
☐ Classification  
☐ Accounting 1  
☐ Sales: Sales org. 1

**Sélection des niveaux organisationnels :**

| Niveau | Valeur |
|--------|--------|
| Plant | 1000 |
| Storage location | 0001 |
| Sales organization |  |
| Distribution channel |  |
| Sales type |  |

**Données à renseigner :**

**Vue Basic Data 1 :**

| Champ | Valeur |
|-------|--------|
| Short text | Article formation IDoc MATMAS |
| Base unit | PC |
| Material group | Sélectionnez-en un |

**Vue Accounting 1 :**

| Champ | Valeur |
|-------|--------|
| Standard price | 100.00 |
| Currency | EUR |

**Vue Sales: Sales org. 1 :**

| Champ | Valeur |
|-------|--------|
| Tax classification | Sélectionnez-en un |

**Sauvegardez l'article**

**Relevé d'informations :**

Numéro d'article créé : ___________________________________________

Date de création : _____/_____/_________

Message système : ____________________________________________________

### Exercice 3.2 - Génération de l'IDoc avec BD10

**Transaction : BD10**

**Paramètres de sélection :**

| Champ | Valeur |
|-------|--------|
| Message type | MATMAS |
| Material from | [Votre article FORM_MATMAS_XXX] |
| Material to | [Votre article FORM_MATMAS_XXX] |
| Logical system | [Système cible - IMPORTANT !] |

**Options :**

☑ Send material in full

**Exécutez (F8)**

**CAS PRATIQUE : Analyse des messages système**

Après l'exécution de BD10, vous pouvez recevoir deux pop-ups successifs :

**Message 1 :** "1 master IDocs set up for message type MATMAS"

**Message 2 :** "0 communication IDoc(s) generated for message type MATMAS"

**Question :** Que signifient ces deux messages ?

Réponse : ________________________________________________________________

________________________________________________________________________

________________________________________________________________________

**Réponse attendue :**
- Message 1 (1 master IDoc) = Un IDoc intermédiaire a été créé avec succès
- Message 2 (0 communication IDoc) = Aucun IDoc de communication n'a été généré car le modèle de distribution n'est pas complet ou le système logique récepteur n'a pas été renseigné dans BD10

**Solution :**
1. Vérifier que le champ "Logical system" est bien rempli dans BD10
2. Vérifier le modèle de distribution dans BD64
3. Vérifier le profil partenaire dans WE20
4. Relancer BD10 avec tous les paramètres corrects

---

**Résultat attendu après correction :**

Message système : "1 communication IDoc(s) generated for message type MATMAS"

**Relevé d'informations :**

Nombre d'IDocs générés : __________

Message système reçu : ____________________________________________

Date et heure de génération : _____/_____/_________ à ______:______

---

### Exercice 3.3 - Analyse de l'IDoc créé

**Transaction : WE02** ou **WE05** (recommandée)

**Critères de recherche dans WE05 :**

| Critère | Valeur |
|---------|--------|
| Direction | ☑ Outbound |
| Basic type | MATMAS05 |
| Created on | Aujourd'hui |
| Time created | Dernières 2 heures |

**Exécutez (F8)**

---

**Question 1 : Identification de l'IDoc**

Quel est le numéro de l'IDoc créé ?

Numéro IDoc : _______________________________

---

**Question 2 : Vérification du statut**

Double-cliquez sur l'IDoc pour afficher le détail.

Regardez la section "Status records" en bas de l'écran.

Statut actuel : __________

Heure : ______:______

**Interprétation des statuts :**

| Statut | Signification |
|--------|---------------|
| 01 | IDoc généré (Master IDoc) |
| 02 | Erreur lors du passage au port |
| 03 | Données transmises au port OK ✓ |
| 30 | IDoc prêt pour envoi (service ALE) |

Que signifie votre statut ?

Réponse : ________________________________________________________________

---

**Question 3 : Analyse de la structure**

Dans l'écran de détail de l'IDoc, vous voyez une arborescence de segments.

Pour voir tous les segments : Menu → Edit → Segment list

Nombre total de segments : __________

---

**Question 4 : Identification des segments remplis**

Développez l'arborescence et cochez les segments qui contiennent des données :

☐ E1MARAM - Données générales article (MARA)  
☐ E1MAKTM - Textes courts (MAKT)  
☐ E1MARCM - Données centre (MARC)  
☐ E1MARMM - Unités de mesure (MARM)  
☐ E1MBEWM - Valorisation (MBEW)  
☐ E1MVKEM - Données vente (MVKE)  
☐ E1MLANM - Classification fiscale (MLAN)

**Note :** Un segment rempli a un triangle développable devant lui.

---

**Question 5 : Analyse détaillée du segment E1MARAM**

Cliquez sur le segment E1MARAM dans l'arborescence.

Les données apparaissent à droite sous forme de paires Champ / Valeur.

**Relevez les valeurs suivantes :**

| Champ | Description | Votre valeur |
|-------|-------------|--------------|
| MSGFN | Fonction message |  |
| MATNR | Numéro d'article |  |
| MTART | Type d'article |  |
| MBRSH | Branche |  |
| MEINS | Unité de base |  |
| MATKL | Groupe de marchandises |  |
| BISMT | Ancien numéro |  |

**Valeurs attendues pour MSGFN :**
- 001 = Création
- 004 = Modification
- 009 = Suppression

---

**Question 6 : Analyse du segment E1MAKTM**

Cliquez sur le segment E1MAKTM

| Champ | Description | Votre valeur |
|-------|-------------|--------------|
| MSGFN | Fonction message |  |
| SPRAS | Langue |  |
| MAKTX | Texte court |  |

---

**Question 7 : Analyse du segment E1MARCM**

Si ce segment existe, cliquez sur E1MARCM

| Champ | Description | Votre valeur |
|-------|-------------|--------------|
| MSGFN | Fonction message |  |
| WERKS | Centre |  |
| MMSTA | Statut article |  |
| DISMM | Caractéristique MRP |  |

**Question 8 : Analyse du segment E1MARMM**

Cliquez sur E1MARMM (peut avoir plusieurs occurrences)

Nombre d'unités de mesure : __________

**Pour chaque unité :**

| N° | MEINH (Unité) | UMREZ (Numérateur) | UMREN (Dénominateur) |
|----|---------------|--------------------|----------------------|
| 1  |               |                    |                      |
| 2  |               |                    |                      |
| 3  |               |                    |                      |

**Fonctions utiles à tester :**

☐ Menu → Edit → Segment list (vue d'ensemble)  
☐ Menu → Edit → Find (chercher une valeur)  
☐ Menu → IDoc → Display data records (vue tabulaire)  
☐ Menu → Environment → Application document (voir l'article dans MM03)

---

## PARTIE 4 : DIAGNOSTIC ET RÉSOLUTION D'ERREURS

### Exercice 4.1 - Identification des statuts d'erreur

**Scénarios d'erreur courants**

Pour chaque scénario, identifiez le statut IDoc et la solution :

**Scénario A : Erreur de port**

Statut IDoc : 02

Message : "Error passing data to port"

**Causes possibles :**

☐ Le port n'existe pas dans WE21  
☐ Le port n'est pas renseigné dans le profil partenaire (WE20)  
☐ La destination RFC est incorrecte  
☐ Le système cible n'est pas accessible

**Action corrective :**

Réponse : ________________________________________________________________

________________________________________________________________________

**Scénario B : Erreur syntaxique**

Statut IDoc : 26 (sortie) ou 60 (entrée)

Message : "Error in IDoc data syntax check"

**Causes possibles :**

☐ Données manquantes dans un champ obligatoire  
☐ Format de données incorrect  
☐ Longueur de champ dépassée  
☐ Type de base incompatible

**Action corrective :**

Réponse : ________________________________________________________________

________________________________________________________________________

**Scénario C : Erreur application (système cible)**

Statut IDoc : 51

Message : "Application document not posted"

**Causes possibles :**

☐ Données incohérentes dans le système cible  
☐ Paramétrage incomplet dans le système cible  
☐ Article déjà existant (si MSGFN = 001)  
☐ Autorisation manquante dans le système cible

**Action corrective :**

Réponse : ________________________________________________________________

________________________________________________________________________

**Scénario D : IDoc en attente**

Statut IDoc : 30

Message : "IDoc ready for dispatch (ALE service)"

**Explication :**

Ce n'est PAS une erreur ! L'IDoc est en attente d'envoi par le job périodique.

**Action pour envoi immédiat :**

Réponse : ________________________________________________________________

________________________________________________________________________

### Exercice 4.2 - Retraitement d'IDocs en erreur

**Transaction : BD87**

Cette transaction permet de retraiter des IDocs en masse.

**Scénario pratique :**

Supposons que vous avez 5 IDocs en statut 51 (erreur application).

**Procédure de retraitement :**

1. Ouvrez la transaction BD87
2. Entrez les critères :

| Critère | Valeur |
|---------|--------|
| Select IDocs | By creation date |
| Date | Aujourd'hui |
| Basic type | MATMAS05 |
| Direction | Inbound |
| Status | 51 |

3. Exécutez
4. Sélectionnez les IDocs à retraiter
5. Menu : Edit → Process

**Questions :**

Combien d'IDocs ont été retraités avec succès ? __________

Nouveau statut après retraitement : __________

---

## PARTIE 5 : DISTRIBUTION AUTOMATIQUE AVEC CHANGE POINTERS

### Exercice 5.1 - Activation des change pointers

**Transaction : BD50**

**Objectif :** Activer la génération automatique d'IDocs lors de modifications d'articles.

**Procédure :**

1. Ouvrez BD50
2. Recherchez le type d'objet : MATERIAL
3. Double-cliquez pour afficher les détails

**Activation :**

| Type d'objet | Code fonction | Message | Actif ? |
|--------------|---------------|---------|---------|
| MATERIAL | MATMAS | MATMAS | ☐ À cocher |

4. Cochez la case "Active" si elle ne l'est pas
5. Sauvegardez

**Vérification :**

☐ Le change pointer est actif pour MATERIAL / MATMAS  
☐ Un message de confirmation apparaît

### Exercice 5.2 - Activation générale des change pointers

**Transaction : BD61**

Cette transaction active/désactive globalement les change pointers.

**Vérification :**

☐ Change pointers activés au niveau général

**Si désactivés :**

1. Cliquez sur "Activate change pointers"
2. Sauvegardez

**IMPORTANT :** Sans cette activation globale, aucun change pointer ne sera créé même si BD50 est configuré.

---

### Exercice 5.3 - Modification d'article et génération automatique

**Transaction : MM02**

**Procédure :**

1. Ouvrez votre article FORM_MATMAS_XXX en modification
2. Modifiez les éléments suivants :

| Modification | Ancienne valeur | Nouvelle valeur |
|--------------|-----------------|-----------------|
| Short text | Article formation IDoc MATMAS | Article formation IDoc - Modifié |
| Standard price | 100.00 | 150.00 |

3. Ajoutez une nouvelle unité de mesure :

| Unité | Numérateur | Dénominateur |
|-------|------------|--------------|
| CTN (Carton) | 10 | 1 |

4. Sauvegardez

**Relevé :**

Date et heure de modification : _____/_____/_________ à ______:______

---

### Exercice 5.4 - Vérification des change pointers créés

**Transaction : BD21**

**Paramètres de sélection :**

| Champ | Valeur |
|-------|--------|
| Object type | MATERIAL |
| Message type | MATMAS |
| Date | Aujourd'hui |

**Exécutez (F8)**

**Analyse :**

Nombre de change pointers créés : __________

**Pour chaque change pointer, relevez :**

| N° | Change document | Objet | Heure création |
|----|-----------------|-------|----------------|
| 1  |                 |       |                |
| 2  |                 |       |                |

**Sélectionnez vos change pointers** (cochez les lignes)

### Exercice 5.5 - Génération des IDocs à partir des change pointers

**Toujours dans BD21**

**Procédure :**

1. Sélectionnez vos change pointers (cochez les lignes)
2. Menu : Edit → Create IDocs

**Paramètres de création :**

| Paramètre | Valeur |
|-----------|--------|
| Background processing | ☐ Non (pour voir le résultat immédiatement) |
| Reduce IDocs | ☐ Oui (recommandé) |

3. Exécutez

**Résultat :**

Message système : ____________________________________________________

Nombre d'IDocs générés : __________

---

### Exercice 5.6 - Analyse de l'IDoc de modification

**Transaction : WE05**

**Recherchez l'IDoc nouvellement créé :**

| Critère | Valeur |
|---------|--------|
| Direction | Outbound |
| Basic type | MATMAS05 |
| Date | Aujourd'hui |
| Time | Dernière heure |

**Questions :**

1. Numéro du nouvel IDoc : ____________________________________________

2. Statut : __________

3. Dans le segment E1MARAM, quelle est la valeur de MSGFN ? __________

**Interprétation :**
- Si MSGFN = 004 → Modification ✓
- Si MSGFN = 001 → Création (incorrect)

4. Comparez avec l'IDoc précédent (celui de la création) :

**Différences identifiées :**

| Segment | Champ | IDoc création | IDoc modification |
|---------|-------|---------------|-------------------|
| E1MAKTM | MAKTX |               |                   |
| E1MBEWM | STPRS |               |                   |
| E1MARMM | Nouvelle ligne CTN | Non présent | Présent ? |

5. Combien de segments E1MARMM contient ce nouvel IDoc ? __________

**Explication :**

________________________________________________________________________

________________________________________________________________________

---

## PARTIE 6 : QUESTIONS DE SYNTHÈSE

### Question 1 : Différence entre IDocs sortants et entrants

**Question :**

Quelle est la différence entre un IDoc sortant avec statut 03 et un IDoc entrant avec statut 53 ?

**Réponse :**

________________________________________________________________________

________________________________________________________________________

________________________________________________________________________

________________________________________________________________________

### Question 2 : Transactions essentielles

**Question :**

Citez 5 transactions essentielles pour le monitoring et la gestion des IDocs MATMAS :

1. ______________________________ : ____________________________________

2. ______________________________ : ____________________________________

3. ______________________________ : ____________________________________

4. ______________________________ : ____________________________________

5. ______________________________ : ____________________________________

### Question 3 : BD10 vs BD21

**Question :**

Dans quel cas utiliseriez-vous BD10 plutôt que BD21, et inversement ?

**Réponse :**

________________________________________________________________________

________________________________________________________________________

________________________________________________________________________

________________________________________________________________________

### Question 4 : Prérequis pour le traitement d'IDocs

**Question :**

Quels sont les prérequis pour qu'un IDoc MATMAS soit correctement traité dans le système cible ?

**Réponse :**

☐ ______________________________________________________________________

☐ ______________________________________________________________________

☐ ______________________________________________________________________

☐ ______________________________________________________________________

☐ ______________________________________________________________________

☐ ______________________________________________________________________

### Question 5 : Diagnostic d'erreur

**Question :**

Vous recevez un IDoc avec le statut 51 dans le système cible. Le message d'erreur indique : "Material group ZTEST unknown".

Quelle est la procédure complète pour résoudre ce problème ?

**Réponse (étapes numérotées) :**

1. _____________________________________________________________________

2. _____________________________________________________________________

3. _____________________________________________________________________

4. _____________________________________________________________________

5. _____________________________________________________________________

---

## PARTIE 7 : CAS PRATIQUE AVANCÉ (BONUS)

### Exercice 7.1 - Filtrage de la distribution

**Objectif :** Ne distribuer que les articles d'un certain type (ex: FERT) vers le système cible.

**Transaction : BD64**

**Procédure :**

1. Éditez votre modèle de distribution
2. Sélectionnez la ligne MATMAS
3. Bouton "Filter"
4. Ajoutez un filtre :

| Type de filtre | Objet | Valeur |
|----------------|-------|--------|
| Filter on field | MTART (Material type) | FERT |

5. Sauvegardez

**Test :**

Créez un article de type ROH (Matière première) et tentez de le distribuer avec BD10.

**Résultat attendu :**

________________________________________________________________________

________________________________________________________________________

---

### Exercice 7.2 - Programmation ABAP (Avancé)

**Objectif :** Créer un programme qui génère automatiquement un IDoc MATMAS.

**Transaction : SE38**

**Programme exemple (à compléter) :**

```abap
REPORT z_generate_matmas_idoc.

DATA: lv_matnr TYPE matnr,
      lt_idocs TYPE TABLE OF edidc,
      lv_error TYPE string.

PARAMETERS: p_matnr TYPE matnr OBLIGATORY.

START-OF-SELECTION.

  lv_matnr = p_matnr.

  " Appel du module fonction pour générer l'IDoc
  CALL FUNCTION 'MASTER_IDOC_DISTRIBUTE'
    EXPORTING
      input_method     = 'BD10'
      mass_processing  = ' '
    TABLES
      idoc_list        = lt_idocs
    EXCEPTIONS
      error_message_send = 1
      OTHERS           = 2.

  IF sy-subrc = 0.
    LOOP AT lt_idocs INTO DATA(ls_idoc).
      WRITE: / 'IDoc généré:', ls_idoc-docnum.
    ENDLOOP.
  ELSE.
    WRITE: / 'Erreur lors de la génération'.
  ENDIF.
```

**Questions :**

1. Quelle fonction module est utilisée pour générer l'IDoc ?

Réponse : ____________________________________________________________

2. Comment récupérer la liste des IDocs générés ?

Réponse : ____________________________________________________________

________________________________________________________________________

3. Testez ce programme avec votre article. Quel est le numéro d'IDoc généré ?

Réponse : ____________________________________________________________

---

## CHECKLIST DE VALIDATION FINALE

**Validation des connaissances théoriques :**

☐ Je comprends la structure du type de base MATMAS05  
☐ Je connais les principaux segments et leur contenu  
☐ Je sais interpréter les statuts d'IDocs  
☐ Je comprends la différence entre Master IDoc et Communication IDoc  
☐ Je connais le rôle des change pointers

**Validation des compétences techniques :**

☐ Je sais configurer un modèle de distribution (BD64)  
☐ Je sais vérifier/créer un profil partenaire (WE20)  
☐ Je sais générer des IDocs manuellement (BD10)  
☐ Je sais activer et utiliser les change pointers (BD50, BD21)  
☐ Je sais analyser un IDoc dans WE02/WE05  
☐ Je sais diagnostiquer les erreurs courantes  
☐ Je sais retraiter des IDocs en erreur (BD87, WE19)

**Validation pratique :**

☐ J'ai créé un article avec MM01  
☐ J'ai généré un IDoc avec BD10 (statut 03 ou 30)  
☐ J'ai analysé la structure complète de l'IDoc  
☐ J'ai modifié l'article et généré un IDoc via BD21  
☐ J'ai comparé l'IDoc de création et l'IDoc de modification  
☐ J'ai résolu au moins un cas d'erreur

---

## ANNEXES

### Annexe A : Principales transactions IDocs

| Transaction | Description | Usage |
|-------------|-------------|-------|
| WE02 | Display IDoc | Consultation simple |
| WE05 | IDoc lists | Consultation avancée avec filtres |
| WE19 | Test tool | Modification et test d'IDocs |
| WE20 | Partner profiles | Configuration des partenaires |
| WE21 | Ports in IDoc processing | Configuration des ports |
| WE30 | IDoc development | Création de types d'IDocs |
| WE60 | IDoc type documentation | Consultation structure |
| BD10 | Send material master | Génération manuelle MATMAS |
| BD12 | Send material master (extended) | Génération avec plus d'options |
| BD21 | Create IDocs from change pointers | Distribution automatique |
|
