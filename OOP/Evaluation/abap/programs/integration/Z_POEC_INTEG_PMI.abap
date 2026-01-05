*&---------------------------------------------------------------------*
*& Report Z_POEC_INTEG_PMI
*&---------------------------------------------------------------------*
*& Programme d'intégration des commandes d'achat depuis fichier texte
*&---------------------------------------------------------------------*
REPORT z_poec_integ_pmi.

" ===== INCLUDES =====
INCLUDE z_poec_integ_pmi_top. " Déclarations globales
INCLUDE z_poec_integ_pmi_scr. " Écran de sélection
INCLUDE z_poec_integ_pmi_f01. " Formes de traitement

" ===== POINT D'ENTRÉE =====
START-OF-SELECTION.
  PERFORM f_main_process.
