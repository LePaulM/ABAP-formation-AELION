*&---------------------------------------------------------------------*
*& Include Z_POEC_INTEG_PMI_SCR
*&---------------------------------------------------------------------*
*& Écran de sélection
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_file TYPE rlgrap-filename OBLIGATORY.
  PARAMETERS: p_test AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

" ===== ÉVÉNEMENTS ÉCRAN =====

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  " Je propose l'aide à la sélection de fichier
  PERFORM f_browse_file.

" TEXTES :
" TEXT-001 : Paramètres
