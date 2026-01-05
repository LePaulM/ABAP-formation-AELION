*&---------------------------------------------------------------------*
*& Include Z_POEC_PMI_SCR
*&---------------------------------------------------------------------*
*& Écran de sélection
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_ebeln FOR gv_ebeln_ref.
  SELECT-OPTIONS: s_matnr FOR gv_matnr_ref.
SELECTION-SCREEN END OF BLOCK b1.

" TEXTES :
" TEXT-001 : Selection parameters
