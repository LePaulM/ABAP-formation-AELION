*&---------------------------------------------------------------------*
*& Include Z_POEC_INTEG_PMI_TOP
*&---------------------------------------------------------------------*
*& Déclarations globales : types, tables internes, variables
*&---------------------------------------------------------------------*

" ===== TYPES DE DONNÉES =====

" Structure pour lecture du fichier (ligne brute)
TYPES: BEGIN OF ty_file_line,
         line TYPE string,
       END OF ty_file_line.

" Structure pour données après découpage
TYPES: BEGIN OF ty_data,
         ebeln TYPE ebeln,
         bstyp TYPE bstyp,
         aedat TYPE aedat,
         ernam TYPE ernam,
         waers TYPE waers,
         ebelp TYPE ebelp,
         matnr TYPE matnr,
         werks TYPE werks_d,
         menge TYPE bstmg,
         netpr TYPE bprei,
         netwr TYPE bwert,
         meins TYPE bstme,
       END OF ty_data.

" Structure pour compte-rendu d'exécution
TYPES: BEGIN OF ty_log,
         ebeln  TYPE ebeln,
         status TYPE char10,
         color  TYPE char4,
       END OF ty_log.

" ===== TABLES INTERNES GLOBALES =====

DATA: gt_file_lines TYPE TABLE OF ty_file_line,
      gt_data       TYPE TABLE OF ty_data,
      gt_header     TYPE TABLE OF zekko_pmi,
      gt_items      TYPE TABLE OF zekpo_pmi,
      gt_log        TYPE TABLE OF ty_log.
