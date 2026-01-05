*&---------------------------------------------------------------------*
*& Report Z_POEC_PMI
*&---------------------------------------------------------------------*
*& Programme de restitution des commandes d'achat
*&---------------------------------------------------------------------*
REPORT z_poec_pmi.

" ===== INCLUDES =====
INCLUDE z_poec_pmi_top. " Déclarations globales
INCLUDE z_poec_pmi_scr. " Écran de sélection
INCLUDE z_poec_pmi_f01. " Formes de traitement

" ===== ÉVÉNEMENTS =====

AT SELECTION-SCREEN.
  " Je valide le purchasing document saisi
  PERFORM f_check_ebeln.

START-OF-SELECTION.
  " Je récupère les données
  PERFORM f_get_data.

  " Je vérifie qu'on a des données
  IF gt_header IS INITIAL.
    MESSAGE 'Aucune donnée à afficher' TYPE 'S'.
    STOP.
  ENDIF.

  " J'appelle l'écran dynpro pour afficher les ALV
  CALL SCREEN 100.

*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_100'.
  SET TITLEBAR 'TITLE_100'.

  " J'affiche les ALV
  PERFORM f_display_alv.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module USER_COMMAND_0100 INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      " Je libère les objets
      IF gv_alv_header IS BOUND.
        CALL METHOD gv_alv_header->free.
        FREE gv_alv_header.
      ENDIF.
      IF gv_alv_items IS BOUND.
        CALL METHOD gv_alv_items->free.
        FREE gv_alv_items.
      ENDIF.
      IF gv_splitter IS BOUND.
        CALL METHOD gv_splitter->free.
        FREE gv_splitter.
      ENDIF.
      FREE go_event_handler.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
