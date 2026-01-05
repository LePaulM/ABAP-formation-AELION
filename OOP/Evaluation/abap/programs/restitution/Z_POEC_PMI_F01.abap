*&---------------------------------------------------------------------*
*& Include Z_POEC_PMI_F01
*&---------------------------------------------------------------------*
*& Formes de traitement
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form f_check_ebeln
*&---------------------------------------------------------------------*
FORM f_check_ebeln.
  " Variables locales
  DATA: lv_ebeln TYPE ebeln,
        lv_count TYPE i.

  " Je vérifie uniquement si un purchasing document est saisi
  READ TABLE s_ebeln INDEX 1 TRANSPORTING NO FIELDS.
  CHECK sy-subrc = 0.

  " Je récupère la première valeur low
  READ TABLE s_ebeln INDEX 1 INTO DATA(ls_ebeln).
  CHECK ls_ebeln-low IS NOT INITIAL.

  lv_ebeln = ls_ebeln-low.

  " Je vérifie l'existence dans la table
  SELECT COUNT(*) FROM zekko_pmi
    INTO lv_count
    WHERE ebeln = lv_ebeln.

  IF lv_count = 0.
    MESSAGE e001(00) WITH 'Purchasing document' lv_ebeln 'not found in table ZEKKO_PMI'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_get_data
*&---------------------------------------------------------------------*
FORM f_get_data.
  " Variables locales avec les bons types
  DATA: ls_header TYPE zcl_event_handler_pmi=>ty_header,
        ls_item   TYPE zcl_event_handler_pmi=>ty_item.

  " Je sélectionne les entêtes selon les critères
  IF s_ebeln[] IS INITIAL.
    SELECT ebeln bstyp aedat ernam waers
      FROM zekko_pmi
      INTO ls_header.
      APPEND ls_header TO gt_header.
    ENDSELECT.
  ELSE.
    SELECT ebeln bstyp aedat ernam waers
      FROM zekko_pmi
      INTO ls_header
      WHERE ebeln IN s_ebeln.
      APPEND ls_header TO gt_header.
    ENDSELECT.
  ENDIF.

  IF gt_header IS INITIAL.
    MESSAGE 'Aucune donnée trouvée' TYPE 'S'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  " Je sélectionne les postes correspondants
  IF gt_header IS NOT INITIAL.
    IF s_matnr[] IS INITIAL.
      SELECT ebeln ebelp matnr werks menge netpr netwr meins
        FROM zekpo_pmi
        INTO ls_item
        FOR ALL ENTRIES IN gt_header
        WHERE ebeln = gt_header-ebeln.
        APPEND ls_item TO gt_items.
      ENDSELECT.
    ELSE.
      SELECT ebeln ebelp matnr werks menge netpr netwr meins
        FROM zekpo_pmi
        INTO ls_item
        FOR ALL ENTRIES IN gt_header
        WHERE ebeln = gt_header-ebeln
          AND matnr IN s_matnr.
        APPEND ls_item TO gt_items.
      ENDSELECT.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_display_alv
*&---------------------------------------------------------------------*
FORM f_display_alv.
  " Je vérifie si c'est le premier affichage
  IF gv_alv_header IS INITIAL.
    " Premier affichage : je crée les containers et ALV
    PERFORM f_create_containers.
    PERFORM f_create_alv_header.
    PERFORM f_create_alv_items.
    PERFORM f_create_event_handler.
  ELSE.
    " Rafraîchissement : je mets à jour les données
    CALL METHOD gv_alv_header->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_create_containers
*&---------------------------------------------------------------------*
FORM f_create_containers.
  " Variables locales
  DATA: lv_container TYPE REF TO cl_gui_custom_container.

  " Je crée le container principal lié au custom control du dynpro
  CREATE OBJECT lv_container
    EXPORTING
      container_name = 'CUSTOM_CONTAINER'
    EXCEPTIONS
      OTHERS         = 1.

  IF sy-subrc <> 0.
    MESSAGE 'Erreur création custom container' TYPE 'E'.
  ENDIF.

  " Je crée un splitter pour séparer l'écran en 2 parties
  CREATE OBJECT gv_splitter
    EXPORTING
      parent            = lv_container
      rows              = 1
      columns           = 2
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.

  IF sy-subrc <> 0.
    MESSAGE 'Erreur création splitter' TYPE 'E'.
  ENDIF.

  " Je récupère le container gauche (header)
  CALL METHOD gv_splitter->get_container
    EXPORTING
      row       = 1
      column    = 1
    RECEIVING
      container = gv_container_left.

  " Je récupère le container droit (items)
  CALL METHOD gv_splitter->get_container
    EXPORTING
      row       = 1
      column    = 2
    RECEIVING
      container = gv_container_right.

  " Je définis la largeur des colonnes (50/50)
  CALL METHOD gv_splitter->set_column_width
    EXPORTING
      id    = 1
      width = 50.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_create_alv_header
*&---------------------------------------------------------------------*
FORM f_create_alv_header.
  " Variables locales
  DATA: lt_fieldcat TYPE lvc_t_fcat,
        ls_fieldcat TYPE lvc_s_fcat,
        ls_layout   TYPE lvc_s_layo.

  " Je crée l'objet ALV pour le header
  CREATE OBJECT gv_alv_header
    EXPORTING
      i_parent = gv_container_left
    EXCEPTIONS
      OTHERS   = 1.

  IF sy-subrc <> 0.
    MESSAGE 'Erreur création ALV header' TYPE 'E'.
  ENDIF.

  " Je configure le field catalog pour le header
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'EBELN'.
  ls_fieldcat-scrtext_l = 'Purchasing Document'.
  ls_fieldcat-col_pos = 1.
  ls_fieldcat-outputlen = 10.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'BSTYP'.
  ls_fieldcat-scrtext_l = 'Doc. Cat.'.
  ls_fieldcat-col_pos = 2.
  ls_fieldcat-outputlen = 3.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'AEDAT'.
  ls_fieldcat-scrtext_l = 'Changed on'.
  ls_fieldcat-col_pos = 3.
  ls_fieldcat-outputlen = 10.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ERNAM'.
  ls_fieldcat-scrtext_l = 'Created by'.
  ls_fieldcat-col_pos = 4.
  ls_fieldcat-outputlen = 12.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'WAERS'.
  ls_fieldcat-scrtext_l = 'Currency'.
  ls_fieldcat-col_pos = 5.
  ls_fieldcat-outputlen = 5.
  APPEND ls_fieldcat TO lt_fieldcat.

  " Je configure le layout
  ls_layout-grid_title = 'HEADER'.
  ls_layout-smalltitle = 'X'.
  ls_layout-sel_mode = 'A'.

  " J'affiche l'ALV header
  CALL METHOD gv_alv_header->set_table_for_first_display
    EXPORTING
      is_layout       = ls_layout
    CHANGING
      it_outtab       = gt_header
      it_fieldcatalog = lt_fieldcat
    EXCEPTIONS
      OTHERS          = 1.

  IF sy-subrc <> 0.
    MESSAGE 'Erreur affichage ALV header' TYPE 'E'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_create_alv_items
*&---------------------------------------------------------------------*
FORM f_create_alv_items.
  " Je crée l'objet ALV pour les items (vide au départ)
  CREATE OBJECT gv_alv_items
    EXPORTING
      i_parent = gv_container_right
    EXCEPTIONS
      OTHERS   = 1.

  IF sy-subrc <> 0.
    MESSAGE 'Erreur création ALV items' TYPE 'E'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_create_event_handler
*&---------------------------------------------------------------------*
FORM f_create_event_handler.
  " Je crée l'instance du gestionnaire d'événements
  " J'injecte les dépendances (tables + référence ALV items)
  CREATE OBJECT go_event_handler
    EXPORTING
      it_header    = gt_header
      it_items     = gt_items
      io_alv_items = gv_alv_items.

  " J'enregistre l'événement double-clic sur l'ALV header
  SET HANDLER go_event_handler->on_double_click FOR gv_alv_header.
ENDFORM.
