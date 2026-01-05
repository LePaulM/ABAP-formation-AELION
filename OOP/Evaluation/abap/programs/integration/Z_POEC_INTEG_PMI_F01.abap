*&---------------------------------------------------------------------*
*& Include Z_POEC_INTEG_PMI_F01
*&---------------------------------------------------------------------*
*& Formes de traitement
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form f_browse_file
*&---------------------------------------------------------------------*
FORM f_browse_file.
  " Variables locales
  DATA: lv_file   TYPE string,
        lt_files  TYPE filetable,
        lv_rc     TYPE i,
        lv_action TYPE i.

  " J'ouvre la boîte de dialogue pour choisir un fichier
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Sélectionner le fichier'
      file_filter             = 'Fichiers texte (*.txt)|*.txt|Tous (*.*)|*.*'
    CHANGING
      file_table              = lt_files
      rc                      = lv_rc
      user_action             = lv_action
    EXCEPTIONS
      file_open_dialog_failed = 1
      OTHERS                  = 2.

  IF sy-subrc = 0 AND lv_action = cl_gui_frontend_services=>action_ok.
    READ TABLE lt_files INTO lv_file INDEX 1.
    IF sy-subrc = 0.
      p_file = lv_file.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_main_process
*&---------------------------------------------------------------------*
FORM f_main_process.
  " J'enchaîne les étapes du traitement
  PERFORM f_check_file_exists.
  PERFORM f_load_file.
  PERFORM f_process_data.
  PERFORM f_save_data.
  PERFORM f_display_log.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_check_file_exists
*&---------------------------------------------------------------------*
FORM f_check_file_exists.
  " Variables locales
  DATA: lv_result TYPE abap_bool,
        lv_file   TYPE string.

  " Je convertis le chemin en string
  lv_file = p_file.

  " Je vérifie l'existence du fichier
  CALL METHOD cl_gui_frontend_services=>file_exist
    EXPORTING
      file                 = lv_file
    RECEIVING
      result               = lv_result
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      wrong_parameter      = 3
      not_supported_by_gui = 4
      OTHERS               = 5.

  IF sy-subrc <> 0 OR lv_result = abap_false.
    MESSAGE 'Fichier introuvable' TYPE 'E'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_load_file
*&---------------------------------------------------------------------*
FORM f_load_file.
  DATA: lv_file TYPE string.

  lv_file = p_file.

  " Je lis le fichier ligne par ligne
  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = lv_file
      filetype                = 'ASC'
      has_field_separator     = space
    CHANGING
      data_tab                = gt_file_lines
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.

  IF sy-subrc <> 0.
    MESSAGE 'Erreur lors de la lecture du fichier' TYPE 'E'.
  ENDIF.

  IF gt_file_lines IS INITIAL.
    MESSAGE 'Fichier vide' TYPE 'E'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_process_data
*&---------------------------------------------------------------------*
FORM f_process_data.
  " Variables locales - TOUTES déclarées au début
  DATA: ls_file_line TYPE ty_file_line,
        ls_data      TYPE ty_data,
        lt_fields    TYPE TABLE OF string,
        lv_field     TYPE string,
        lv_menge     TYPE string,
        lv_netpr     TYPE string,
        lv_netwr     TYPE string.

  " Je parcours chaque ligne du fichier
  LOOP AT gt_file_lines INTO ls_file_line.
    CLEAR: ls_data, lt_fields.

    " Je découpe la ligne en champs (séparateur tabulation)
    SPLIT ls_file_line-line AT cl_abap_char_utilities=>horizontal_tab
      INTO TABLE lt_fields.

    " Colonne 1 : EBELN
    READ TABLE lt_fields INTO lv_field INDEX 1.
    IF sy-subrc = 0.
      ls_data-ebeln = lv_field.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_data-ebeln
        IMPORTING
          output = ls_data-ebeln.
    ENDIF.

    " Colonne 2 : BSTYP
    READ TABLE lt_fields INTO lv_field INDEX 2.
    IF sy-subrc = 0.
      ls_data-bstyp = lv_field.
    ENDIF.

    " Colonne 3 : AEDAT
    READ TABLE lt_fields INTO lv_field INDEX 3.
    IF sy-subrc = 0.
      CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
        EXPORTING
          date_external = lv_field
        IMPORTING
          date_internal = ls_data-aedat
        EXCEPTIONS
          OTHERS        = 1.
    ENDIF.

    " Colonne 4 : ERNAM
    READ TABLE lt_fields INTO lv_field INDEX 4.
    IF sy-subrc = 0.
      ls_data-ernam = lv_field.
    ENDIF.

    " Colonne 5 : WAERS
    READ TABLE lt_fields INTO lv_field INDEX 5.
    IF sy-subrc = 0.
      ls_data-waers = lv_field.
    ENDIF.

    " Colonne 6 : EBELP
    READ TABLE lt_fields INTO lv_field INDEX 6.
    IF sy-subrc = 0.
      ls_data-ebelp = lv_field.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_data-ebelp
        IMPORTING
          output = ls_data-ebelp.
    ENDIF.

    " Colonne 7 : MATNR
    READ TABLE lt_fields INTO lv_field INDEX 7.
    IF sy-subrc = 0.
      ls_data-matnr = lv_field.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input  = ls_data-matnr
        IMPORTING
          output = ls_data-matnr.
    ENDIF.

    " Colonne 8 : WERKS
    READ TABLE lt_fields INTO lv_field INDEX 8.
    IF sy-subrc = 0.
      ls_data-werks = lv_field.
    ENDIF.

    " Colonne 9 : MENGE
    READ TABLE lt_fields INTO lv_field INDEX 9.
    IF sy-subrc = 0.
      lv_menge = lv_field.
      " Je nettoie le format : supprime points de milliers, remplace virgule par point
      REPLACE ALL OCCURRENCES OF '.' IN lv_menge WITH ''.
      REPLACE ALL OCCURRENCES OF ',' IN lv_menge WITH '.'.
      ls_data-menge = lv_menge.
    ENDIF.

    " Colonne 10 : NETPR
    READ TABLE lt_fields INTO lv_field INDEX 10.
    IF sy-subrc = 0.
      lv_netpr = lv_field.
      REPLACE ALL OCCURRENCES OF '.' IN lv_netpr WITH ''.
      REPLACE ALL OCCURRENCES OF ',' IN lv_netpr WITH '.'.
      ls_data-netpr = lv_netpr.
    ENDIF.

    " Colonne 11 : NETWR
    READ TABLE lt_fields INTO lv_field INDEX 11.
    IF sy-subrc = 0.
      lv_netwr = lv_field.
      REPLACE ALL OCCURRENCES OF '.' IN lv_netwr WITH ''.
      REPLACE ALL OCCURRENCES OF ',' IN lv_netwr WITH '.'.
      ls_data-netwr = lv_netwr.
    ENDIF.

    " Colonne 12 : MEINS
    READ TABLE lt_fields INTO lv_field INDEX 12.
    IF sy-subrc = 0.
      ls_data-meins = lv_field.
    ENDIF.

    " J'ajoute la ligne traitée à la table de travail
    APPEND ls_data TO gt_data.
  ENDLOOP.

  " Je construis les tables d'entête et de postes
  PERFORM f_build_tables.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_build_tables
*&---------------------------------------------------------------------*
FORM f_build_tables.
  " Variables locales - TOUTES au début
  DATA: ls_data   TYPE ty_data,
        ls_header TYPE zekko_pmi,
        ls_item   TYPE zekpo_pmi,
        ls_log    TYPE ty_log,
        lv_has_duplicate TYPE abap_bool,
        lv_current_ebeln TYPE ebeln,
        lv_prev_ebelp TYPE ebelp.

  " Je parcours les données extraites du fichier
  LOOP AT gt_data INTO ls_data.
    " Je vérifie si l'entête existe déjà dans ma table de travail
    READ TABLE gt_header TRANSPORTING NO FIELDS
      WITH KEY ebeln = ls_data-ebeln.

    IF sy-subrc <> 0.
      CLEAR ls_header.
      ls_header-mandt = sy-mandt.
      ls_header-ebeln = ls_data-ebeln.
      ls_header-bstyp = ls_data-bstyp.
      ls_header-aedat = ls_data-aedat.
      ls_header-ernam = ls_data-ernam.
      ls_header-waers = ls_data-waers.
      APPEND ls_header TO gt_header.
    ENDIF.

    " Je crée le poste
    CLEAR ls_item.
    ls_item-mandt = sy-mandt.
    ls_item-ebeln = ls_data-ebeln.
    ls_item-ebelp = ls_data-ebelp.
    ls_item-matnr = ls_data-matnr.
    ls_item-werks = ls_data-werks.
    ls_item-menge = ls_data-menge.
    ls_item-netpr = ls_data-netpr.
    ls_item-netwr = ls_data-netwr.
    ls_item-meins = ls_data-meins.
    APPEND ls_item TO gt_items.
  ENDLOOP.

  " Je trie pour détecter les doublons
  SORT gt_items BY ebeln ebelp.

  " Je détecte les commandes avec postes en doublon
  CLEAR: lv_current_ebeln, lv_has_duplicate, lv_prev_ebelp.

  LOOP AT gt_items INTO ls_item.
    " Changement de commande
    IF ls_item-ebeln <> lv_current_ebeln.
      " Je traite la commande précédente si elle avait des doublons
      IF lv_has_duplicate = abap_true AND lv_current_ebeln IS NOT INITIAL.
        " Je marque cette commande en erreur
        CLEAR ls_log.
        ls_log-ebeln = lv_current_ebeln.
        ls_log-status = 'not created'.
        ls_log-color = 'C600'.
        APPEND ls_log TO gt_log.

        " Je supprime tous les postes et l'entête de cette commande
        DELETE gt_items WHERE ebeln = lv_current_ebeln.
        DELETE gt_header WHERE ebeln = lv_current_ebeln.
      ENDIF.

      " Nouvelle commande
      lv_current_ebeln = ls_item-ebeln.
      lv_has_duplicate = abap_false.
      CLEAR lv_prev_ebelp.
    ENDIF.

    " Je détecte un doublon si le même EBELP apparaît 2 fois
    IF ls_item-ebelp = lv_prev_ebelp.
      lv_has_duplicate = abap_true.
    ENDIF.

    lv_prev_ebelp = ls_item-ebelp.
  ENDLOOP.

  " Je traite la dernière commande si elle avait des doublons
  IF lv_has_duplicate = abap_true AND lv_current_ebeln IS NOT INITIAL.
    CLEAR ls_log.
    ls_log-ebeln = lv_current_ebeln.
    ls_log-status = 'not created'.
    ls_log-color = 'C600'.
    APPEND ls_log TO gt_log.

    DELETE gt_items WHERE ebeln = lv_current_ebeln.
    DELETE gt_header WHERE ebeln = lv_current_ebeln.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_save_data
*&---------------------------------------------------------------------*
FORM f_save_data.
  " Variables locales
  DATA: ls_header TYPE zekko_pmi,
        ls_item   TYPE zekpo_pmi,
        ls_log    TYPE ty_log,
        lt_existing_headers TYPE TABLE OF zekko_pmi.

  " Je vérifie quelles commandes existent déjà en base
  IF gt_header IS NOT INITIAL.
    SELECT * FROM zekko_pmi
      INTO TABLE lt_existing_headers
      FOR ALL ENTRIES IN gt_header
      WHERE ebeln = gt_header-ebeln.
  ENDIF.

  " Je traite chaque entête
  LOOP AT gt_header INTO ls_header.
    CLEAR ls_log.
    ls_log-ebeln = ls_header-ebeln.

    " Je vérifie si la commande existe déjà
    READ TABLE lt_existing_headers TRANSPORTING NO FIELDS
      WITH KEY ebeln = ls_header-ebeln.

    IF sy-subrc = 0.
      ls_log-status = 'not created'.
      ls_log-color = 'C600'.
      APPEND ls_log TO gt_log.
      DELETE gt_items WHERE ebeln = ls_header-ebeln.
      CONTINUE.
    ENDIF.

    " Si on n'est pas en mode test, j'insère en base
    IF p_test IS INITIAL.
      INSERT zekko_pmi FROM ls_header.

      IF sy-subrc = 0.
        LOOP AT gt_items INTO ls_item WHERE ebeln = ls_header-ebeln.
          INSERT zekpo_pmi FROM ls_item.
        ENDLOOP.

        COMMIT WORK.
        ls_log-status = 'created'.
        ls_log-color = 'C500'.
      ELSE.
        ls_log-status = 'not created'.
        ls_log-color = 'C600'.
      ENDIF.
    ELSE.
      " Mode test : je simule la création
      ls_log-status = 'created'.
      ls_log-color = 'C500'.
    ENDIF.

    APPEND ls_log TO gt_log.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_display_log
*&---------------------------------------------------------------------*
FORM f_display_log.
  " Variables locales pour l'affichage ALV
  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,
        ls_fieldcat TYPE slis_fieldcat_alv,
        ls_layout   TYPE slis_layout_alv.

  " Je configure les colonnes
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'EBELN'.
  ls_fieldcat-seltext_l = 'Purchasing Document'.
  ls_fieldcat-col_pos = 1.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'STATUS'.
  ls_fieldcat-seltext_l = 'Status'.
  ls_fieldcat-col_pos = 2.
  APPEND ls_fieldcat TO lt_fieldcat.

  " Je configure le layout pour les couleurs
  ls_layout-info_fieldname = 'COLOR'.

  " J'affiche l'ALV
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = ls_layout
      it_fieldcat        = lt_fieldcat
    TABLES
      t_outtab           = gt_log
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
ENDFORM.
