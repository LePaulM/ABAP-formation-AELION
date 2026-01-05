*&---------------------------------------------------------------------*
*& Classe ZCL_EVENT_HANDLER_PMI
*&---------------------------------------------------------------------*
*& Gestionnaire d'événements ALV pour navigation par double-clic
*& Utilise l'injection de dépendances pour recevoir les données
*&---------------------------------------------------------------------*

CLASS zcl_event_handler_pmi DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    " Types publics pour injection de dépendances
    TYPES: BEGIN OF ty_header,
             ebeln TYPE ebeln,
             bstyp TYPE bstyp,
             aedat TYPE aedat,
             ernam TYPE ernam,
             waers TYPE waers,
           END OF ty_header.

    TYPES: ty_header_tab TYPE STANDARD TABLE OF ty_header
           WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_item,
             ebeln TYPE ebeln,
             ebelp TYPE ebelp,
             matnr TYPE matnr,
             werks TYPE werks_d,
             menge TYPE bstmg,
             netpr TYPE bprei,
             netwr TYPE bwert,
             meins TYPE bstme,
           END OF ty_item.

    TYPES: ty_items_tab TYPE STANDARD TABLE OF ty_item
           WITH DEFAULT KEY.

    " Attributs publics
    DATA: mt_header    TYPE ty_header_tab,
          mt_items     TYPE ty_items_tab,
          mo_alv_items TYPE REF TO cl_gui_alv_grid.

    " Méthodes publiques
    METHODS: constructor
      IMPORTING
        it_header    TYPE ty_header_tab
        it_items     TYPE ty_items_tab
        io_alv_items TYPE REF TO cl_gui_alv_grid.

    METHODS: on_double_click
      FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING
        e_row
        e_column
        es_row_no.

  PRIVATE SECTION.
    " Méthodes privées
    METHODS: build_items_fieldcat
      CHANGING
        ct_fieldcat TYPE lvc_t_fcat.

ENDCLASS.

CLASS zcl_event_handler_pmi IMPLEMENTATION.

  METHOD constructor.
    " J'initialise les attributs avec les données injectées
    me->mt_header = it_header.
    me->mt_items = it_items.
    me->mo_alv_items = io_alv_items.
  ENDMETHOD.

  METHOD on_double_click.
    " Variables locales
    DATA: ls_header         TYPE ty_header,
          ls_item           TYPE ty_item,
          lt_items_filtered TYPE STANDARD TABLE OF ty_item,
          lt_fieldcat       TYPE lvc_t_fcat,
          ls_layout         TYPE lvc_s_layo,
          lv_index          TYPE i.

    " Je récupère l'index de la ligne sélectionnée
    lv_index = es_row_no-row_id.

    IF lv_index = 0.
      MESSAGE 'Index invalide' TYPE 'E'.
      RETURN.
    ENDIF.

    " Je récupère la ligne correspondante
    READ TABLE me->mt_header INTO ls_header INDEX lv_index.
    IF sy-subrc <> 0.
      MESSAGE 'Erreur lecture ligne' TYPE 'E'.
      RETURN.
    ENDIF.

    " Je filtre les items pour cette commande
    LOOP AT me->mt_items INTO ls_item WHERE ebeln = ls_header-ebeln.
      APPEND ls_item TO lt_items_filtered.
    ENDLOOP.

    " Je vérifie qu'on a trouvé des items
    IF lt_items_filtered IS INITIAL.
      MESSAGE 'Aucun poste trouvé pour cette commande' TYPE 'I'.
      RETURN.
    ENDIF.

    " Je vérifie si l'ALV items est déjà créé
    IF me->mo_alv_items IS NOT BOUND.
      MESSAGE 'ALV Items non initialisé' TYPE 'E'.
      RETURN.
    ENDIF.

    " Je construis le field catalog
    me->build_items_fieldcat( CHANGING ct_fieldcat = lt_fieldcat ).

    " Je configure le layout
    ls_layout-grid_title = 'ITEM'.
    ls_layout-smalltitle = 'X'.

    " J'affiche les données dans l'ALV items
    CALL METHOD me->mo_alv_items->set_table_for_first_display
      EXPORTING
        is_layout       = ls_layout
      CHANGING
        it_outtab       = lt_items_filtered
        it_fieldcatalog = lt_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc <> 0.
      MESSAGE 'Erreur affichage ALV items' TYPE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD build_items_fieldcat.
    " Variables locales
    DATA: ls_fieldcat TYPE lvc_s_fcat.

    " Je configure chaque colonne de l'ALV items
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'EBELN'.
    ls_fieldcat-scrtext_l = 'Purchasing Document'.
    ls_fieldcat-col_pos = 1.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO ct_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'EBELP'.
    ls_fieldcat-scrtext_l = 'Item'.
    ls_fieldcat-col_pos = 2.
    ls_fieldcat-outputlen = 5.
    APPEND ls_fieldcat TO ct_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'MATNR'.
    ls_fieldcat-scrtext_l = 'Material'.
    ls_fieldcat-col_pos = 3.
    ls_fieldcat-outputlen = 18.
    APPEND ls_fieldcat TO ct_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'WERKS'.
    ls_fieldcat-scrtext_l = 'Plant'.
    ls_fieldcat-col_pos = 4.
    ls_fieldcat-outputlen = 4.
    APPEND ls_fieldcat TO ct_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'MENGE'.
    ls_fieldcat-scrtext_l = 'Quantity'.
    ls_fieldcat-col_pos = 5.
    ls_fieldcat-outputlen = 13.
    ls_fieldcat-ref_table = 'ZEKPO_PMI'.
    ls_fieldcat-ref_field = 'MENGE'.
    APPEND ls_fieldcat TO ct_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'NETPR'.
    ls_fieldcat-scrtext_l = 'Net Price'.
    ls_fieldcat-col_pos = 6.
    ls_fieldcat-outputlen = 11.
    ls_fieldcat-ref_table = 'ZEKPO_PMI'.
    ls_fieldcat-ref_field = 'NETPR'.
    APPEND ls_fieldcat TO ct_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'NETWR'.
    ls_fieldcat-scrtext_l = 'Net Value'.
    ls_fieldcat-col_pos = 7.
    ls_fieldcat-outputlen = 13.
    ls_fieldcat-ref_table = 'ZEKPO_PMI'.
    ls_fieldcat-ref_field = 'NETWR'.
    APPEND ls_fieldcat TO ct_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'MEINS'.
    ls_fieldcat-scrtext_l = 'Order Unit'.
    ls_fieldcat-col_pos = 8.
    ls_fieldcat-outputlen = 3.
    APPEND ls_fieldcat TO ct_fieldcat.
  ENDMETHOD.

ENDCLASS.
