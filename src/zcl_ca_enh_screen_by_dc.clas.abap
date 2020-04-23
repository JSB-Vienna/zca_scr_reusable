"! <p class="shorttext synchronized" lang="en">Common object: Enhance screen/dynpro by docking container</p>
CLASS zcl_ca_enh_screen_by_dc DEFINITION PUBLIC
                                         CREATE PUBLIC.

* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      zif_ca_c_bool.

*   a l i a s e s
    ALIASES:
*     Boolean flags
      c_false              FOR  zif_ca_c_bool~c_false,
      c_true               FOR  zif_ca_c_bool~c_true.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Check, if it is currently relevant to enhance the screen</p>
      "!
      "! @parameter iv_repid       | <p class="shorttext synchronized" lang="en">Program to screen</p>
      "! @parameter iv_dynnr       | <p class="shorttext synchronized" lang="en">Screen number to be enhanced</p>
      "! @parameter is_lporb       | <p class="shorttext synchronized" lang="en">BO key - for buffering purposes</p>
      "! @parameter it_params      | <p class="shorttext synchronized" lang="en">Parameters and there value references</p>
      "! @parameter rv_is_relevant | <p class="shorttext synchronized" lang="en">1 = Is relevant to display</p>
      "! @raising   zcx_ca_enh_screen_by_dc | <p class="shorttext synchronized" lang="en">Common exception: Enhancing screen/dynpro by docking contain</p>
      check_relevance
        IMPORTING
          VALUE(iv_repid)       TYPE syrepid       DEFAULT sy-cprog
          VALUE(iv_dynnr)       TYPE syst_dynnr    DEFAULT sy-dynnr
          is_lporb              TYPE sibflporb     OPTIONAL
          it_params             TYPE zca_tt_params OPTIONAL
        RETURNING
          VALUE(rv_is_relevant) TYPE dml_boolean
        RAISING
          zcx_ca_enh_screen_by_dc,

      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @raising   zcx_ca_enh_screen_by_dc | <p class="shorttext synchronized" lang="en">Common exception: Enhancing screen/dynpro by docking contain</p>
      constructor
        RAISING
          zcx_ca_enh_screen_by_dc,

      "! <p class="shorttext synchronized" lang="en">Create the docking container and display</p>
      "!
      "! @parameter iv_side     | <p class="shorttext synchronized" lang="en">Where to dock at the screen</p>
      "! @parameter iv_ratio    | <p class="shorttext synchronized" lang="en">Part of the screen / dynpro in percent</p>
      "! @parameter iv_lifetime | <p class="shorttext synchronized" lang="en">Lifetime of control (use const CL_GUI_CONTROL=>LIFETIME_*)</p>
      "! @raising   zcx_ca_enh_screen_by_dc | <p class="shorttext synchronized" lang="en">Common exception: Enhancing screen/dynpro by docking contain</p>
      create_n_display_docking
        IMPORTING
          iv_side     TYPE i DEFAULT cl_gui_docking_container=>dock_at_right
          iv_ratio    TYPE i DEFAULT 50
          iv_lifetime TYPE i DEFAULT cl_gui_control=>lifetime_default
        RAISING
          zcx_ca_enh_screen_by_dc.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   l o c a l   t y p e   d e f i n i t i o n
    TYPES:
      "! <p class="shorttext synchronized" lang="en">Workarea for buffered controls / containers</p>
      BEGIN OF ty_s_ctrl_buffer,
        s_lporb   TYPE sibflporb,
        o_ctrl    TYPE REF TO cl_gui_control,
        o_curr_dc TYPE REF TO cl_gui_docking_container,
        o_obj     TYPE REF TO object,
      END   OF ty_s_ctrl_buffer,
      "! <p class="shorttext synchronized" lang="en">Buffer for used controls / containers</p>
      ty_t_ctrl_buffer TYPE SORTED TABLE OF ty_s_ctrl_buffer
                                   WITH UNIQUE KEY s_lporb
                                   WITH UNIQUE SORTED KEY for_ctrl
                                               COMPONENTS o_ctrl,

      "! <p class="shorttext synchronized" lang="en">Workarea for buffered docking containers</p>
      BEGIN OF ty_s_dc_buffer,
        repid TYPE syrepid,
        dynnr TYPE syst_dynnr,
        o_dc  TYPE REF TO cl_gui_docking_container,
      END   OF ty_s_dc_buffer,


      "! <p class="shorttext synchronized" lang="en">Buffer for used docking containers</p>
      ty_t_dc_buffer TYPE SORTED TABLE OF ty_s_dc_buffer
                                 WITH UNIQUE KEY repid  dynnr.

*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Buffer for used controls / containers</p>
      mt_ctrl_buffer TYPE ty_t_ctrl_buffer,
      "! <p class="shorttext synchronized" lang="en">Buffer for used docking containers</p>
      mt_dc_buffer   TYPE ty_t_dc_buffer.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Workarea for buffered controls / containers</p>
      ms_ctrl_buffer TYPE ty_s_ctrl_buffer,
      "! <p class="shorttext synchronized" lang="en">Workarea for buffered docking containers</p>
      ms_dc_buffer   TYPE ty_s_dc_buffer,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">1 = Docking container is new; 0 = already buffered</p>
      mv_dc_is_new   TYPE dml_boolean,
      "! <p class="shorttext synchronized" lang="en">Screen / dynpro no. the docking container is attached</p>
      mv_dynnr       TYPE syst_dynnr,
      "! <p class="shorttext synchronized" lang="en">Lifetime of control</p>
      mv_lifetime    TYPE i,
      "! <p class="shorttext synchronized" lang="en">Ratio the screen is used in percent</p>
      mv_ratio       TYPE i,
      "! <p class="shorttext synchronized" lang="en">Program name to screen / dynpro</p>
      mv_repid       TYPE syrepid,
      "! <p class="shorttext synchronized" lang="en">Docking side</p>
      mv_side        TYPE i.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Create docking container (redefine, if constr param too less</p>
      "!
      "! @parameter iv_side      | <p class="shorttext synchronized" lang="en">Where to dock at the screen</p>
      "! @parameter iv_ratio     | <p class="shorttext synchronized" lang="en">Part of the screen / dynpro in percent</p>
      "! @parameter iv_extension | <p class="shorttext synchronized" lang="en">Lifetime of control (use const CL_GUI_CONTROL=>LIFETIME_*)</p>
      "! @parameter iv_metric    | <p class="shorttext synchronized" lang="en">Lifetime of control (use const CL_GUI_CONTROL=>LIFETIME_*)</p>
      "! @parameter iv_caption   | <p class="shorttext synchronized" lang="en">Lifetime of control (use const CL_GUI_CONTROL=>LIFETIME_*)</p>
      "! @parameter iv_cnt_name  | <p class="shorttext synchronized" lang="en">Lifetime of control (use const CL_GUI_CONTROL=>LIFETIME_*)</p>
      "! @parameter iv_style     | <p class="shorttext synchronized" lang="en">Lifetime of control (use const CL_GUI_CONTROL=>LIFETIME_*)</p>
      "! @parameter iv_lifetime  | <p class="shorttext synchronized" lang="en">Lifetime of control (use const CL_GUI_CONTROL=>LIFETIME_*)</p>
      "! @raising   zcx_ca_enh_screen_by_dc | <p class="shorttext synchronized" lang="en">Common exception: Enhancing screen/dynpro by docking contain</p>
      create_docking_container
        IMPORTING
          iv_side      TYPE i         DEFAULT cl_gui_docking_container=>dock_at_right
          iv_ratio     TYPE i         DEFAULT 50
          iv_extension TYPE i         OPTIONAL
          iv_metric    TYPE i         DEFAULT cl_gui_control=>metric_default
          iv_caption   TYPE csequence OPTIONAL
          iv_cnt_name  TYPE csequence OPTIONAL
          iv_style     TYPE i         OPTIONAL
          iv_lifetime  TYPE i         DEFAULT cl_gui_control=>lifetime_default
        RETURNING
          VALUE(ro_dc) TYPE REF TO cl_gui_docking_container
        RAISING
          zcx_ca_enh_screen_by_dc,

      "! <p class="shorttext synchronized" lang="en">Lookup for an active workitem and get object</p>
      "!
      "! @parameter ro_ctrl                 | <p class="shorttext synchronized" lang="en">Determined control / container bound to DC</p>
      "! @raising   zcx_ca_enh_screen_by_dc | <p class="shorttext synchronized" lang="en">Common exception: Enhancing screen/dynpro by docking contain</p>
      determine_ctrl_from_dc_childs
        RETURNING
          VALUE(ro_ctrl) TYPE REF TO cl_gui_control
        RAISING
          zcx_ca_enh_screen_by_dc,

      "! <p class="shorttext synchronized" lang="en">Get control from buffer (check for workitem, if requested)</p>
      "!
      "! @parameter is_lporb                | <p class="shorttext synchronized" lang="en">BO key - for buffering purposes</p>
      "! @raising   zcx_ca_enh_screen_by_dc | <p class="shorttext synchronized" lang="en">Common exception: Enhancing screen/dynpro by docking contain</p>
      get_ctrl_from_buffer
        IMPORTING
          is_lporb TYPE sibflporb
        RAISING
          zcx_ca_enh_screen_by_dc,

      "! <p class="shorttext synchronized" lang="en">Get docking from buffer (creates + insert DC if not exist)</p>
      "!
      "! @raising   zcx_ca_enh_screen_by_dc | <p class="shorttext synchronized" lang="en">Common exception: Enhancing screen/dynpro by docking contain</p>
      get_dc_from_buffer
        RAISING
          zcx_ca_enh_screen_by_dc,

      "! <p class="shorttext synchronized" lang="en">Set control into buffer</p>
      "!
      "! @raising   zcx_ca_enh_screen_by_dc | <p class="shorttext synchronized" lang="en">Common exception: Enhancing screen/dynpro by docking contain</p>
      set_ctrl_into_buffer
        RAISING
          zcx_ca_enh_screen_by_dc,

      "! <p class="shorttext synchronized" lang="en">Set all included controls / containers in- / visible</p>
      "!
      "! @parameter io_control | <p class="shorttext synchronized" lang="en">Control / Container</p>
      "! @parameter iv_visible | <p class="shorttext synchronized" lang="en">1 = Visible</p>
      set_current_ctrls_visible
        IMPORTING
          io_control TYPE REF TO cl_gui_control
          iv_visible TYPE dml_boolean DEFAULT c_true,

      "! <p class="shorttext synchronized" lang="en">Set docking container into buffer</p>
      "!
      "! @parameter rs_dc_buffer | <p class="shorttext synchronized" lang="en">Workarea for buffered docking containers</p>
      "! @raising   zcx_ca_enh_screen_by_dc | <p class="shorttext synchronized" lang="en">Common exception: Enhancing screen/dynpro by docking contain</p>
      set_dc_into_buffer
        RETURNING
          VALUE(rs_dc_buffer) TYPE ty_s_dc_buffer
        RAISING
          zcx_ca_enh_screen_by_dc.

ENDCLASS.


CLASS zcl_ca_enh_screen_by_dc IMPLEMENTATION.

  METHOD check_relevance.
    "-----------------------------------------------------------------*
    "   Check, if it is currently relevant to enhance the screen and
    "   display the docking container. Call SUPER->METHOD in any case!!
    "-----------------------------------------------------------------*
    rv_is_relevant = c_false.

    me->mv_repid = iv_repid.
    me->mv_dynnr = iv_dynnr.

    IF is_lporb IS NOT INITIAL.
      me->ms_ctrl_buffer-s_lporb = is_lporb.
    ENDIF.

    "h a s   t o   b e   r e d e f i n e d   f r o m   h  e r e
  ENDMETHOD.                    "method_name


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   description
    "-----------------------------------------------------------------*
    IF cl_gui_object=>gui_is_running EQ abap_false OR
       cl_gui_object=>activex        EQ abap_false OR
       cl_gui_object=>catt_activ     EQ abap_true  OR
       sy-batch                      EQ abap_true  OR
       sy-binpt                      EQ abap_true.
      "Environment or current process type is not proper for use of controls
      RAISE EXCEPTION TYPE zcx_ca_enh_screen_by_dc
        EXPORTING
          textid = zcx_ca_enh_screen_by_dc=>no_proper_environment.
    ENDIF.
  ENDMETHOD.                    "method_name


  METHOD create_docking_container.
    "---------------------------------------------------------------------*
    "     Creates the docking container. If the parameters of the
    "     constructor are not enough redefine this method and provide
    "     the values for your needs.
    "---------------------------------------------------------------------*
    TRY.
        ro_dc = zcl_ca_cfw_util=>create_dock_cont( iv_repid     = me->mv_repid
                                                   iv_dynnr     = me->mv_dynnr
                                                   iv_side      = me->mv_side
                                                   iv_ratio     = me->mv_ratio
                                                   iv_extension = iv_extension
                                                   iv_metric    = iv_metric
                                                   iv_caption   = iv_caption
                                                   iv_cnt_name  = iv_cnt_name
                                                   iv_style     = iv_style
                                                   iv_lifetime  = me->mv_lifetime ).

      CATCH zcx_ca_error
            zcx_ca_intern INTO DATA(lx_error).
        DATA(lx_enh_scr_dc) =
              CAST zcx_ca_enh_screen_by_dc(
                       zcx_ca_error=>create_exception(
                                      iv_excp_cls = zcx_ca_enh_screen_by_dc=>c_zcx_ca_enh_screen_by_dc
                                      iv_class    = 'ZCL_CA_CFW_UTIL'
                                      iv_method   = 'CREATE_DOCK_CONT'
                                      ix_error    = lx_error ) )  ##no_text.
        IF lx_enh_scr_dc IS BOUND.
          RAISE EXCEPTION lx_enh_scr_dc.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "method_name


  METHOD create_n_display_docking ##needed.
    "-----------------------------------------------------------------*
    "   Create the docking container and display. Call BEFORE the
    "   SUPER->METHOD in any case!!
    "-----------------------------------------------------------------*
    me->mv_side      = iv_side.
    me->mv_ratio     = iv_ratio.
    me->mv_lifetime  = iv_lifetime.
    me->get_dc_from_buffer( ).

    "h a s   t o   b e   r e d e f i n e d   f r o m   h  e r e
  ENDMETHOD.                    "method_name


  METHOD determine_ctrl_from_dc_childs.
    "-----------------------------------------------------------------*
    "   description
    "-----------------------------------------------------------------*
    LOOP AT me->ms_dc_buffer-o_dc->children INTO ro_ctrl.
      IF NOT line_exists( me->mt_ctrl_buffer[ KEY for_ctrl
                                              COMPONENTS o_ctrl = ro_ctrl ] ).
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "method_name


  METHOD get_ctrl_from_buffer.
    "-----------------------------------------------------------------*
    "   description
    "-----------------------------------------------------------------*
    "At this point the LPORB must be filled
    IF is_lporb IS INITIAL.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_enh_screen_by_dc
        EXPORTING
          textid   = zcx_ca_enh_screen_by_dc=>param_invalid
          mv_msgv1 = 'MS_CTRL_BUFFER-S_LPORB'
          mv_msgv2 = 'SPACE' ##no_text.
    ENDIF.

    CLEAR me->ms_ctrl_buffer.
    READ TABLE zcl_ca_enh_screen_by_dc=>mt_ctrl_buffer
                          INTO me->ms_ctrl_buffer
                          WITH TABLE KEY primary_key
                              COMPONENTS s_lporb = is_lporb.
    IF sy-subrc NE 0.
      me->ms_ctrl_buffer-s_lporb = is_lporb.
    ENDIF.
  ENDMETHOD.                    "method_name


  METHOD get_dc_from_buffer.
    "-----------------------------------------------------------------*
    "   description
    "-----------------------------------------------------------------*
    TRY.
        me->mv_dc_is_new = c_false.
        READ TABLE zcl_ca_enh_screen_by_dc=>mt_dc_buffer
                                            INTO me->ms_dc_buffer
                                            WITH TABLE KEY primary_key
                                                COMPONENTS repid = me->mv_repid
                                                           dynnr = me->mv_dynnr.
        IF sy-subrc NE 0.
          "Keep index for insert later
          DATA(lv_tabix) = sy-tabix.

          READ TABLE zcl_ca_enh_screen_by_dc=>mt_dc_buffer
                                              INTO me->ms_dc_buffer
                                              WITH KEY repid = me->mv_repid.
          CASE sy-subrc.
            WHEN 0.
              "found in buffer to program, but new dynpro
              me->ms_dc_buffer-dynnr = me->mv_dynnr.
              INSERT me->ms_dc_buffer INTO  zcl_ca_enh_screen_by_dc=>mt_dc_buffer
                                      INDEX lv_tabix.

            WHEN 4.
              me->mv_dc_is_new = c_true.

              me->ms_dc_buffer-repid = me->mv_repid.
              me->ms_dc_buffer-dynnr = me->mv_dynnr.
              me->ms_dc_buffer-o_dc  = me->create_docking_container( ).
              INSERT me->ms_dc_buffer INTO  zcl_ca_enh_screen_by_dc=>mt_dc_buffer
                                      INDEX lv_tabix.

            WHEN 8.
              me->mv_dc_is_new = c_true.

              me->ms_dc_buffer-repid = me->mv_repid.
              me->ms_dc_buffer-dynnr = me->mv_dynnr.
              me->ms_dc_buffer-o_dc  = me->create_docking_container( ).
              APPEND me->ms_dc_buffer TO zcl_ca_enh_screen_by_dc=>mt_dc_buffer.
          ENDCASE.
        ENDIF.

      CATCH zcx_ca_error
            zcx_ca_intern INTO DATA(lx_error).
        DATA(lx_enh_scr_dc) =
              CAST zcx_ca_enh_screen_by_dc(
                       zcx_ca_error=>create_exception(
                                      iv_excp_cls = zcx_ca_enh_screen_by_dc=>c_zcx_ca_enh_screen_by_dc
                                      iv_class    = 'ZCL_CA_ENH_SCREEN_BY_DC'
                                      iv_method   = 'GET_DC_FROM_BUFFER'
                                      ix_error    = lx_error ) )  ##no_text.
        IF lx_enh_scr_dc IS BOUND.
          RAISE EXCEPTION lx_enh_scr_dc.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "method_name


  METHOD set_ctrl_into_buffer.
    "-----------------------------------------------------------------*
    "   description
    "-----------------------------------------------------------------*
    TRY.
        "At this point the LPORB must be filled
        IF me->ms_ctrl_buffer-s_lporb IS INITIAL.
          "Parameter '&1' has invalid value '&2'
          RAISE EXCEPTION TYPE zcx_ca_enh_screen_by_dc
            EXPORTING
              textid   = zcx_ca_enh_screen_by_dc=>param_invalid
              mv_msgv1 = 'MS_CTRL_BUFFER-S_LPORB'
              mv_msgv2 = 'SPACE' ##no_text.
        ENDIF.

        READ TABLE zcl_ca_enh_screen_by_dc=>mt_ctrl_buffer
                              TRANSPORTING NO FIELDS
                              WITH TABLE KEY primary_key
                                  COMPONENTS s_lporb = me->ms_ctrl_buffer-s_lporb.
        CASE sy-subrc.
          WHEN 0.
            RETURN.

          WHEN 4.
            me->ms_ctrl_buffer-o_ctrl    = me->determine_ctrl_from_dc_childs( ).
            me->ms_ctrl_buffer-o_curr_dc = me->ms_dc_buffer-o_dc.
            INSERT me->ms_ctrl_buffer INTO  zcl_ca_enh_screen_by_dc=>mt_ctrl_buffer
                                      INDEX sy-tabix.

          WHEN 8.
            me->ms_ctrl_buffer-o_ctrl    = me->determine_ctrl_from_dc_childs( ).
            me->ms_ctrl_buffer-o_curr_dc = me->ms_dc_buffer-o_dc.
            APPEND me->ms_ctrl_buffer TO zcl_ca_enh_screen_by_dc=>mt_ctrl_buffer.
        ENDCASE.

      CATCH zcx_ca_error
            zcx_ca_intern INTO DATA(lx_error).
        DATA(lx_enh_scr_dc) =
              CAST zcx_ca_enh_screen_by_dc(
                       zcx_ca_error=>create_exception(
                                      iv_excp_cls = zcx_ca_enh_screen_by_dc=>c_zcx_ca_enh_screen_by_dc
                                      iv_class    = 'ZCL_CA_ENH_SCREEN_BY_DC'
                                      iv_method   = 'SET_CTRL_INTO_BUFFER'
                                      ix_error    = lx_error ) )  ##no_text.
        IF lx_enh_scr_dc IS BOUND.
          RAISE EXCEPTION lx_enh_scr_dc.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "method_name


  METHOD set_current_ctrls_visible.
    "-----------------------------------------------------------------*
    "   description
    "-----------------------------------------------------------------*
    TRY.
        DATA(lo_container) = CAST cl_gui_container( io_control ).

        LOOP AT lo_container->children INTO DATA(lo_child).
          me->set_current_ctrls_visible( io_control = lo_child
                                         iv_visible = iv_visible ).
        ENDLOOP.

      CATCH cx_sy_move_cast_error.
        "Only containers have children
    ENDTRY.

    zcl_ca_cfw_util=>set_visible( io_control = io_control
                                  iv_visible = iv_visible ).
  ENDMETHOD.                    "method_name


  METHOD set_dc_into_buffer.
    "-----------------------------------------------------------------*
    "   description
    "-----------------------------------------------------------------*
    TRY.
        "Erst nur mit Programm suchen
        READ TABLE zcl_ca_enh_screen_by_dc=>mt_dc_buffer
                                            INTO rs_dc_buffer
                                            WITH KEY repid = me->mv_repid.
        IF sy-subrc NE 0.
          READ TABLE zcl_ca_enh_screen_by_dc=>mt_dc_buffer
                                              INTO rs_dc_buffer
                                              WITH TABLE KEY primary_key
                                                  COMPONENTS repid = me->mv_repid
                                                             dynnr = me->mv_dynnr.
          CASE sy-subrc.
            WHEN 0.
              "found in buffer

            WHEN 4.
              rs_dc_buffer-repid = me->mv_repid.
              rs_dc_buffer-dynnr = me->mv_dynnr.
              rs_dc_buffer-o_dc  = me->create_docking_container( ).
              INSERT rs_dc_buffer INTO  zcl_ca_enh_screen_by_dc=>mt_dc_buffer
                                  INDEX sy-tabix.

            WHEN 8.
              rs_dc_buffer-repid = me->mv_repid.
              rs_dc_buffer-dynnr = me->mv_dynnr.
              rs_dc_buffer-o_dc  = me->create_docking_container( ).
              APPEND rs_dc_buffer TO zcl_ca_enh_screen_by_dc=>mt_dc_buffer.
          ENDCASE.
        ENDIF.

      CATCH zcx_ca_error
            zcx_ca_intern INTO DATA(lx_error).
        DATA(lx_enh_scr_dc) =
              CAST zcx_ca_enh_screen_by_dc(
                       zcx_ca_error=>create_exception(
                                      iv_excp_cls = zcx_ca_enh_screen_by_dc=>c_zcx_ca_enh_screen_by_dc
                                      iv_class    = 'ZCL_CA_ENH_SCREEN_BY_DC'
                                      iv_method   = 'GET_DC_FROM_BUFFER'
                                      ix_error    = lx_error ) )  ##no_text.
        IF lx_enh_scr_dc IS BOUND.
          RAISE EXCEPTION lx_enh_scr_dc.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "method_name

ENDCLASS.
