CLASS zcx_ca_enh_screen_by_dc DEFINITION
  PUBLIC
  INHERITING FROM zcx_ca_param
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF zcx_ca_enh_screen_by_dc,
        msgid TYPE symsgid VALUE 'ZCA',
        msgno TYPE symsgno VALUE '081',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ca_enh_screen_by_dc .
    CONSTANTS:
      BEGIN OF no_proper_environment,
        msgid TYPE symsgid VALUE 'ZCA',
        msgno TYPE symsgno VALUE '082',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_proper_environment .

    "! <p class="shorttext synchronized" lang="en">My own name</p>
    CONSTANTS c_zcx_ca_enh_screen_by_dc TYPE seoclsname VALUE 'ZCX_CA_ENH_SCREEN_BY_DC' ##NO_TEXT.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        !textid    LIKE if_t100_message=>t100key OPTIONAL
        !previous  LIKE previous OPTIONAL
        !mt_return TYPE bapiret2_t OPTIONAL
        !mv_subrc  TYPE syst_subrc OPTIONAL
        !mv_msgty  TYPE syst_msgty OPTIONAL
        !mv_msgv1  TYPE syst_msgv OPTIONAL
        !mv_msgv2  TYPE syst_msgv OPTIONAL
        !mv_msgv3  TYPE syst_msgv OPTIONAL
        !mv_msgv4  TYPE syst_msgv OPTIONAL .
protected section.
private section.
ENDCLASS.



CLASS ZCX_CA_ENH_SCREEN_BY_DC IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
MT_RETURN = MT_RETURN
MV_SUBRC = MV_SUBRC
MV_MSGTY = MV_MSGTY
MV_MSGV1 = MV_MSGV1
MV_MSGV2 = MV_MSGV2
MV_MSGV3 = MV_MSGV3
MV_MSGV4 = MV_MSGV4
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_CA_ENH_SCREEN_BY_DC .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
