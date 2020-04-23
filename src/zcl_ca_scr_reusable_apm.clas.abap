"! <p class="shorttext synchronized" lang="en">DO NOT USE! Tech. purpose! APACK manifest for package</p>
CLASS zcl_ca_scr_reusable_apm DEFINITION PUBLIC
                                  FINAL
                                  CREATE PUBLIC.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      zif_apack_manifest.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      constructor.
ENDCLASS.



CLASS zcl_ca_scr_reusable_apm IMPLEMENTATION.

  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    zif_apack_manifest~descriptor =
         VALUE #(
            group_id        = 'itinere.at'
            artifact_id     = 'zca_scr_reusable'
            version         = '0.1.0'
            repository_type = 'abapGit'
            git_url         = 'https://github.com/JSB-Vienna/zca_scr_reusable.git'
            dependencies    =
                  VALUE #(
                        group_id       = 'itinere.at'
                      ( artifact_id    = 'zca'
                        git_url        = 'https://github.com/JSB-Vienna/zca.git'
                        target_package = 'zca' )
                      ( artifact_id    = 'zca'
                        git_url        = 'https://github.com/JSB-Vienna/zca_scr_fw.git'
                        target_package = 'zca' ) ) ) ##no_text.
  ENDMETHOD.                    "constructor

ENDCLASS.
