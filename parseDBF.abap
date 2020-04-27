FORM upload_dbf_file
  TABLES   pt_file
  USING    p_fpath.

  DATA l_fpath  TYPE string.

  TYPES: BEGIN OF ty_data
    , row(1024) TYPE x
    , END OF ty_data.

  DATA lt_data  TYPE STANDARD TABLE OF ty_data.
  DATA lwa_data  LIKE LINE OF lt_data.

  l_fpath = p_fpath.


  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = l_fpath
      filetype                = 'BIN'
    CHANGING
      data_tab                = lt_data
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
      not_supported_by_gui    = 17
      error_no_gui            = 18.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Parse Dbf File Into Internal Table
  PERFORM read_dbf
    TABLES lt_data
           pt_file.



ENDFORM.                    "upload_dbf_file
*&---------------------------------------------------------------------*
*&      Form  read_dbf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_dbf
  TABLES pt_xdata
         pt_dbf.

* Dbf Header
  DATA: BEGIN OF ls_dbf_head
    , version           TYPE int1
    , date              LIKE sy-datum
    , numRecords              TYPE int4
    , lenHeader      TYPE int2
    , lenRecord           TYPE int2
    , END OF ls_dbf_head.

* Fields Catalog
  DATA: BEGIN OF lt_dbf_fields  OCCURS 0
    , name              TYPE char10
    , type              TYPE c
    , pos               TYPE int4
    , len               TYPE int1
    , digits            TYPE int1
    , END OF lt_dbf_fields.

  DATA l_xdbf          TYPE xstring.
  DATA l_xstring       TYPE xstring.
  DATA l_xfield        TYPE xstring.
  DATA l_xrow          TYPE xstring.
  DATA l_fields_len    TYPE i.
  DATA l_offset        TYPE i.

  FIELD-SYMBOLS <fs_x>       TYPE ANY.
  FIELD-SYMBOLS <fs_any>     TYPE ANY.
  FIELD-SYMBOLS <fs_value>   TYPE ANY.


* All Table into X string
  CLEAR l_xdbf.

  LOOP AT pt_xdata ASSIGNING <fs_any>.
    ASSIGN COMPONENT 1 OF STRUCTURE <fs_any> TO <fs_x>.
    CHECK sy-subrc EQ 0.

    l_xstring = <fs_x>.
    CONCATENATE l_xdbf l_xstring INTO l_xdbf IN BYTE MODE.
  ENDLOOP.

* Fill Dbf Header
  CLEAR ls_dbf_head.


* Count of Rows
  l_xstring = l_xdbf+4(4).
  PERFORM convert_from_x USING l_xstring CHANGING  ls_dbf_head-numRecords.

* Start Offset
  l_xstring = l_xdbf+8(2).
  PERFORM convert_from_x USING l_xstring CHANGING  ls_dbf_head-lenHeader.

* Row Len
  l_xstring = l_xdbf+10(2).
  PERFORM convert_from_x USING l_xstring CHANGING  ls_dbf_head-lenRecord.


* Fill Dbf Fields
  REFRESH lt_dbf_fields[].

  l_offset = 32.

  WHILE l_offset < ls_dbf_head-lenHeader.

    l_xfield = l_xdbf+l_offset(32).
    CLEAR lt_dbf_fields.

*   Field Name
    l_xstring = l_xfield(10).
    PERFORM convert_from_x USING l_xstring CHANGING  lt_dbf_fields-name.

*   Field Type
    l_xstring = l_xfield+11(1).
    PERFORM convert_from_x USING l_xstring CHANGING  lt_dbf_fields-type.

*   Field Position
    l_xstring = l_xfield+12(4).
    PERFORM convert_from_x USING l_xstring CHANGING  lt_dbf_fields-pos.

*   Field Length
    l_xstring = l_xfield+16(1).
    PERFORM convert_from_x USING l_xstring CHANGING  lt_dbf_fields-len.

*   Digits
    l_xstring = l_xfield+17(1).
    PERFORM convert_from_x USING l_xstring CHANGING  lt_dbf_fields-digits.

    APPEND lt_dbf_fields.
    l_offset = l_offset + 32.

  ENDWHILE.

* Fill Data
  REFRESH pt_dbf[].

  l_offset = ls_dbf_head-lenHeader + 1.
  DO ls_dbf_head-numRecords TIMES.
    CLEAR pt_dbf.

*   For All Fields
    LOOP AT lt_dbf_fields.

      ASSIGN COMPONENT lt_dbf_fields-name OF STRUCTURE pt_dbf TO <fs_value>.
      CHECK sy-subrc EQ 0.

      l_xstring = l_xdbf+l_offset(lt_dbf_fields-len).

*     Read Value
      PERFORM convert_from_x USING l_xstring CHANGING  <fs_value>.

      ADD lt_dbf_fields-len TO l_offset.
    ENDLOOP.

    l_offset = l_offset + 1.

    APPEND pt_dbf.
  ENDDO.

ENDFORM.                    " read_dbf
*&---------------------------------------------------------------------*
*&      Form  convert_from_x
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_XDBF+4(4)  text
*      <--P_LS_DBF_HEAD_ROWS  text
*----------------------------------------------------------------------*
FORM convert_from_x  USING    p_xstring TYPE xstring
                     CHANGING p_value.

  DATA l_type        TYPE c.
  DATA l_x00         TYPE x VALUE '00'.
  DATA l_x20         TYPE x VALUE '20'.

* Convert From XString
  DATA lc_conv     TYPE REF TO cl_abap_conv_in_ce.
  DATA l_len       TYPE i.
  DATA l_xstring   TYPE xstring.

  l_xstring = p_xstring.

  DESCRIBE FIELD p_value TYPE l_type.

  IF l_type CA 'Cg'.
    REPLACE ALL OCCURRENCES OF l_x00 IN l_xstring WITH l_x20  IN BYTE MODE.
  ENDIF.


  CALL METHOD cl_abap_conv_in_ce=>create
    EXPORTING
      input       = l_xstring
      encoding    = '1504'
      replacement = space
      ignore_cerr = abap_true
    RECEIVING
      conv        = lc_conv.

  TRY.
      CALL METHOD lc_conv->read
        IMPORTING
          data = p_value
          len  = l_len.

*   Should ignore errors in code conversions
    CATCH cx_sy_conversion_codepage.
    CATCH cx_sy_codepage_converter_init.
    CATCH cx_parameter_invalid_type.
    CATCH cx_parameter_invalid_range.
  ENDTRY.

ENDFORM.                    " convert_from_x 