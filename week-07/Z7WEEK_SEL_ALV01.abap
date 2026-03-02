*&---------------------------------------------------------------------*
*& Report Z7WEEK_SEL_ALV01
*& SCARR 테이블의 항공사 정보를
*& 다양한 선택옵션과 최대 조회 건수(NUM) 파라미터를 이용하여
*& ALV Grid 형식으로 조회하는 예제 프로그램
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z7WEEK_SEL_ALV01.


TABLES:     scarr.

TYPE-POOLS: slis.                                 "ALV Declarations

*Data Declaration
*----------------
TYPES: BEGIN OF t_scarr,
  mandt    TYPE scarr-mandt,
  carrid   TYPE scarr-carrid,
  carrname TYPE scarr-carrname,
  currcode TYPE scarr-currcode,
  url      TYPE scarr-url,
 END OF t_scarr.

DATA: it_scarr TYPE STANDARD TABLE OF t_scarr,
      wa_scarr TYPE t_scarr.

*ALV data declarations
DATA: fieldcatalog TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      gd_tab_group TYPE slis_t_sp_group_alv,
      gd_layout    TYPE slis_layout_alv,
      gd_repid     LIKE sy-repid.


DATA : t TYPE slis_t_sp_group_alv .

* 검색화면
SELECTION-SCREEN BEGIN OF BLOCK part1 WITH FRAME TITLE text-001.
SELECT-OPTIONS s_carrid   FOR scarr-carrid.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS s_cname    FOR scarr-carrname.
SELECT-OPTIONS s_ccode    FOR scarr-currcode.
SELECT-OPTIONS s_url      FOR scarr-url.
SELECTION-SCREEN SKIP.
PARAMETERS NUM TYPE I. " 파라미터로 숫자 받아오도록

*PARAMETERS p_carrid TYPE scarr-carrid OBLIGATORY DEFAULT 'AA'.
SELECTION-SCREEN END OF BLOCK part1.


************************************************************************
*Start-of-selection.
START-OF-SELECTION.

  PERFORM data_retrieval.
  PERFORM build_fieldcatalog.
  PERFORM build_layout.
  PERFORM display_alv_report.


*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       Build Fieldcatalog for ALV Report
*----------------------------------------------------------------------*
FORM build_fieldcatalog.

  fieldcatalog-fieldname   = 'MANDT'.
  fieldcatalog-seltext_m   = '클라이언트'.
  fieldcatalog-col_pos     = 0.
  fieldcatalog-outputlen   = 10.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'CARRID'.
  fieldcatalog-seltext_m   = '항공사 코드'.
  fieldcatalog-col_pos     = 1.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'CARRNAME'.
  fieldcatalog-seltext_m   = '항공사 이름'.
  fieldcatalog-col_pos     = 2.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'CURRCODE'.
  fieldcatalog-seltext_m   = '통화'.
  fieldcatalog-col_pos     = 3.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'URL'.
  fieldcatalog-seltext_m   = '사이트주소'.
  fieldcatalog-col_pos     = 4.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

ENDFORM.                    " BUILD_FIELDCATALOG


*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       Build layout for ALV grid report
*----------------------------------------------------------------------*
FORM build_layout.

  gd_layout-no_input          = 'X'.
  gd_layout-colwidth_optimize = 'X'.
  gd_layout-zebra = 'X'.
*  gd_layout-info_fieldname =      'LINE_COLOR'.
*  gd_layout-def_status = 'A'.

ENDFORM.                    " BUILD_LAYOUT


*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_REPORT
*&---------------------------------------------------------------------*
*       Display report using ALV grid
*----------------------------------------------------------------------*
FORM display_alv_report.
  gd_repid = sy-repid.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = gd_repid
      is_layout          = gd_layout
      it_fieldcat        = fieldcatalog[]
      i_save             = 'X'
    TABLES
      t_outtab           = it_scarr
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " DISPLAY_ALV_REPORT


*&---------------------------------------------------------------------*
*&      Form  DATA_RETRIEVAL
*&---------------------------------------------------------------------*
*       Retrieve data form EKPO table and populate itab it_ekko
*----------------------------------------------------------------------*
FORM data_retrieval.
  DATA: ld_color(1) TYPE c.

  SELECT mandt carrid carrname currcode url
   UP TO NUM ROWS
    FROM scarr   "항공사 테이블
    INTO TABLE it_scarr
    WHERE carrid    IN s_carrid
      AND carrname  IN s_cname
      AND currcode  IN s_ccode
      AND url       IN s_url.
*    WHERE carrid = p_carrid.

ENDFORM.                    " DATA_RETRIEVAL