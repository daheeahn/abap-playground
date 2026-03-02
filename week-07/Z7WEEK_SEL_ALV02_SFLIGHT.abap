*&---------------------------------------------------------------------*
*& Report Z7WEEK_SEL_ALV02_SFLIGHT
*&
*& SFLIGHT 테이블의 항공편 정보를
*& 여러 검색조건(항공사, 노선, 운항일자, 운임, 통화, 기종, 최대좌석수)과
*& 최대 조회 건수(NUM) 파라미터를 적용하여
*& ALV Grid 형식으로 조회하는 예제 프로그램
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z7WEEK_SEL_ALV02_SFLIGHT. 

TABLES:     sflight.

TYPE-POOLS: slis.                                 "ALV Declarations

*Data Declaration
*----------------
TYPES: BEGIN OF t_sflight,
  carrid    TYPE sflight-carrid,
  connid    TYPE sflight-connid,
  fldate    TYPE sflight-fldate,
  price     TYPE sflight-price,
  currency  TYPE sflight-currency,
  planetype TYPE sflight-planetype,
  seatsmax  TYPE sflight-seatsmax,
 END OF t_sflight.

DATA: it_sflight TYPE STANDARD TABLE OF t_sflight.

*ALV data declarations
DATA: fieldcatalog TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      gd_tab_group TYPE slis_t_sp_group_alv,
      gd_layout    TYPE slis_layout_alv,
      gd_repid     LIKE sy-repid.

* 검색화면
SELECTION-SCREEN BEGIN OF BLOCK part1 WITH FRAME TITLE text-001.
SELECT-OPTIONS s_carrid    FOR sflight-carrid.
SELECT-OPTIONS s_connid    FOR sflight-connid.
SELECT-OPTIONS s_fldate    FOR sflight-fldate.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS s_price     FOR sflight-price.
SELECT-OPTIONS s_curr      FOR sflight-currency.
SELECT-OPTIONS s_ptype     FOR sflight-planetype.
SELECT-OPTIONS s_smax      FOR sflight-seatsmax.
SELECTION-SCREEN SKIP.
PARAMETERS NUM TYPE I. " 파라미터로 숫자 받아오도록
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

  fieldcatalog-fieldname   = 'CARRID'.
  fieldcatalog-seltext_m   = '항공사 코드'.
  fieldcatalog-col_pos     = 0.
  fieldcatalog-outputlen   = 10.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'CONNID'.
  fieldcatalog-seltext_m   = '항공편 번호'.
  fieldcatalog-col_pos     = 1.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'FLDATE'.
  fieldcatalog-seltext_m   = '운항 일자'.
  fieldcatalog-col_pos     = 2.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'PRICE'.
  fieldcatalog-seltext_m   = '운임'.
  fieldcatalog-col_pos     = 3.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'CURRENCY'.
  fieldcatalog-seltext_m   = '통화'.
  fieldcatalog-col_pos     = 4.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'PLANETYPE'.
  fieldcatalog-seltext_m   = '항공기 기종'.
  fieldcatalog-col_pos     = 5.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'SEATSMAX'.
  fieldcatalog-seltext_m   = '최대 좌석 수'.
  fieldcatalog-col_pos     = 6.
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
      t_outtab           = it_sflight
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

  SELECT carrid connid fldate price currency planetype seatsmax
   UP TO NUM ROWS
    FROM sflight
    INTO TABLE it_sflight
    WHERE carrid    IN s_carrid
      AND connid    IN s_connid
      AND fldate    IN s_fldate
      AND price     IN s_price
      AND currency  IN s_curr
      AND planetype IN s_ptype
      AND seatsmax  IN s_smax.

ENDFORM.                    " DATA_RETRIEVAL