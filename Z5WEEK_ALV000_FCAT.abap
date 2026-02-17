*&---------------------------------------------------------------------*
*& Report Z5WEEK_ALV000_FCAT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z5WEEK_ALV000_FCAT.

TABLES:     sflight.

TYPE-POOLS: slis.                                 "ALV Declarations

*Data Declaration: 데이터 담기 (엑셀에 첫 행 구성)
*----------------
TYPES: BEGIN OF t_sf,
  carrid    TYPE sflight-carrid,
  connid  TYPE sflight-connid,
  seatsmax  TYPE sflight-seatsmax,
 END OF t_sf.

*it_scarr로 표를 만든거임. internal-table
*wa_scarr는 표는 아니지만, 5개 필드 들어갈 수 있는 공간 만든거임. work-area
DATA: it_sf TYPE STANDARD TABLE OF t_sf.

*ALV data declarations
DATA: fieldcatalog TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      gd_layout    TYPE slis_layout_alv,
      gd_repid     LIKE sy-repid.

DATA: it_sort      TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE.

************************************************************************
*Start-of-selection.
START-OF-SELECTION.

  PERFORM data_retrieval.
  PERFORM build_fieldcatalog.
  PERFORM build_layout.
  PERFORM build_sort.
  PERFORM display_alv_report.


*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       Build Fieldcatalog for ALV Report
*----------------------------------------------------------------------*
*제목을 붙이는 작업.
FORM build_fieldcatalog.

  fieldcatalog-fieldname   = 'CARRID'.
  fieldcatalog-seltext_m   = '항공사 코드'.
  fieldcatalog-col_pos     = 0.
  fieldcatalog-outputlen   = 10.
  fieldcatalog-key         = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'CONNID'.
  fieldcatalog-seltext_l   = '비행기번호를넣는다코드를추가안녕안녕와우안녕제발제발'. "디버깅 하면서
  fieldcatalog-col_pos     = 1.
  fieldcatalog-key         = 'X'. "key 표시.
  fieldcatalog-lzero       = 'X'. "0 포함 출력.
  fieldcatalog-just        = 'L'. "왼쪽 정렬.
  fieldcatalog-hotspot        = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'SEATSMAX'.
  fieldcatalog-seltext_m   = '좌석수'.
  fieldcatalog-col_pos     = 2.
*  fieldcatalog-emphasize   = 'X'. "강조
  fieldcatalog-emphasize   = 'C310'. "노락색으로 강조
  fieldcatalog-do_sum      = 'X'. "합계
  fieldcatalog-edit      = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.
ENDFORM.                    " BUILD_FIELDCATALOG


*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       Build layout for ALV grid report
*----------------------------------------------------------------------*
FORM build_layout.

  gd_layout-no_input            = 'X'.
  gd_layout-colwidth_optimize   = 'X'.
  gd_layout-zebra               = 'X'.
  gd_layout-totals_before_items = 'X'. "합계를 맨 윗줄로

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
      is_layout          = gd_layout "위에 붙인 3가지 옵션(build_layout)
      it_fieldcat        = fieldcatalog[] "필드별 제목 넣었던 거.
      it_sort            = it_sort[]
      i_save             = 'X'
    TABLES
      t_outtab           = it_sf "내가 담은 테이블 출력
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " DISPLAY_ALV_REPORT

CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
* EXPORTING
*   I_CALLBACK_PROGRAM                = ' '
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
*   I_CALLBACK_TOP_OF_PAGE            = ' '
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   I_STRUCTURE_NAME                  =
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
*   IS_LAYOUT                         =
*   IT_FIELDCAT                       =
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
*   IT_SORT                           =
*   IT_FILTER                         =
*   IS_SEL_HIDE                       =
*   I_DEFAULT                         = 'X'
*   I_SAVE                            = ' '
*   IS_VARIANT                        =
*   IT_EVENTS                         =
*   IT_EVENT_EXIT                     =
*   IS_PRINT                          =
*   IS_REPREP_ID                      =
*   I_SCREEN_START_COLUMN             = 0
*   I_SCREEN_START_LINE               = 0
*   I_SCREEN_END_COLUMN               = 0
*   I_SCREEN_END_LINE                 = 0
*   I_HTML_HEIGHT_TOP                 = 0
*   I_HTML_HEIGHT_END                 = 0
*   IT_ALV_GRAPHICS                   =
*   IT_HYPERLINK                      =
*   IT_ADD_FIELDCAT                   =
*   IT_EXCEPT_QINFO                   =
*   IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER           =
*   ES_EXIT_CAUSED_BY_USER            =
  TABLES
    t_outtab                          =
* EXCEPTIONS
*   PROGRAM_ERROR                     = 1
*   OTHERS                            = 2
          .
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.


*&---------------------------------------------------------------------*
*&      Form  DATA_RETRIEVAL
*&---------------------------------------------------------------------*
FORM data_retrieval.
  DATA: ld_color(1) TYPE c.

  SELECT carrid connid seatsmax
*   UP TO 10 ROWS
    FROM sflight
    INTO TABLE it_sf.

ENDFORM.                    " DATA_RETRIEVAL

*&---------------------------------------------------------------------*
*& Form BUILD_SORT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM build_sort .
  it_sort-spos        = 1.
  it_sort-fieldname   = 'CARRID'. "필드이름은 대문자로. 소문자 쓰다 인식 안되는 경우 존재.
  it_sort-up          = 'X'. "asc 오름차순
*  it_sort-down          = 'X'.
  it_sort-subtot        = 'X'.
  APPEND  it_sort TO it_sort.
  CLEAR   it_sort.

  it_sort-spos        = 2.
  it_sort-fieldname   = 'CONNID'.
  it_sort-down        = 'X'.
  it_sort-subtot        = 'X'.
*  it_sort-expa        = 'X'.
  APPEND  it_sort TO it_sort.
  CLEAR   it_sort.

ENDFORM.