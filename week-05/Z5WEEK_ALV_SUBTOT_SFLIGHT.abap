*&---------------------------------------------------------------------*
*& Report  Z5WEEK_ALV_SUBTOT_SFLIGHT
*&---------------------------------------------------------------------*
*& Purpose:
*&   - SFLIGHT 테이블 데이터를 ALV Grid로 출력
*&   - 항공사(CARRID) → 노선(CONNID) 계층 정렬 및 Subtotal 생성
*&
*& Main Features:
*&   1) CARRID / CONNID 기준 그룹 정렬 및 소계(Subtotal)
*&   2) PRICE, SEATSMAX 합계(Total) 계산
*&   3) 기준통화 / 비행기 기종 컬럼 색상 강조 (EMPHASIZE)
*&   4) CONNID 컬럼 Hotspot 설정
*&   5) Read-only ALV + Zebra + 컬럼 자동 너비
*&
*& Learning Focus:
*&   - Sort + Subtotal 구조 이해
*&   - Fieldcatalog 주요 옵션 활용
*&   - ALV Layout 옵션 적용 실습
*&---------------------------------------------------------------------*

REPORT Z5WEEK_ALV_SUBTOT_SFLIGHT.

TABLES:     sflight.

TYPE-POOLS: slis.                                 "ALV Declarations

*Data Declaration
*----------------
TYPES: BEGIN OF t_sf,
  carrid    type sflight-carrid,
  connid    type sflight-connid,
  fldate    type sflight-fldate,
  price     type sflight-price,
  currency  type sflight-currency,
  planetype type sflight-planetype,
  seatsmax  type sflight-seatsmax,

 END OF t_sf.

DATA: it_sf TYPE STANDARD TABLE OF t_sf.

*ALV data declarations
DATA: fieldcatalog TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      gd_layout    TYPE slis_layout_alv,
      gd_repid     LIKE sy-repid.
DATA: it_sort      TYPE slis_t_sortinfo_alv WITH HEADER LINE.

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
FORM build_fieldcatalog.

  " CARRID
  fieldcatalog-fieldname   = 'CARRID'.
  fieldcatalog-seltext_m   = '항공사코드'.
  fieldcatalog-col_pos     = 0.
  fieldcatalog-key         = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  " CONNID
  fieldcatalog-fieldname   = 'CONNID'.
  fieldcatalog-seltext_m   = '노선번호'.
  fieldcatalog-col_pos     = 1.
  fieldcatalog-key         = 'X'.
  fieldcatalog-lzero       = 'X'.
  fieldcatalog-hotspot     = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  " FLDATE
  fieldcatalog-fieldname   = 'FLDATE'.
  fieldcatalog-seltext_m   = '비행날짜'.
  fieldcatalog-col_pos     = 2.
  fieldcatalog-key         = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  " PRICE
  fieldcatalog-fieldname   = 'PRICE'.
  fieldcatalog-seltext_m   = '가격'.
  fieldcatalog-col_pos     = 3.
  fieldcatalog-do_sum      = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  " CURRENCY
  fieldcatalog-fieldname   = 'CURRENCY'.
  fieldcatalog-seltext_m   = '기준통화'.
  fieldcatalog-col_pos     = 4.
  fieldcatalog-emphasize   = 'C500'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  " PLANETYPE
  fieldcatalog-fieldname   = 'PLANETYPE'.
  fieldcatalog-seltext_m   = '비행기기종'.
  fieldcatalog-emphasize   = 'C100'.
  fieldcatalog-col_pos     = 5.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  " SEATSMAX
  fieldcatalog-fieldname   = 'SEATSMAX'.
  fieldcatalog-seltext_m   = '좌석수'.
  fieldcatalog-col_pos     = 6.
  fieldcatalog-do_sum      = 'X'.
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
  gd_layout-totals_before_items   = 'X'.

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
      i_callback_program       = gd_repid
      is_layout                = gd_layout
      it_fieldcat              = fieldcatalog[]
      it_sort                  = it_sort[]
      i_save                   = 'X'
    TABLES
      t_outtab                 = it_sf
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  DATA_RETRIEVAL
*&---------------------------------------------------------------------*
FORM data_retrieval.

 SELECT carrid connid fldate price currency planetype seatsmax
   FROM SFLIGHT
   INTO CORRESPONDING FIELDS OF TABLE it_sf.

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
  it_sort-spos      = '1'.
  it_sort-fieldname = 'CARRID'.
  it_sort-up        = 'X'.
  it_sort-subtot    = 'X'.
  APPEND it_sort.
  CLEAR  it_sort.

  it_sort-spos      = '2'.
  it_sort-fieldname = 'CONNID'.
  it_sort-up        = 'X'.
  it_sort-subtot    = 'X'.
  APPEND it_sort.
  CLEAR  it_sort.
ENDFORM.