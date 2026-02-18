*&---------------------------------------------------------------------*
*& Report  Z5WEEK_ALV_SUBTOT_TEST01
*&---------------------------------------------------------------------*
*& Purpose:
*&   - SFLIGHT 데이터를 ALV Grid로 출력
*&   - 항공사별(CARRID) 및 노선별(CONNID) Subtotal 구조 테스트
*&
*& Subtotal Structure:
*&   1) CARRID  → 상위 그룹 (항공사별 소계)
*&   2) CONNID  → 하위 그룹 (노선별 소계)
*&   3) PRICE   → DO_SUM 적용 (합계 대상 필드)
*&
*& Key Concept:
*&   - SORT 순서(spos)가 그룹 계층을 결정
*&   - SUBTOT은 해당 정렬 레벨 종료 시 소계 발생
*&   - DO_SUM이 설정된 필드만 집계 계산됨
*&---------------------------------------------------------------------*

REPORT Z5WEEK_ALV_SUBTOT_TEST01.

TABLES:     sflight.

TYPE-POOLS: slis.                                 "ALV Declarations

*Data Declaration
*----------------
TYPES: BEGIN OF t_sf,
  carrid    type sflight-carrid,
  connid    type sflight-connid,
  price     type sflight-price,
  currency  type sflight-currency,

 END OF t_sf.

DATA: it_sf TYPE STANDARD TABLE OF t_sf,
      wa_sf TYPE t_sf.

*ALV data declarations
DATA: fieldcatalog TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      gd_tab_group TYPE slis_t_sp_group_alv,
      gd_layout    TYPE slis_layout_alv,
      gd_repid     LIKE sy-repid.
DATA: it_sort      TYPE slis_t_sortinfo_alv WITH HEADER LINE.
*Alv Event
DATA: gt_events TYPE slis_t_event,
      ls_event TYPE slis_alv_event.

*DATA : t TYPE slis_t_sp_group_alv .
************************************************************************
*Start-of-selection.
START-OF-SELECTION.

  PERFORM data_retrieval.
  PERFORM build_fieldcatalog.
  PERFORM build_layout.
  PERFORM build_sort.
  PERFORM alv_event_set.
  PERFORM display_alv_report.

*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       Build Fieldcatalog for ALV Report
*----------------------------------------------------------------------*
FORM build_fieldcatalog.

  fieldcatalog-fieldname   = 'CARRID'.
  fieldcatalog-seltext_m   = '항공사코드'.
  fieldcatalog-col_pos     = 0.
  fieldcatalog-key         = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'CONNID'.
  fieldcatalog-seltext_m   = '노선번호'.
  fieldcatalog-col_pos     = 1.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'PRICE'.
  fieldcatalog-seltext_m   = '가격'.
  fieldcatalog-col_pos     = 2.
  fieldcatalog-do_sum      = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'CURRENCY'.
  fieldcatalog-seltext_m   = '기준통화'.
  fieldcatalog-col_pos     = 3.
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
      it_sort            = it_sort[]
      it_events          = gt_events[]
      i_save             = 'X'
    TABLES
      t_outtab           = it_sf
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
FORM data_retrieval.

 SELECT carrid connid price currency
   FROM SFLIGHT
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

*&---------------------------------------------------------------------*
*&      Form  alv_event_set
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_event_set .
  CONSTANTS : c_formname_subtotal_text TYPE slis_formname VALUE
                                                      'SUBTOTAL_TEXT'.
*  DATA: l_s_event TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type     = 4
    IMPORTING
      et_events       = gt_events
    EXCEPTIONS
      list_type_wrong = 0
      OTHERS          = 0.


  "* Subtotal
  READ TABLE gt_events  INTO ls_event
                    WITH KEY name = slis_ev_subtotal_text.
  IF sy-subrc = 0.
    MOVE c_formname_subtotal_text TO ls_event-form.
    MODIFY gt_events FROM ls_event INDEX sy-tabix.
  ENDIF.

ENDFORM.                    " alv_event_set

*&---------------------------------------------------------------------*
*&      Form  subtotal_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TOTAL        text
*      -->P_SUBTOT_TEXT  text
*----------------------------------------------------------------------*
FORM subtotal_text CHANGING
               p_total TYPE any
               p_subtot_text TYPE slis_subtot_text.
* Patient Level Text
*  IF p_subtot_text-criteria = 'CARRID'.
*    p_subtot_text-display_text_for_subtotal
*                 = 'Patient Total'(011).
*  ENDIF.

**  Material Group Text
*  IF p_subtot_text-criteria = 'WGBEZ'.
*    p_subtot_text-display_text_for_subtotal = 'Material Group Total'(012) .
*  ENDIF.

ENDFORM.                    "subtotal_text