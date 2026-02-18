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
*&   - CARRID/CONNID 를 각각 두 개로 복제
*&     · 원본(CARRID/CONNID)  → no_out = X (숨김), 소계 기준으로만 사용
*&     · 복제(CARRID1/CONNID1)→ 화면 표시용
*&   - 소계 텍스트는 subtotal_text 콜백 폼에서 처리
*&     · p_total 을 FIELD-SYMBOLS 로 캐스팅하여 키값(항공사/노선) 추출
*&     · CARRID 소계 → "[항공사코드] 항공사 소계"
*&     · CONNID 소계 → "- [노선번호] 노선 소계"
*&   - totals_before_items = X → 소계 행을 데이터 상단에 표시
*&   - SORT 순서(spos)가 그룹 계층을 결정
*&   - DO_SUM이 설정된 필드(PRICE)만 집계 계산됨
*&---------------------------------------------------------------------*

REPORT Z5WEEK_ALV_SUBTOT_TEST01.

TABLES:     sflight.

TYPE-POOLS: slis.                                 "ALV Declarations

*Data Declaration
*----------------
TYPES: BEGIN OF t_sf,
  carrid    type sflight-carrid,
  carrid1   type sflight-carrid,
  connid    type sflight-connid,
  connid1   type sflight-connid,
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
  PERFORM alv_event_set.   " START-OF-SELECTION에 추가
  PERFORM display_alv_report.

*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       Build Fieldcatalog for ALV Report
*----------------------------------------------------------------------*
FORM build_fieldcatalog.

  " CARRID → 숨김 (소계 기준용)
  fieldcatalog-fieldname   = 'CARRID'.
  fieldcatalog-seltext_m   = '항공사코드'.
  fieldcatalog-col_pos     = 0.
  fieldcatalog-no_out      = 'X'. " 숨김
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  " CARRID1 → 화면 표시용
  fieldcatalog-fieldname   = 'CARRID1'.
  fieldcatalog-seltext_m   = '항공사코드'.
  fieldcatalog-col_pos     = 1.
  fieldcatalog-key         = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  " CONNID → 숨김 (소계 기준용)
  fieldcatalog-fieldname   = 'CONNID'.
  fieldcatalog-seltext_m   = '노선번호'.
  fieldcatalog-col_pos     = 2.
  fieldcatalog-no_out      = 'X'. " 숨김
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  " CONNID1 → 화면 표시용
  fieldcatalog-fieldname = 'CONNID1'.
  fieldcatalog-seltext_m = '노선번호'.
  fieldcatalog-col_pos   = 3.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR fieldcatalog.

  fieldcatalog-fieldname   = 'PRICE'.
  fieldcatalog-seltext_m   = '가격'.
  fieldcatalog-col_pos     = 4.
  fieldcatalog-do_sum      = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'CURRENCY'.
  fieldcatalog-seltext_m   = '기준통화'.
  fieldcatalog-col_pos     = 5.
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
      it_events                = gt_events[]
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

 SELECT carrid connid price currency
   FROM SFLIGHT
   INTO CORRESPONDING FIELDS OF TABLE it_sf.

  LOOP AT it_sf INTO wa_sf.
    wa_sf-carrid1 = wa_sf-carrid.   " 복사
    wa_sf-connid1 = wa_sf-connid.   " 복사
    MODIFY it_sf FROM wa_sf.
  ENDLOOP.

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
  it_sort-fieldname = 'CARRID'. " 숨긴 컬럼 기준으로 소계
  it_sort-up        = 'X'.
  it_sort-subtot    = 'X'.
  APPEND it_sort.
  CLEAR  it_sort.

  it_sort-spos      = '2'.
  it_sort-fieldname = 'CONNID'. " 숨긴 컬럼 기준으로 소계
  it_sort-up        = 'X'.
  it_sort-subtot    = 'X'.
  APPEND it_sort.
  CLEAR  it_sort.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  subtotal_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TOTAL        text
*      -->P_SUBTOT_TEXT  text
*----------------------------------------------------------------------*
*FORM subtotal_text CHANGING
*         p_total TYPE any
*         p_subtot_text TYPE slis_subtot_text.
*
*  " 항공사별 소계
*  IF p_subtot_text-criteria = 'CARRID'.
*    p_subtot_text-display_text_for_subtotal = '항공사 소계'.
*
*  " 노선별 소계
*  ELSEIF p_subtot_text-criteria = 'CONNID'.
*    p_subtot_text-display_text_for_subtotal = '노선 소계'.
*
*  ENDIF.
*
*ENDFORM.

FORM subtotal_text CHANGING
         p_total TYPE any
         p_subtot_text TYPE slis_subtot_text.

  FIELD-SYMBOLS: <fs_total> TYPE t_sf.
  ASSIGN p_total TO <fs_total>.

  IF p_subtot_text-criteria = 'CARRID'.
    CONCATENATE <fs_total>-carrid ' 항공사 소계'
      INTO p_subtot_text-display_text_for_subtotal.

  ELSEIF p_subtot_text-criteria = 'CONNID'.
    CONCATENATE '*' <fs_total>-connid ' 노선 소계'
      INTO p_subtot_text-display_text_for_subtotal.

  ENDIF.

ENDFORM.

FORM alv_event_set.
  CONSTANTS: c_formname TYPE slis_formname VALUE 'SUBTOTAL_TEXT'.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 4
    IMPORTING
      et_events   = gt_events.

  READ TABLE gt_events INTO ls_event
       WITH KEY name = slis_ev_subtotal_text.
  IF sy-subrc = 0.
    ls_event-form = c_formname.
    MODIFY gt_events FROM ls_event INDEX sy-tabix.
  ENDIF.

ENDFORM.