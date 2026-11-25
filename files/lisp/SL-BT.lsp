;;; =====================================================================
;;;  SL-BT.lsp  -  블록 안의 문자를 블록을 깨지 않고 수정 (EXPLODE 없음)
;;;  명령어 : SL-BT
;;;  대상   : AutoCAD 2007 한글판 / Windows 한글 (CP949, BOM 없음)
;;; =====================================================================
;;;  사용법
;;;    1) 명령창에  APPLOAD  -> 이 파일 선택 -> 로드
;;;    2) 명령창에  SL-BT
;;;    3) 수정할 블록(또는 블록 안의 글자)을 클릭
;;;    4) 새 글자 입력 -> Enter
;;;    5) 계속 클릭해서 수정, 끝낼 때 Enter 또는 ESC
;;;
;;;  동작
;;;    * 블록 정의 안의 TEXT / MTEXT 를 찾아 글자만 바꿉니다. 블록은 그대로.
;;;    * 그 블록이 도면에 하나뿐이면 그 블록을 직접 고칩니다.
;;;    * 같은 블록이 여러 개면 묻지 않고, 선택한 그 블록만 새 블록(SL_BT_n)으로
;;;      자동으로 나눠서 고칩니다. 나머지 블록은 그대로입니다.
;;;    * 블록 복제는 entmake 만 씁니다. (AutoCAD 2007 처럼 ActiveX 블록 복사가
;;;      안 되는 버전에서도 확실히 동작합니다. 블록 안 객체를 하나씩 그대로 옮겨 담습니다)
;;;    * 속성(ATTRIB)이 있는 블록은 그 블록의 속성값만 바꿉니다.
;;;    * 작업 전체가 UNDO 한 번에 되돌아갑니다.
;;; =====================================================================

(if (member (type vl-load-com) (list 'SUBR 'USUBR 'EXRXSUBR))
  (vl-load-com)
)

(setq *SL-COM*     nil)
(setq *SL-DOC*     nil)
(setq *SL-UNDO*    nil)
(setq *SL-CMDECHO* nil)
(setq *SL-STOP*    nil)
(setq *SL-EDITS*   0)

;;; ---------------------------------------------------------------------
;;;  공통 유틸
;;; ---------------------------------------------------------------------

;; ssget 필터에서 와일드카드로 해석되지 않도록 이름을 그대로 이스케이프
;; (*A, *U 같은 익명 블록 이름이 다른 블록까지 잡는 것을 막는다)
(defun sl:esc (s / i n ch out)
  (setq i 1 n (strlen s) out "")
  (repeat n
    (setq ch (substr s i 1))
    (if (member ch (list "*" "?" "[" "]" "#" "@" "." "," "~" "`"))
      (setq out (strcat out "`" ch))
      (setq out (strcat out ch))
    )
    (setq i (1+ i))
  )
  out
)

;; Visual LISP(ActiveX) 사용 가능 여부 - 한 번만 검사
(defun sl:com ( / res)
  (if (null *SL-COM*)
    (setq *SL-COM*
      (cond
        ((not (member (type vl-catch-all-apply) (list 'SUBR 'USUBR 'EXRXSUBR))) nil)
        ((not (member (type vl-load-com) (list 'SUBR 'USUBR 'EXRXSUBR))) nil)
        (T
         (setq res (vl-catch-all-apply
                     '(lambda () (vl-load-com) (vlax-get-acad-object))
                     nil
                   )
         )
         (if (or (null res) (vl-catch-all-error-p res)) nil T)
        )
      )
    )
  )
  *SL-COM*
)

;; ActiveX 호출을 안전하게 (오류면 nil)
(defun sl:vcall (fn args / res)
  (if (not (sl:com))
    nil
    (progn
      (setq res (vl-catch-all-apply fn args))
      (if (vl-catch-all-error-p res) nil res)
    )
  )
)

(defun sl:doc ( / app)
  (if (null *SL-DOC*)
    (progn
      (setq app (sl:vcall 'vlax-get-acad-object nil))
      (if app (setq *SL-DOC* (sl:vcall 'vla-get-ActiveDocument (list app))))
    )
  )
  *SL-DOC*
)

;; 화면 갱신 (블록 정의를 고쳤을 때 반드시 필요)
(defun sl:regen ( / d r typ)
  (setq r nil)
  (setq typ (if (boundp 'acActiveViewport) acActiveViewport 0))
  (if (setq d (sl:doc))
    (progn
      (setq r (vl-catch-all-apply 'vla-Regen (list d typ)))
      (if (vl-catch-all-error-p r) (setq r nil) (setq r T))
    )
  )
  (if (null r) (command "_.REGEN"))
  (princ)
)

;;; ---------------------------------------------------------------------
;;;  문자열 처리
;;; ---------------------------------------------------------------------

;; MTEXT 서식코드를 걷어낸 화면 표시용 문자열
(defun sl:plain (s / i n ch nx out)
  (setq i 1 n (strlen s) out "")
  (while (<= i n)
    (setq ch (substr s i 1))
    (cond
      ((= ch "\\")
       (setq i (1+ i))
       (if (<= i n)
         (progn
           (setq nx (substr s i 1))
           (cond
             ((= nx "P") (setq out (strcat out " ")))
             ((member nx (list "f" "F" "H" "W" "Q" "T" "A" "C" "c" "p" "S"))
              (while (and (<= i n) (/= ";" (substr s i 1)))
                (setq i (1+ i))
              )
             )
             ((member nx (list "L" "l" "O" "o" "K" "k" "~")) nil)
             (T (setq out (strcat out nx)))
           )
         )
       )
      )
      ((member ch (list "{" "}")) nil)
      (T (setq out (strcat out ch)))
    )
    (setq i (1+ i))
  )
  out
)

;; 화면에 보여줄 형태 (따옴표 + 길이 제한)
(defun sl:show (s / p)
  (setq p (sl:plain (if s s "")))
  (if (> (strlen p) 60) (setq p (strcat (substr p 1 60) "...")))
  (strcat "\"" p "\"")
)

;; 문자 값 읽기
(defun sl:val (en / ed s)
  (setq ed (entget en))
  (if ed
    (progn
      (setq s (cdr (assoc 1 ed)))
      (if s s "")
    )
    ""
  )
)

;; DXF 리스트의 문자값(1) 교체 + MTEXT 이어붙이기(3) 제거
(defun sl:setstr (ed val / res hit x)
  (setq res nil hit nil)
  (foreach x ed
    (cond
      ((= (car x) 1) (setq res (cons (cons 1 val) res)) (setq hit T))
      ((= (car x) 3) nil)
      (T (setq res (cons x res)))
    )
  )
  (if (not hit) (setq res (cons (cons 1 val) res)))
  (reverse res)
)

;; 문자 값을 쓴다 (entmod 우선 - AutoCAD 2007에서 확실, 안 되면 VLA)
(defun sl:settext (en val / ed ok o)
  (setq ok nil)
  (setq ed (entget en))
  (cond
    ((and ed (assoc -1 ed))
     (if (entmod (sl:setstr ed val))
       (if (equal (sl:val en) val) (setq ok T))
     )
    )
  )
  (cond
    ((null ok)
     (setq o (sl:vcall 'vlax-ename->vla-object (list en)))
     (if o
       (progn
         (sl:vcall 'vla-put-TextString (list o val))
         (if (equal (sl:val en) val) (setq ok T))
       )
     )
    )
  )
  ok
)

;;; ---------------------------------------------------------------------
;;;  블록 안 문자 찾기 / 거리 계산
;;; ---------------------------------------------------------------------

;; 블록 정의 안에 직접 들어있는 TEXT / MTEXT
(defun sl:blocktexts (bname / blk e ed res)
  (setq blk (tblobjname "BLOCK" bname) res nil)
  (if blk
    (progn
      (setq e (entnext blk))
      (while (and e (setq ed (entget e)) (/= "ENDBLK" (cdr (assoc 0 ed))))
        (if (member (cdr (assoc 0 ed)) (list "TEXT" "MTEXT"))
          (setq res (cons e res))
        )
        (setq e (entnext e))
      )
    )
  )
  (reverse res)
)

;; 한 단계 안쪽 블록의 문자 ((문자 . 블록이름) 목록)
(defun sl:nesttexts (bname / blk e ed res sub v)
  (setq blk (tblobjname "BLOCK" bname) res nil)
  (if blk
    (progn
      (setq e (entnext blk))
      (while (and e (setq ed (entget e)) (/= "ENDBLK" (cdr (assoc 0 ed))))
        (if (= "INSERT" (cdr (assoc 0 ed)))
          (progn
            (setq sub (cdr (assoc 2 ed)))
            (foreach v (sl:blocktexts sub)
              (setq res (cons (cons v sub) res))
            )
          )
        )
        (setq e (entnext e))
      )
    )
  )
  (reverse res)
)

;; 블록 참조의 속성 목록 ((속성엔티티 . 값) ...)
(defun sl:attrs (en / e ed res)
  (setq e (entnext en) res nil)
  (while (and e (setq ed (entget e)) (= "ATTRIB" (cdr (assoc 0 ed))))
    (setq res (cons (cons e (cdr (assoc 1 ed))) res))
    (setq e (entnext e))
  )
  (reverse res)
)

;; 문자 기준점 (TEXT 는 정렬점 고려)
(defun sl:anchor (ed / typ j72 j73 p)
  (setq typ (cdr (assoc 0 ed)))
  (if (= typ "MTEXT")
    (cdr (assoc 10 ed))
    (progn
      (setq j72 (cdr (assoc 72 ed)) j73 (cdr (assoc 73 ed)))
      (setq p (cdr (assoc 11 ed)))
      (if (and p (or (and j72 (/= j72 0)) (and j73 (/= j73 0))))
        p
        (cdr (assoc 10 ed))
      )
    )
  )
)

;; 글자 폭 추정 (한글 1.1 / 영문 0.62)
(defun sl:strw (s / i n tot)
  (setq i 1 n (strlen s) tot 0.0)
  (repeat n
    (setq tot (+ tot (if (> (ascii (substr s i 1)) 127) 0.55 0.62)))
    (setq i (1+ i))
  )
  tot
)

;; 점 - 선분 거리
(defun sl:d2seg (p a b / ax ay bx by px py vx vy wx wy dd tt)
  (setq ax (car a) ay (cadr a) bx (car b) by (cadr b) px (car p) py (cadr p))
  (setq vx (- bx ax) vy (- by ay))
  (setq wx (- px ax) wy (- py ay))
  (setq dd (+ (* vx vx) (* vy vy)))
  (if (< dd 1e-12)
    (sqrt (+ (* wx wx) (* wy wy)))
    (progn
      (setq tt (/ (+ (* wx vx) (* wy vy)) dd))
      (if (< tt 0.0) (setq tt 0.0))
      (if (> tt 1.0) (setq tt 1.0))
      (setq vx (- px (+ ax (* tt vx))))
      (setq vy (- py (+ ay (* tt vy))))
      (sqrt (+ (* vx vx) (* vy vy)))
    )
  )
)

;; 문자 높이
(defun sl:tht (en / h)
  (setq h (cdr (assoc 40 (entget en))))
  (if (or (null h) (<= h 0.0)) 1.0 h)
)

;; 블록 좌표계의 한 점에서 문자까지의 거리
(defun sl:dist (en bp / ed typ h wf rot anch w p2)
  (setq ed (entget en))
  (setq typ (cdr (assoc 0 ed)))
  (setq h (sl:tht en))
  (setq wf (cdr (assoc 41 ed)))
  (setq rot (cdr (assoc 50 ed)))
  (if (null rot) (setq rot 0.0))
  (setq anch (sl:anchor ed))
  (if (null anch) (setq anch (list 0.0 0.0 0.0)))
  (if (= typ "MTEXT")
    (progn
      (setq w (* h (sl:strw (sl:val en))))
      (if (and wf (> wf 0.0)) (setq w wf))
    )
    (progn
      (if (or (null wf) (<= wf 0.0)) (setq wf 1.0))
      (setq w (* h wf (sl:strw (sl:val en))))
    )
  )
  (if (< w (* h 0.6)) (setq w (* h 0.6)))
  (setq p2 (list (+ (car anch) (* w (cos rot)))
                 (+ (cadr anch) (* w (sin rot)))
           )
  )
  (sl:d2seg bp anch p2)
)

;; 삽입점(UCS)을 블록 좌표계로
(defun sl:wcs2blk (wpt en bname / ed ip sx sy rot base cc ss dx dy ux uy)
  (setq ed (entget en))
  (setq ip (cdr (assoc 10 ed)))
  (setq sx (cdr (assoc 41 ed)))
  (setq sy (cdr (assoc 42 ed)))
  (setq rot (cdr (assoc 50 ed)))
  (if (null rot) (setq rot 0.0))
  (if (or (null sx) (< (abs sx) 1e-9)) (setq sx 1.0))
  (if (or (null sy) (< (abs sy) 1e-9)) (setq sy 1.0))
  (setq base (cdr (assoc 10 (entget (tblobjname "BLOCK" bname)))))
  (if (null base) (setq base (list 0.0 0.0 0.0)))
  (setq cc (cos rot) ss (sin rot))
  (setq dx (- (car wpt) (car ip)))
  (setq dy (- (cadr wpt) (cadr ip)))
  (setq ux (+ (* dx cc) (* dy ss)))
  (setq uy (- (* dy cc) (* dx ss)))
  (list (+ (car base) (/ ux sx))
        (+ (cadr base) (/ uy sy))
        0.0
  )
)

;;; ---------------------------------------------------------------------
;;;  정렬 / 입력
;;; ---------------------------------------------------------------------

(defun sl:ins (x lst / res done y)
  (setq res nil done nil)
  (foreach y lst
    (if (and (not done) (< (car x) (car y)))
      (progn (setq res (cons x res)) (setq done T))
    )
    (setq res (cons y res))
  )
  (if (not done) (setq res (cons x res)))
  (reverse res)
)

(defun sl:sort (lst / res x)
  (setq res nil)
  (foreach x lst
    (setq res (sl:ins x res))
  )
  res
)

(defun sl:getnum (prompt dflt maxn / n ok)
  (setq ok nil n dflt)
  (while (not ok)
    (setq n (getint (strcat prompt " <" (itoa dflt) ">: ")))
    (cond
      ((null n) (setq n dflt ok T))
      ((and (>= n 0) (<= n maxn)) (setq ok T))
      (T (princ "\n   범위를 벗어났습니다. 다시 입력하세요."))
    )
  )
  n
)

;; 새 글자 입력 (Enter 면 nil = 변경 안 함)
(defun sl:askstr (old / s)
  (setq s (getstring T (strcat "\n[SL-BT] 새 글자 입력 <" (sl:plain old) "> (Enter=그대로): ")))
  (if (= s "") nil s)
)

;;; ---------------------------------------------------------------------
;;;  클릭한 문자 고르기
;;; ---------------------------------------------------------------------

;; 클릭 지점에서 잡힌 문자(중첩 포함)의 핸들을 비교
;; nentselp 결과는 (엔티티 매트릭스 [중첩목록]) 또는 ((엔티티...) 매트릭스) 형태이므로
;; 나온 엔티티 이름을 모두 모아 핸들로 비교한다.
(defun sl:hit (texts pt / res n1 h e v lst)
  (setq res nil)
  (if (member (type vl-catch-all-apply) (list 'SUBR 'USUBR 'EXRXSUBR))
    (progn
      (setq n1 (vl-catch-all-apply 'nentselp (list pt)))
      (if (vl-catch-all-error-p n1) (setq n1 nil))
      (setq lst nil)
      (foreach v n1
        (cond
          ((= (type v) 'ENAME) (setq lst (cons v lst)))
          ((and (listp v) (= (type (car v)) 'ENAME))
           (foreach e v (setq lst (cons e lst)))
          )
        )
      )
      (foreach v lst
        (if (null res)
          (progn
            (setq h (cdr (assoc 5 (entget v))))
            (foreach e texts
              (if (and (null res) (equal h (cdr (assoc 5 (entget e)))))
                (setq res e)
              )
            )
          )
        )
      )
    )
  )
  res
)

;; 문자 하나 고르기
(defun sl:choose (texts pt en bname / bp hit lst n lstout item i shown)
  (setq bp (sl:wcs2blk (trans pt 1 0) en bname))
  (setq hit (sl:hit texts pt))
  (cond
    (hit hit)
    ((= (length texts) 1) (car texts))
    (T
     (setq lst nil)
     (foreach item texts
       (setq lst (cons (list (sl:dist item bp) item) lst))
     )
     (setq lst (sl:sort lst))
     (if (< (car (car lst)) (* 0.5 (sl:tht (cadr (car lst)))))
       (cadr (car lst))
       (progn
         (setq n (length lst))
         (setq shown (if (> n 9) 9 n))
         (princ (strcat "\n[SL-BT] 이 블록 안의 글자 " (itoa n) "개 (클릭한 곳에서 가까운 순)"))
         (setq i 0)
         (foreach item lst
           (setq i (1+ i))
           (if (<= i shown)
             (princ (strcat "\n   [" (itoa i) "] " (sl:show (sl:val (cadr item)))))
           )
         )
         (if (> n shown)
           (princ (strcat "\n   ... 외 " (itoa (- n shown)) "개  (0 = 취소)"))
         )
         (setq lstout (sl:getnum "\n번호 선택" 1 shown))
         (if (= lstout 0) nil (cadr (nth (1- lstout) lst)))
       )
     )
    )
  )
)

;;; ---------------------------------------------------------------------
;;;  블록 개수 세기 (와일드카드 이스케이프 주의)
;;; ---------------------------------------------------------------------

(defun sl:count (bname / ss)
  (setq ss (ssget "_X" (list '(0 . "INSERT") (cons 2 (sl:esc bname)))))
  (if ss (sslength ss) 0)
)

;;; ---------------------------------------------------------------------
;;;  블록 정의 복제 (같은 블록이 여러 개일 때 "선택한 것만")
;;; ---------------------------------------------------------------------

(defun sl:newname (n0 / i nm done)
  (setq i (if n0 n0 1) done nil nm "SL_BT_1")
  (while (not done)
    (setq nm (strcat "SL_BT_" (itoa i)))
    (if (null (tblobjname "BLOCK" nm))
      (setq done T)
      (setq i (1+ i))
    )
    (if (> i 9000) (setq done T))
  )
  nm
)

;; 레이어 잠김 확인
(defun sl:locked (en / lay lrec f70)
  (setq lay (cdr (assoc 8 (entget en))))
  (setq lrec (tblsearch "LAYER" lay))
  (if (and lrec (setq f70 (cdr (assoc 70 lrec))))
    (/= 0 (logand 4 f70))
    nil
  )
)

;; 블록 정의의 기준점 (없으면 0,0,0)
(defun sl:origin (bname / ed p)
  (setq ed (entget (tblobjname "BLOCK" bname)))
  (setq p (if ed (cdr (assoc 10 ed))))
  (if p p (list 0.0 0.0 0.0))
)

;; 엔티티 하나를 entmake 로 새로 만들 수 있는 DXF 리스트로 손질한다.
;;   핸들(-1), 핸들값(5), 소유자(330), 확장사전(360), 102 그룹, XDATA(1000 이상)를 뺀다.
;;   (그대로 넣으면 entmake 가 실패하거나 다른 객체와 핸들이 겹친다)
(defun sl:mkdxf (ed / res x c)
  (setq res nil)
  (foreach x ed
    (setq c (car x))
    (if (not (or (member c (list -1 5 330 360 102))
                 (and (numberp c) (>= c 1000))
             )
        )
      (setq res (cons x res))
    )
  )
  (reverse res)
)

;; 블록 정의 안의 첫 엔티티 (블록이 비어 있으면 nil)
(defun sl:blkfirst (bname / blk e)
  (setq blk (tblobjname "BLOCK" bname) e nil)
  (if blk
    (progn
      (setq e (entnext blk))
      (if (null e) (setq e (cdr (assoc -2 (entget blk)))))
    )
  )
  e
)

;; 실패해서 남은 임시 블록 정의를 지운다 (아직 어디에도 안 쓰이므로 PURGE 로 지워진다)
(defun sl:delblock (nm / blk)
  (setq blk (tblobjname "BLOCK" nm))
  (if (and blk (member (type vl-catch-all-apply) (list 'SUBR 'USUBR 'EXRXSUBR)))
    (progn
      (vl-catch-all-apply 'entdel (list blk))
      (if (tblobjname "BLOCK" nm)
        (vl-catch-all-apply
          '(lambda (n) (command "_.-PURGE" "_B" n "_N"))
          (list nm)
        )
      )
    )
  )
  nil
)

;; 빈 블록 정의를 entmake 로 만들고 그 이름을 돌려준다. (실패하면 nil)
;;   예전 실패로 이름만 남은(지워진) 블록이 있으면 PURGE 로 치우고 다음 이름으로 다시 시도한다.
(defun sl:mkblock (bname / nm base tries ok)
  (setq base (sl:origin bname) tries 0 ok nil)
  (while (and (null ok) (< tries 4))
    (setq nm (sl:newname (1+ tries)))
    (if (entmake (list '(0 . "BLOCK") (cons 2 nm) '(70 . 0) (cons 10 base)))
      (setq ok nm)
      (progn
        (sl:delblock nm)
        (setq tries (1+ tries))
      )
    )
  )
  ok
)

;; 블록 정의를 entmake 로 그대로 복제한다. (ActiveX/COPYOBJECTS 를 쓰지 않는다)
;;   th  = 새 글자로 바꿀 대상 문자의 핸들
;;   new = 새 글자
;;   성공하면 새 블록 이름(문자열), 실패하면 nil
(defun sl:copydef (bname th new / e ed ed2 ok hit nm bad)
  ;;   bad = 복제하지 못한 객체 종류 (실패 원인을 알려 주기 위한 것)
  (setq nm (sl:mkblock bname))
  (cond
    ((null nm)
     (princ "\n[SL-BT] 새 블록을 만들지 못했습니다.")
     nil
    )
    (T
     (setq e (sl:blkfirst bname) ok T hit nil bad nil)
     (while (and ok e (setq ed (entget e)) (/= "ENDBLK" (cdr (assoc 0 ed))))
       (setq ed2 (sl:mkdxf ed))
       (if (and th (equal th (cdr (assoc 5 ed)))
                (member (cdr (assoc 0 ed)) (list "TEXT" "MTEXT"))
           )
         (progn
           (setq ed2 (sl:setstr ed2 new))
           (setq hit T)
         )
       )
       (if (null (entmake ed2))
         (progn
           (setq ok nil)
           (setq bad (cdr (assoc 0 ed)))
         )
       )
       (setq e (entnext e))
     )
     (entmake '((0 . "ENDBLK")))
     (cond
       ((or (null ok) (null (tblobjname "BLOCK" nm)))
        (sl:delblock nm)
        (princ "\n[SL-BT] 블록 내용을 복제하지 못했습니다.")
        (if bad
          (princ (strcat "\n        복제하지 못한 객체: " bad))
        )
        (princ "\n        (3D솔리드·프록시 객체처럼 entmake 로 못 만드는 객체가 들어 있으면 복제할 수 없습니다)")
        nil
       )
       ((null hit)
        (sl:delblock nm)
        (princ "\n[SL-BT] 복제한 블록에서 해당 글자를 찾지 못했습니다.")
        nil
       )
       (T nm)
     )
    )
  )
)

;; 원래 INSERT 의 블록 이름(2)만 새 블록으로 바꾼다.
;;   entmod 로 바꾸므로 레이어·색·축척·회전·핸들·그리기 순서가 모두 그대로 남는다.
;;   성공하면 T, entmod 가 안 먹으면 새로 넣는 방법(sl:swapnew)을 한 번 더 시도한다.
(defun sl:swapins (en newName / ed ed2)
  (setq ed (entget en))
  (cond
    ((or (null ed) (null (assoc -1 ed)) (null (assoc 2 ed)))
     (princ "\n[SL-BT] 블록 참조를 읽지 못했습니다.")
     nil
    )
    (T
     (setq ed2 (subst (cons 2 newName) (assoc 2 ed) ed))
     (cond
       ((and (entmod ed2) (equal newName (cdr (assoc 2 (entget en)))))
        (entupd en)
        T
       )
       (T (sl:swapnew en newName))
     )
    )
  )
)

;; entmod 로 이름을 못 바꿀 때: 같은 자리에 새 INSERT 를 entmake 로 만들고 원래 것은 지운다.
;; (속성 값도 함께 옮긴다)
(defun sl:swapnew (en newName / ed ed2 e a lst ens)
  (setq ed (entget en) lst nil ens nil)
  (cond
    ((null ed)
     (princ "\n[SL-BT] 블록 참조를 읽지 못했습니다.")
     nil
    )
    (T
     (setq e (entnext en))
     (while (and e (setq ed2 (entget e)) (= "ATTRIB" (cdr (assoc 0 ed2))))
       (setq lst (cons ed2 lst))
       (setq ens (cons e ens))
       (setq e (entnext e))
     )
     (setq lst (reverse lst))
     (setq ed2 (sl:mkdxf (subst (cons 2 newName) (assoc 2 ed) ed)))
     (cond
       ((null (entmake ed2))
        (princ "\n[SL-BT] 새 블록 참조를 넣지 못했습니다. (레이어 잠금 확인)")
        nil
       )
       (T
        (foreach a lst
          (if (null (entmake (sl:mkdxf a)))
            (princ "\n[SL-BT] 참고: 속성 일부를 옮기지 못했습니다.")
          )
        )
        (if lst (entmake '((0 . "SEQEND"))))
        (entdel en)
        (foreach a ens
          (if (entget a) (entdel a))     ; 속성이 남아 있으면 같이 지운다
        )
        (if (entget en)
          (princ "\n[SL-BT] 경고: 원래 블록을 지우지 못했습니다. 겹쳐 보이면 원래 것을 지우세요.")
        )
        T
       )
     )
    )
  )
)

;; 블록 정의를 복제하고, 그 사본 안의 글자를 바꾼 뒤, 이 INSERT 만 새 블록으로 교체
;; 성공하면 새 블록 이름(문자열), 실패하면 nil
(defun sl:clone (en bname tex new / th nn)
  (setq th (cdr (assoc 5 (entget tex))))
  (setq nn (sl:copydef bname th new))
  (cond
    ((null nn) nil)
    ((sl:swapins en nn)
     (sl:regen)
     (princ (strcat "\n[SL-BT] 선택한 블록만 새 블록 \"" nn "\" 으로 분리해 수정했습니다."))
     nn
    )
    (T
     (sl:delblock nn)
     nil
    )
  )
)

;;; ---------------------------------------------------------------------
;;;  실제 수정
;;; ---------------------------------------------------------------------

;; 블록 정의의 글자 하나를 바꿈 (모든 참조가 함께 바뀜)
(defun sl:applydef (tex new bname / ok)
  (setq ok (sl:settext tex new))
  (sl:regen)
  (cond
    (ok
     (setq *SL-EDITS* (1+ *SL-EDITS*))
     (princ (strcat "\n[SL-BT] 완료: 블록 \"" bname "\" 안의 글자를 "
                    (sl:show new) " 로 바꿨습니다. (블록은 그대로)")
     )
     T
    )
    (T
     (princ "\n[SL-BT] 글자를 바꾸지 못했습니다. (레이어 잠금/읽기전용 확인)")
     nil
    )
  )
)

;; "선택한 것만" 처리 - 묻지 않고 이 블록 하나만 나눠서 고친다
(defun sl:applyclone (en bname tex new / r atts go)
  (setq go T)
  (setq atts (sl:attrs en))
  (if atts
    (progn
      (princ "\n[SL-BT] 이 블록에는 속성이 있습니다. (속성 값은 그대로 유지됩니다)")
      (initget "Y N")
      (if (= (getkword "\n계속? [예(Y)/아니오(N)] <Y>: ") "N") (setq go nil))
    )
  )
  (cond
    ((null go)
     (princ "\n[SL-BT] 취소했습니다.")
     nil
    )
    (T
     (setq r (sl:clone en bname tex new))
     (cond
       (r
        (setq *SL-EDITS* (1+ *SL-EDITS*))
        T
       )
       (T
        (princ "\n[SL-BT] 이 블록만 분리하지 못했습니다. (다른 블록은 건드리지 않았습니다)")
        (princ "\n        블록 편집기(BEDIT)에서 직접 고쳐 주세요.")
        nil
       )
     )
    )
  )
)

;; 블록 정의 안 글자 수정 (메인 경로)
(defun sl:editdef (en bname pt / texts tex old new n)
  (setq texts (sl:blocktexts bname))
  (cond
    ((null texts) (sl:nohelp en bname pt))
    (T
     (setq tex (sl:choose texts pt en bname))
     (cond
       ((null tex) (princ "\n[SL-BT] 취소했습니다."))
       (T
        (setq old (sl:val tex))
        (princ (strcat "\n[SL-BT] 선택한 글자: " (sl:show old)))
        (setq new (sl:askstr old))
        (cond
          ((null new) (princ "\n[SL-BT] 그대로 두었습니다."))
          ((= new old) (princ "\n[SL-BT] 같은 글자입니다."))
          (T
           (setq n (sl:count bname))
           (cond
             ;; 도면에 하나뿐 -> 그 블록을 직접 고친다
             ((<= n 1) (sl:applydef tex new bname))
             ;; 여러 개 -> 묻지 않고 선택한 그 블록만 자동으로 나눠서 고친다
             (T (sl:applyclone en bname tex new))
           )
          )
        )
       )
     )
    )
  )
)

;; 블록 안에 직접 들어있는 글자가 없을 때 (한 단계 안쪽 블록 확인)
(defun sl:nohelp (en bname pt / nest texts tex old new v i shown n)
  (princ (strcat "\n[SL-BT] 블록 \"" bname "\" 안에 직접 들어있는 글자(TEXT/MTEXT)가 없습니다."))
  (setq nest (sl:nesttexts bname))
  (cond
    ((null nest)
     (princ "\n      (블록 편집기 BEDIT 로 확인해 보세요.)")
    )
    (T
     (setq texts nil)
     (foreach v nest
       (setq texts (cons (car v) texts))
     )
     (setq texts (reverse texts))
     (princ "\n[SL-BT] 안쪽 블록에서 글자를 찾았습니다. (그 블록을 쓰는 모든 곳이 바뀝니다)")
     (setq tex (sl:choose texts pt en bname))
     (cond
       ((null tex) (princ "\n[SL-BT] 취소했습니다."))
       (T
        (setq old (sl:val tex))
        (princ (strcat "\n[SL-BT] 선택한 글자: " (sl:show old)))
        (setq new (sl:askstr old))
        (cond
          ((null new) (princ "\n[SL-BT] 그대로 두었습니다."))
          ((= new old) (princ "\n[SL-BT] 같은 글자입니다."))
          (T
           (initget "Y N")
           (if (= (getkword "\n안쪽 블록이라 모든 곳이 바뀝니다. 계속? [예(Y)/아니오(N)] <N>: ") "Y")
             (sl:applydef tex new bname)
             (princ "\n[SL-BT] 취소했습니다.")
           )
          )
        )
       )
     )
    )
  )
)

;; 속성(ATTRIB) 하나 수정 - 그 블록만 바뀜
(defun sl:editattr (en a / old new ok)
  (setq old (sl:val (car a)))
  (princ (strcat "\n[SL-BT] 속성 값: " (sl:show old)))
  (setq new (sl:askstr old))
  (cond
    ((null new) (princ "\n[SL-BT] 그대로 두었습니다."))
    ((= new old) (princ "\n[SL-BT] 같은 값입니다."))
    (T
     (setq ok (sl:settext (car a) new))
     (cond
       (ok
        (entupd en)
        (sl:regen)
        (setq *SL-EDITS* (1+ *SL-EDITS*))
        (princ "\n[SL-BT] 완료: 이 블록의 속성 값만 바꿨습니다. (다른 블록은 그대로)")
       )
       (T (princ "\n[SL-BT] 속성 값을 바꾸지 못했습니다."))
     )
    )
  )
)

;; 속성이 여러 개인 블록
(defun sl:menuattr (en bname pt atts / n i a k)
  (setq n (length atts))
  (princ (strcat "\n[SL-BT] 이 블록에는 속성이 " (itoa n) "개 있습니다."))
  (setq i 0)
  (foreach a atts
    (setq i (1+ i))
    (princ (strcat "\n   [" (itoa i) "] " (sl:show (cdr a))))
  )
  (princ "\n   [0] 블록 내부 글자(TEXT/MTEXT) 수정")
  (setq k (sl:getnum "\n번호 선택" 1 n))
  (cond
    ((= k 0) (sl:editdef en bname pt))
    (T (sl:editattr en (nth (1- k) atts)))
  )
)

;; 외부참조 여부
(defun sl:xrefp (bname / blk ed f70)
  (setq blk (tblobjname "BLOCK" bname))
  (cond
    ((null blk) nil)
    (T
     (setq ed (entget blk))
     (setq f70 (cdr (assoc 70 ed)))
     (if (or (and f70 (/= 0 (logand 4 f70)))
             (and f70 (/= 0 (logand 8 f70)))
             (wcmatch bname "*|*")
         )
       T
       nil
     )
    )
  )
)

;; 동적 블록 여부 (2006 이상)
(defun sl:dynamic (en / o r)
  (setq o (sl:vcall 'vlax-ename->vla-object (list en)))
  (setq r (if o (sl:vcall 'vla-get-IsDynamicBlock (list o))))
  (if (= r :vlax-true) T nil)
)

;; 블록 참조 클릭
(defun sl:doinsert (en pt / ed bname atts)
  (setq ed (entget en))
  (setq bname (cdr (assoc 2 ed)))
  (cond
    ((sl:xrefp bname)
     (princ (strcat "\n[SL-BT] 외부참조(XREF) 블록은 수정할 수 없습니다: " bname))
    )
    (T
     (if (sl:dynamic en)
       (princ "\n[SL-BT] 참고: 동적 블록입니다. 그립을 움직이면 글자가 되돌아갈 수 있습니다.")
     )
     (setq atts (sl:attrs en))
     (cond
       ((and atts (= (length atts) 1) (null (sl:blocktexts bname)))
        (princ (strcat "\n[SL-BT] 블록 \"" bname "\" 의 속성입니다."))
        (sl:editattr en (car atts))
       )
       (atts (sl:menuattr en bname pt atts))
       (T
        (cond
          ((sl:locked en)
           (princ "\n[SL-BT] 이 블록이 잠긴 레이어에 있습니다. 레이어 잠금을 풀고 하세요.")
          )
          (T (sl:editdef en bname pt))
        )
       )
     )
    )
  )
)

;; 일반 문자(블록 아님) 수정
(defun sl:dotext (en / old new)
  (setq old (sl:val en))
  (princ (strcat "\n[SL-BT] 글자: " (sl:show old) "   (블록이 아닌 일반 문자)"))
  (setq new (sl:askstr old))
  (cond
    ((null new) (princ "\n[SL-BT] 그대로 두었습니다."))
    ((= new old) (princ "\n[SL-BT] 같은 글자입니다."))
    ((sl:settext en new)
     (entupd en)
     (setq *SL-EDITS* (1+ *SL-EDITS*))
     (princ "\n[SL-BT] 완료.")
    )
    (T (princ "\n[SL-BT] 글자를 바꾸지 못했습니다."))
  )
)

;; 클릭한 객체 처리
(defun sl:handle (en pt typ)
  (cond
    ((= typ "INSERT") (sl:doinsert en pt))
    ((member typ (list "TEXT" "MTEXT")) (sl:dotext en))
    ((= typ "ATTRIB") (sl:editattr en (cons en (sl:val en))))
    ((= typ "ATTDEF")
     (princ "\n[SL-BT] 블록 정의 안의 글자입니다. 블록 참조를 클릭해 주세요.")
    )
    (T (princ (strcat "\n[SL-BT] 블록이나 글이 아닙니다: " typ)))
  )
)

;; 한 번 클릭 처리 (계속하려면 T)
(defun sl:step ( / sel en pt typ r em)
  (setq *SL-STOP* nil)
  (setq sel (entsel "\n[SL-BT] 블록(또는 블록 안 글자) 클릭 / 끝내기=Enter: "))
  (cond
    ((null sel) nil)
    (T
     (setq en (car sel) pt (cadr sel) typ (cdr (assoc 0 (entget en))))
     (cond
       ((member (type vl-catch-all-apply) (list 'SUBR 'USUBR 'EXRXSUBR))
        (setq r (vl-catch-all-apply 'sl:handle (list en pt typ)))
        (if (vl-catch-all-error-p r)
          (progn
            (setq em (strcase (vl-catch-all-error-message r)))
            (cond
              ((wcmatch em "*CANCEL*,*QUIT*,*EXIT*,*BREAK*")
               (setq *SL-STOP* T)
              )
              (T (princ (strcat "\n[SL-BT] 오류: " (vl-catch-all-error-message r))))
            )
          )
        )
       )
       (T (sl:handle en pt typ))
     )
     (not *SL-STOP*)
    )
  )
)

;;; ---------------------------------------------------------------------
;;;  오류 처리 / 시작
;;; ---------------------------------------------------------------------

(defun sl:cleanup ()
  (if *SL-UNDO*
    (progn
      (setq *SL-UNDO* nil)
      (command "_.UNDO" "_E")
    )
  )
  (if *SL-CMDECHO* (setvar "CMDECHO" *SL-CMDECHO*))
  (setq *SL-CMDECHO* nil)
  (princ)
)

(defun sl:error (msg)
  (cond
    ((and msg (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*,*BREAK*"))
     (princ "\n[SL-BT] 중단했습니다.")
    )
    (T (princ (strcat "\n[SL-BT] 오류: " (if msg msg "unknown"))))
  )
  (sl:cleanup)
  (princ)
)

(defun sl:main ()
  (setq *SL-EDITS* 0)
  (setq *SL-CMDECHO* (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (command "_.UNDO" "_BE")
  (setq *SL-UNDO* T)
  (princ "\n[SL-BT] 블록 안의 글자 수정 - 블록을 깨지 않습니다.")
  (princ "\n        블록을 클릭하고 새 글자를 입력하세요. 끝내기 = Enter / ESC")
  (while (sl:step))
  (sl:cleanup)
  (princ (strcat "\n[SL-BT] 끝. 이번에 수정한 글자: " (itoa *SL-EDITS*) "개"))
  (princ)
)

(defun c:SL-BT ( / *error* olderr)
  (setq olderr *error*
        *error* sl:error
  )
  (sl:main)
  (setq *error* olderr)
  (princ)
)

(princ "\n[SL-BT] 로드 완료.  명령어: SL-BT")
(princ)
