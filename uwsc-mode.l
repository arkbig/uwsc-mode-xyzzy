;キーワードファイルuwscをetcフォルダにコピー
;
;くにさん追加と書いたところは
;くに(http://hc4.seikyou.ne.jp/home/koukuni/)さんが追加してくれたところです

;おまじない
(provide "uwsc-mode")
(in-package "editor")

(export '(uwsc-mode uwsc-indent-line uwsc-newline-and-indent))

; uwsc の場所 くにさん追加
(defvar *uwsc-command* "D:/util/operate/uwsc/UWSC.exe")
(defvar *uwsc-help* "D:/util/operate/uwsc/uwsc.chm")

;フック
(defvar *uwsc-mode-hook* nil)

;キーワード
(defvar *uwsc-keyword-hash-table* nil)
(defvar *uwsc-keyword-file* "uwsc")

;シンタックステーブル
(defvar *uwsc-mode-syntax-table* nil)
(unless *uwsc-mode-syntax-table*
  (setq *uwsc-mode-syntax-table* (make-syntax-table))
  (set-syntax-symbol *uwsc-mode-syntax-table* #\_)
  (set-syntax-match *uwsc-mode-syntax-table* #\( #\))
  (set-syntax-match *uwsc-mode-syntax-table* #\{ #\})
  (set-syntax-match *uwsc-mode-syntax-table* #\[ #\])
  (set-syntax-start-c++-comment *uwsc-mode-syntax-table* #\/)
  (set-syntax-end-c++-comment *uwsc-mode-syntax-table* #\LFD)
  (set-syntax-string *uwsc-mode-syntax-table* #\")
  )

;正規表現シンタックス
;１個づつ指定が面倒なので正規表現で
(defvar *uwsc-variables-format*
  (compile-regexp-keyword-list
   '(
     ("CLK_[a-zA-Z0-9_]*" t (:keyword 1))
     ("STR_[a-zA-Z0-9_]*" t (:keyword 1))
     ("BTN_[a-zA-Z0-9_]*" t (:keyword 1))
     ("SLCT_[a-zA-Z0-9_]*" t (:keyword 1))
     ("ITM_[a-zA-Z0-9_]*" t (:keyword 1))
     ("ST_[a-zA-Z0-9_]*" t (:keyword 1))
     ("CUR_[a-zA-Z0-9_]*" t (:keyword 1))
     ("GET_[a-zA-Z0-9_]*" t (:keyword 1))
     ("SC_[a-zA-Z0-9_]*" t (:keyword 1))
     ("FOM_[a-zA-Z0-9_]*" t (:keyword 1))
     ("G_SCREEN_[a-zA-Z0-9_]*" t (:keyword 1))
     ("G_TIME_[a-zA-Z0-9_]*" t (:keyword 1))
     ("TEXTBLOCK[^.]+ENDTEXTBLOCK" t (:keyword :string))
     )))

;キー割り当て
(defvar *uwsc-mode-map* nil)
(unless *uwsc-mode-map*
  (setq *uwsc-mode-map* (make-sparse-keymap))
  (define-key *uwsc-mode-map* #\TAB 'uwsc-indent-line)
  (define-key *uwsc-mode-map* #\C-M-q 'indent-sexp)
  (define-key *uwsc-mode-map* #\RET 'uwsc-newline-and-indent)
  (define-key *uwsc-mode-map* '(#\C-c #\C-e) 'uwsc-exec) ;くにさん追加
  (define-key *uwsc-mode-map* #\F1 'show-uwsc-help)) ;くにさん追加


;関数
(defun uwsc-build-summary-of-functions ()
  (let ((result nil))
    (save-excursion
      (goto-char (point-min))
      (while (scan-buffer "^[ \t]*\\(function\\|procedure\\)[ \t]+\\([^ \t\n{(]+\\)"
                          :regexp t :tail t)
        (push (list (current-line-number) (match-string 2)) result)))
    (nreverse result)))


;くにさん追加
(defun uwsc-exec () ; コマンドの実行
  (interactive)
  (let*
      ((uwsc-file (get-buffer-file-name)))
    (call-process (concat *uwsc-command* " " uwsc-file))))
;くにさん追加
(defun show-uwsc-help () ;ヘルプの表示
  (interactive)
  (call-process (concat "C:/WINDOWS/hh.exe" " "  *uwsc-help*)))


;インデント
(defvar *uwsc-indent-tabs-mode* t)
(defun uwsc-indent-line ()
  (interactive "*")
  (case (save-excursion
          (goto-bol)
          (parse-point-syntax))
    (:string)
    (:comment)
    (t
     (let ((column (calc-uwsc-indent)) (col (point)))
       (goto-bol)
       (skip-chars-forward " \t")
       (setq col (- col (point)))
       (when (integerp column)
         (smart-indentation column)
         (if (> col 0) (goto-char (+ (point) col))))
       ))
    (insert "\t"))
  t)

(defun uwsc-newline-and-indent (&optional (arg 1))
  (interactive "*p")
  (delete-trailing-spaces)
  (insert #\LFD arg)
  (uwsc-indent-line))

(defun calc-uwsc-indent ()
  (let (column (line (current-line-number)))
    (save-excursion
      (unless (= line 1)
        (goto-bol)
        (skip-chars-forward " \t")
        (setq column (uwsc-count-blk-end))
        (goto-line line)
        ;文字列、コメントを飛ばす
        (loop (if (= (current-line-number) 1) (return-from calc-uwsc-indent column))
          (goto-bol)
          (skip-chars-backward " \t\n")
          (goto-bol)
          (skip-chars-forward " \t")
          (case (save-excursion (goto-bol) (parse-point-syntax))
            (:string)
            (:comment)
            (t (return))))
        (goto-bol)
        (skip-chars-forward "\t")
        (+ column (uwsc-count-blk-begin (current-column)))))))

(defun uwsc-count-blk-end ()
  (let ((column 0) (col (point)) (eolp))
    (goto-eol)
    (setq eolp (point))
    (goto-char col)
    (cond ((looking-at "[eE][lL][sS][eE][iI][fF][ \n(]") ;ELSEIF
           (setq column (- column *tab-columns*)))
          ((looking-at "[eE][lL][sS][eE][ \n]") ; ELSE
           (setq column (- column *tab-columns*)))
          ((looking-at "[eE][nN][dD][iI][fF][ \n]") ;ENDIF
           (setq column (- column *tab-columns*)))
          ((looking-at "[nN][eE][xX][tT][ \n]") ;NEXT
           (setq column (- column *tab-columns*)))
          ((looking-at "[wW][eE][nN][dD][ \n]") ;WEND
           (setq column (- column *tab-columns*)))
          ((looking-at "[uU][nN][tT][iI][lL][ \n(]") ;UNTIL
           (setq column (- column *tab-columns*)))
          ((looking-at "[cC][aA][sS][eE][ \n(]") ;CASE
           (setq column (- column *tab-columns*)))
          ((looking-at "[dD][eE][fF][aA][uU][lL][tT][ \n]") ;DEFAULT
           (setq column (- column *tab-columns*)))
          ((looking-at "[sS][eE][lL][eE][nN][dD][ \n]") ;SELEND
           (setq column (- column *tab-columns*)))
          ((looking-at "[fF][eE][nN][dD][ \n]") ;DEFAULT
           (setq column (- column *tab-columns*)))
          ((looking-at "[eE][nN][dD][wW][iI][tT][hH][ \n]") ;ENDWITH
           (setq column (- column *tab-columns*)))
          ((looking-at "[eE][nN][dD][cC][lL][aA][sS][sS][ \n]") ;ENDCLASS
           (setq column (- column *tab-columns*)))
          )
    column))

(defun uwsc-count-blk-begin (column)
  (let ((col (point)) (eolp))
    (goto-eol)
    (setq eolp (point))
    (goto-char col)
    (skip-chars-forward " ")
    (cond ((looking-at "[iI][fF][bB][ \n(]") ;IFB
           (setq column (+ column *tab-columns*)))
          ((looking-at "[eE][lL][sS][eE][iI][fF][ \n(]") ;ELSEIF
           (setq column (+ column *tab-columns*)))
          ((looking-at "[eE][lL][sS][eE][ \n]") ;ELSE
           (setq column (+ column *tab-columns*)))
          ((looking-at "[fF][oO][rR][ \n(]") ;FOR
           (setq column (+ column *tab-columns*)))
          ((looking-at "[wW][hH][iL][lL][eE][ \n(]") ;WHILE
           (setq column (+ column *tab-columns*)))
          ((looking-at "[rR][eE][pP][eE][aA][tT][ \n]") ;REPEAT
           (setq column (+ column *tab-columns*)))
          ((looking-at "[sS][eE][lL][eE][cC][tT][ \n(]") ;SELECT
           (setq column (+ column *tab-columns*)))
          ((looking-at "[cC][aA][sS][eE][ \n(]") ;CASE
           (setq column (+ column *tab-columns*)))
          ((looking-at "[dD][eE][fF][aA][uU][lL][tT][ \n]") ;DEFAULT
           (setq column (+ column *tab-columns*)))
          ((looking-at "[pP][rR][oO][cC][eE][dD][uU][rR][eE][ \n]") ;PROCEDURE
           (setq column (+ column *tab-columns*)))
          ((looking-at "[fF][uU][nN][cC][tT][iI][oO][nN][ \n]") ;FUNCTION
           (setq column (+ column *tab-columns*)))
          ((looking-at "[wW][iI][tT][hH][ \n]") ;WITH
           (setq column (+ column *tab-columns*)))
          ((looking-at "[cC][lL][aA][sS][sS][ \n]") ;CLASS
           (setq column (+ column *tab-columns*)))
          )
    column))


(defun uwsc-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq buffer-mode 'uwsc-mode)
  (setq mode-name "uwsc")
  (use-keymap *uwsc-mode-map*)
  (use-syntax-table *uwsc-mode-syntax-table*)
  (make-local-variable 'mode-specific-indent-command)
  (setq mode-specific-indent-command 'uwsc-indent-line)
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode *uwsc-indent-tabs-mode*)
  ;キーワードのロード
  (and *uwsc-keyword-file*
       (null *uwsc-keyword-hash-table*)
       (setq *uwsc-keyword-hash-table*
             (load-keyword-file *uwsc-keyword-file* t)))
  (when *uwsc-keyword-hash-table*
    (make-local-variable 'keyword-hash-table)
    (setq keyword-hash-table *uwsc-keyword-hash-table*))
  ;正規表現キーワード
  (make-local-variable 'regexp-keyword-list)
  (setq regexp-keyword-list *uwsc-variables-format*)
  ;関数
  (make-local-variable 'build-summary-function)
  (setq build-summary-function 'uwsc-build-summary-of-functions)
  ;フックの実行
  (run-hooks '*uwsc-mode-hook*))
