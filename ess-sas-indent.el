;;; ess-sas-indent.el --- Provides alternative SAS indention
;; 
;; Filename: ess-sas-indent.el
;; Description: ess
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Wed Nov 20 11:11:35 2013 (-0600)
;; Version: 
;; Package-Requires: ()
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:
(require 'ess)
(require 'ess-sas-d)

(defvar ess-sas-tab-width 2)
(defvar ess-sas-tab-if-else-width 2)
(defvar ess-sas-indent-end-to-match-do nil)

(defun ess-sas-is-between (first last &optional eof)
  "Returns true of the carat is between the first and last .

If eof is true, then the last position is:
   (1) the position of the variable last
   (2) the position of the next variable first
   (3) the end of the buffer.
If eof is nil, then the last position is:
   (1) the position of the next variable last.
"
  (let (first-posB last-posF (between t))
    (save-excursion
      (if (re-search-backward first nil t)
          (setq first-posB (point))
        (setq between nil)))
    (if between
        (save-excursion
          (if (re-search-forward last nil t)
              (setq last-posF (point))
            ;; 
            ;; Last not found, look for first.
            ;;
            (if eof
                (progn
                  (if (re-search-forward first nil t)
                      (setq last-posF (point))
                    ;;
                    ;; First not found, set to end of buffer.
                    ;;
                    (setq last-posF (point-max))))
              ;;
              ;; Eof not true.  
              ;;
              (setq between nil)))))
    (if (and (not eof) between)
        (save-excursion
          (if (re-search-forward first nil t)
              (if (< (point) last-posF)
                  (setq between nil)))))
    (if between
        (save-excursion
          (if (re-search-backward last nil t)
              (if (> (point) first-posB)
                  (setq between nil)))))
    between))

(defun ess-sas-bo-sas ()
  "Move point to beginning of current sas statement. (kind of)"
  (interactive)
  (let ((pos (point))
	(cont 't))
    (if (search-forward ";" nil 1) 
	(forward-char -1))
    (re-search-backward ";" nil t)
    ;; Break out of comments
    ;; (while (ess-sas-is-between "/\\*" "\\*/")
    ;;   (re-search-backward ";" nil t))
    (skip-chars-forward "\t\n\f; ")
    (while (and cont (looking-at "/\\*"))
      (if (not (re-search-forward "\\*/" nil t))
	  (setq cont nil)
	(skip-chars-forward "\t\n\f ")))
    (if (> pos  (point)) nil
      (goto-char pos)
      (if (not (re-search-backward "[;\n]" nil t)) nil
	(forward-char 1)
	(skip-chars-forward "\t ;")))))

;; SASmod indentation function
(defun ess-sas-detab (arg)
  "Removes the tabs and places spaces in the string passed to the function."
  (let ((tmp arg))
    (while (string-match "\t" tmp)
      (setq tmp (replace-match (make-string default-tab-width ? ) nil nil tmp)))
    (symbol-value 'tmp)))

(defun ess-sas-indent ()
  "Indentation function for SASmod"
  (interactive)
  (let* ((case-fold-search 't)
	 (p nil)
	 (bol nil)
	 (bosas nil)
	 (bosasl nil)
	 (tmp nil)
	 (boi nil) ;; Beginning of indent
	 (sasindent '("proc"
		      "data"
		      "do"
		      "%macro"
		      "%do"))
	 (sasdeindent '( "run"
			"end"
			;;		       "quit"
			"%mend"
			"%end"))
	 (sasstop (regexp-opt (append sasindent
				      sasdeindent)))
	 (sasi (regexp-opt sasindent))
	 (sasd (regexp-opt sasdeindent))
	 (indent-length-a 0)
	 (indent-length-b 0)
	 (in-statement nil)
	 (last-end nil)
	 (di nil)
	 (empty nil)
	 (cont 't)
	 (cur ""))
    (save-excursion
      (end-of-line)
      (skip-chars-backward " \t;")
      (setq p (point))
      (beginning-of-line)
      (if (not (looking-at (format "^[ \t]*\\<\\(%s\\)\\>" sasd))) nil
	(if (string= (match-string 1) "end")
	    (setq last-end 't))
	(setq di 't))
      (setq bol (point))
      (goto-char p)
      (ess-sas-bo-sas)
      (setq bosas (point))
      (beginning-of-line)
      (setq bosasl (point))
      (if (not (looking-at "^\\([ \t]*\\)\\(\\(?:proc[ \t]+\\)?%?[a-z]+\\)")) nil
	(setq tmp (match-string 0))
	(if (match-string 1)
	    (setq indent-length-a (length (ess-sas-detab (match-string 1))))
	  (setq indent-length-a 0))
	(setq indent-length-b (+ 1 (length (ess-sas-detab tmp))))
	(if (or (string= (match-string 2) "else") (string= (match-string 2) "if"))
	    (setq indent-length-b (+ ess-sas-tab-if-else-width indent-length-a))))
      (setq in-statement (string-match "[^ \t\n\f]" (buffer-substring bosas p)))
      (setq indent-length-a nil)
      (if (not (re-search-backward (format "\\(?:\\(?:;\\|\\*/\\|\\`\\)[\t\n\f ]*\\<\\(?:%s\\)\\>\\|\\<do\\>\\)" sasstop) nil t)) nil 
	(setq tmp (match-string 0))
	(while (and 
		(string= "do" tmp)
		(memq (get-text-property (point) 'face) '(font-lock-comment-face font-lock-string-face)))
	  (if (re-search-backward (format "\\(?:\\(?:;\\|\\*/\\|\\`\\)[\t\n\f ]*\\<\\(?:%s\\)\\>\\|\\<do\\>\\)" sasstop) nil t)
	      (setq tmp (match-string 0))
            (goto-char (bobp))))
	;; Ok found stop word.
	(if (looking-at "\\*/")
	    (forward-char 2))
	(if (string= tmp "do")
	    (progn
	      (ess-sas-bo-sas)
	      (beginning-of-line)
	      (if (not (looking-at "^[ \t]*")) nil
		(setq indent-length-a (+ (length (ess-sas-detab (match-string 0))) ess-sas-tab-width))))
	  (skip-chars-forward "\t\n\f ;")
	  (if (not (looking-at (format "\\<\\(?:%s\\)\\>" sasi))) 
	      (progn
		(beginning-of-line)
		(if (looking-at "^\\([ \t]*\\)") 
		    (setq indent-length-a (length (ess-sas-detab (match-string 0)))))
		(if (and ess-sas-indent-end-to-match-do (looking-at "^[ \t]*\\<end\\>"))
		    (setq indent-length-a (- indent-length-a ess-sas-tab-if-else-width)))
		;; Now fix data and proc do deindent if not already deindented on the last line.
		(save-excursion
		  (goto-char p)
		  (ess-sas-bo-sas)
		  (if (looking-at "\\(data\\|proc\\)")
		      (progn
			(skip-chars-backward "\t\n\f; ")
			(ess-sas-bo-sas)
			(if (looking-at "\\<\\(run\\|quit\\)\\>") nil
			  (setq indent-length-a (- indent-length-a ess-sas-tab-width)))))))
	    (beginning-of-line)
	    (setq boi (point))
	    (if (= boi bol) nil
	      ;; Ok, add a tab if not looking at a stop word.
	      (if (not (looking-at "^\\([ \t]*\\)")) nil
		(setq indent-length-a (+ (length (ess-sas-detab (match-string 1))) ess-sas-tab-width))
		(if (looking-at (format "^[ \t]*\\<\\(?:%s\\)\\>" sasd))
		    (progn
		      (setq indent-length-a (- indent-length-a ess-sas-tab-width))))))))))
    (cond
     ((and di indent-length-a)
      (setq indent-length-a (- indent-length-a ess-sas-tab-width)))
     ((and ess-sas-indent-end-to-match-do (and di last-end))
      (setq indent-length-a (+ indent-length-a ess-sas-tab-if-else-width))))
    (if (or (not in-statement) (= bosasl bol))
        ;; Hanging indent
        (if indent-length-a
            (ess-sas-indent-line-to indent-length-a))
      (ess-sas-indent-line-to indent-length-b))
    ))

(defun ess-sas-indent-line-to (arg)
  "Indents a line to the length using spaces only (sometimes tabs cause problems..."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^[ \t]*")
	(replace-match ""))
    (if (> arg 0)
	(insert (make-string arg ? ))))
  (skip-chars-forward " "))

(defalias 'sas-indent-line 'ess-sas-indent)

(provide 'ess-sas-indent)

(define-key sas-mode-local-map "\t" 'sas-indent-line)

(add-hook 'SAS-mode-hook
          #'(lambda()
              (set (make-local-variable 'indent-line-function) 'sas-indent-line)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ess-sas-indent.el ends here
