;;; cadlaw.el --- Parse the Tobacco Product Regulations  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Alexander Griffith
;; Author: Alexander Griffith <griffitaj@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.3"))
;; Homepage: nil

;; This file is not part of GNU Emacs.

;; This file is part of cadlaw.el.

;; cadlaw.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; cadlaw.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mastodon.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library will help to format sections of canadian regulations
;; By default it is setup to parse the Tobacco Product Regulations

;; (cadlaw-dl-ess-raw) ;; download regs
;; (insert (cadlaw-apply-render (cadtrp-ess-get-section* "89") nil))

;; TODO
;; - [ ] Test this out with other regulations
;; - [ ] Replace [ammended] text with actual ammending text
;; - [ ] Write alternate layout formaters, the current formater only
;;       really works well for standard formating
;; - [ ] Develop a system for interoping with notes

;;; Code:

(defgroup cadlaw nil
  "External functions to parse the Tobacco Product Regulations"
  :prefix "cadlaw-"
  :group 'external)

(defvar cadlaw-law-plist
  '(tpr-psa "SOR-2019-107.xml" excise-act "E-14.1.xml")
  "Holds links to the regs. See also` cadlaw-endpoint-prefix'.")

(defcustom cadlaw-endpoint-prefix "https://laws-lois.justice.gc.ca/eng/XML/"
  "Prefix for the source for the law."
  :group 'cadlaw
  :type 'string)

(defcustom cadlaw-xml-buffer-prefix " *CADLAW XML Buffer - "
  "Prefix of buffer where the laws are stored."
  :group 'cadlaw
  :type 'string)

(defvar cadlaw-ess-raw-alist '()
  "Variable where the Regulations are stored as ess. 
Uses the same names as those defined in `cadlaw-law-plist'.")

(defun cadlaw--make-endpoint (law)
  "Make endpoint from LAW index in `cadlaw-law-plist'."
  (concat cadlaw-endpoint-prefix (plist-get cadlaw-law-plist law)))

(defun cadlaw--make-buffer-name (law)
  "Make buffer name from LAW index in `cadlaw-law-plist'."
  (concat cadlaw-xml-buffer-prefix (plist-get cadlaw-law-plist law)))

(defun cadlaw--retrieve-callback (_status law)
  "Parse the Regulations.
Parses XML to ESS and stores the result in `cadlaw-ess-raw-plist'
_STATUS is unused
LAW key in `cadlaw-ess-raw-plist'"
  (goto-char (point-min))
  (re-search-forward "^$")
  (let ((str (buffer-substring-no-properties (point) (point-max)))
        (new-buffer (cadlaw--make-buffer-name law)))
    (kill-buffer)
    (with-current-buffer (get-buffer-create new-buffer)
      (erase-buffer)
      (insert str)
      (message (concat "Copied XML data to " new-buffer))
      (add-to-list 'cadlaw-ess-raw-alist `(,law . ,(xml-parse-region))))))

(defun cadlaw--dl-ess-raw (law)
  "Download the Regulations.
LAW is a key in the `cadlaw-law-plist'"
  (url-retrieve (cadlaw--make-endpoint law) #'cadlaw--retrieve-callback (list law)))

(defun cadlaw--dl-ess-raw-sync (law)
  "Download the Regulations synchronously.
LAW is a key in the `cadlaw-law-plist'"
  (let* ((endpoint (cadlaw--make-endpoint law))
         (buffer (url-retrieve-synchronously endpoint)))
    (with-current-buffer buffer
      (cadlaw--retrieve-callback 200 law))))

(defun cadlaw--get-keys (plist)
  "Return the keys of PLIST."
  (let ((return nil)
        (first (pop plist))
        (second (pop plist)))
    (while (and first second)
      (push first return)
      (setq first (pop plist))
      (setq second (pop plist)))
    (reverse return)))

(defun cadlaw-init (&rest endpoints)
  "Download all law in `cadlaw-law-plist'.
ENDPOINTS are a cons (key, law string) that is added to
`cadlaw-law-plist before downloading.'"
  (mapc (lambda (endpoint) (plist-put cadlaw-law-plist (car endpoint) (cdr endpoint))) endpoints)
  (let ((laws (cadlaw--get-keys cadlaw-law-plist)))
    (mapc (lambda (law) (cadlaw--dl-ess-raw law)) laws)))

(defun cadlaw--ess-get-law (law)
  "Get the law form the `cadlaw-ess-raw-alist'.
If there is no law it will check `cadlaw-law-alist'.
LAW is a key in the `cadlaw-law-plist'"
  (cond
   ((member law (mapcar #'car cadlaw-ess-raw-alist))
    (cdr (assoc law cadlaw-ess-raw-alist)))
   ((plist-member cadlaw-law-plist law)
    (progn
      (message (format "Downloading %s from %s." law (cadlaw--make-endpoint law)))
      (cadlaw--dl-ess-raw-sync law)
      (cdr (assoc law cadlaw-ess-raw))))
   (t (error (format "%s not in `cadlaw-ess-raw-alist' or `cadlaw-law-plist'" law)))))

(defun cadlaw--ess-get-body (law)
  "Get the body, either saved as `cadlaw-ess-raw' or from `cadlaw-endpoint'."
  (assert (equalp (not cadlaw-ess-raw) nil) t "XML has not finished downloading")
  ;; (when (not cadlaw-ess-raw))
  (cdr (assoc 'Body (cdar (cadlaw--ess-get-law law)))))

(defun cadlaw--list-to-string (list)
  "Convert a LIST to string.
E.g. ((sample \"A\") \" B\") -> \"A B\""
  (cond ((equal (type-of list) 'string) list)
        ((equal (type-of list) 'cons)
         (mapconcat (lambda(x)
                      (if (equalp (type-of x) 'cons) (caddr x) x))
                    list
                    ""))))

(defun cadtrp-ess-get-section (law section-label &optional searchable return-heading?)
  "Get a single section from the Regulations.
For the nested equivielent (i.e. normal use case) see `cadlaw-ess-get-section*'.
SECTION-LABEL the name of the section as a string
SEARCHABLE the ess to be searched, if not included it defaults to a call to
`cadlaw--ess-get-body'
RETURN-HEADING bool, if true return the related headings rather than the section"
  (let* ((body (or searchable (cddr (cadlaw--ess-get-body law))))
         (section (pop body))
         (heading nil)
         (ret nil))
    (while section
      (let* ((relevant (cddr section))
             (type (car section))
             (marginal-note (caddr (assoc 'MarginalNote relevant)))
             (label (caddr (assoc 'Label relevant)))
             (text (caddr (assoc 'Text relevant))))
        (if (equalp type 'Heading)
            (push section heading) ;; need to expand this stub!!
                                   ;; should return just relevant headings
          (progn
            (when (equalp label section-label)
              (setq ret section)))))
      (setq section (pop body)))
    (if return-heading?
        heading
        ret)))

(defun cadtrp-ess-get-section* (law &rest sections)
  "Search through Regulations to find SECTIONS.
SECTIONS are strings."
  (let ((searchable (cadlaw--ess-get-body law))
        (section (pop sections)))
    (while section
      (setq searchable (cadtrp-ess-get-section law section (cddr searchable)))
      (setq section (pop sections)))
    searchable))

(defun cadlaw-render-text (type text)
  "Render raw text.
TYPE symbol for type of text, 'text 'marginal-note 'label 'children
TEXT string to format"
  (when text
    text))

(defun cadlaw-render-emacs (type text)
  "Render for Emacs.
TYPE symbol for type of text, 'text 'marginal-note 'label 'children
TEXT string to format"
  (when text
    (pcase type
    ('text text)
    ('marginal-note (propertize text 'face 'bold))
    ('label (propertize text 'face 'bold))
    ('children text)
    (_ text))))

(defun cadlaw-render-org (type text)
  "Render for org mode.
TYPE symbol for type of text, 'text 'marginal-note 'label 'children
TEXT string to format"
  (when text
    (pcase type
    ('text text)
    ('marginal-note (concat "** " text))
    ('label (concat "*" text "*"))
    ('children text)
    (_ text))))

(defvar cadlaw-render-sub-format-plist '(t cadlaw-render-text
                                           emacs cadlaw-render-emacs
                                           org cadlaw-render-org)
  "List of callbacks with instructions on how individual elements of a Section
should be written.")

(defvar cadlaw-render-format-plist '(t cadlaw--render-helper-default
                                       emacs cadlaw--render-helper-default
                                       org cadlaw--render-helper-default)
    "List of callbacks with instructions on how  elements of a Section should
be layed out.")

(defun cadlaw--render-sub-formater (format type text)
  "Call the appropriate render sub element level formater.
FORMAT formater to use, see `cadlaw-render-sub-format-plist'
TYPE type of element 'text 'marginal-note 'label 'children
TEXT string to format"
  (funcall (plist-get cadlaw-render-sub-format-plist (or format 't)) type text))

(defun cadlaw--render-helper
    (format marginal-note text children label prefix &optional debug?)
  "Call the appropriate render high level formater that defines layout.
default 't calls `cadlaw--render-helper-default', which renders the xml
as it is layed out on the justice.gc.ca website
https://laws-lois.justice.gc.ca/
FORMAT symbol denoting the format to use, default options
include Emacs org or t (for raw text)
MARGINAL-NOTE the header that goes above the section
TEXT text of element
CHILDREN child elements
LABEL the curent element's label
PREFIX characters to appear before the main string
DEBUG?? bool, if true include the debug string with every sub element"
  (funcall (plist-get cadlaw-render-format-plist (or format 't))
           format marginal-note text children label prefix debug?))

(defun cadlaw--render-helper-default
    (format marginal-note text children label prefix debug?)
  "Render the items in the default order.
I.e. make it look like whats on the Regulation website.
FORMAT symbol denoting the format to use, default options
include Emacs org or t (for raw text)
MARGINAL-NOTE the header that goes above the section
TEXT text of element
CHILDREN child elements
LABEL the curent element's label
PREFIX characters to appear before the main string
DEBUG?? bool, if true include the debug string with every sub element"
  (let ((marginal-note (cadlaw--render-sub-formater format 'marginal-note marginal-note))
        (text (cadlaw--render-sub-formater format 'text text))
        (label (cadlaw--render-sub-formater format 'label label))
        (children (cadlaw--render-sub-formater format 'children children))
        (prefix (cadlaw--render-sub-formater format 'prefix prefix)))
   (cond ((and marginal-note text children label)
         (concat (when debug? (format "//%s 111  - //" debug?))
                 prefix marginal-note "\n" label " " text "\n" children))
        ((and (not marginal-note) text children label)
         (concat (when debug? "//011  - //")
                 prefix label " " text "\n" children))
        ((and marginal-note (not text) children label)
         (concat (when debug? "//101  - //")
                 prefix marginal-note "\n" label children))
        ((and (not marginal-note) (not text) children label)
         (concat (when debug? "//001  - //")
                 prefix label children))
        ((and marginal-note text (not children) label)
         (concat (when debug? "//110  - //")
                 prefix marginal-note "\n" label " " text))
        ((and (not marginal-note) text (not children) label)
         (concat (when debug? "//010  - //")
                 prefix label " " text))
        ((and marginal-note (not text) (not children) label)
         (concat (when debug? "//100  - //")
                 prefix label " " marginal-note ))
        ((and (not marginal-note) (not text) (not children) label)
         (concat (when debug? "//000  - //")
                 prefix label)))))

(defun cadlaw--render-section (element prefix format)
  "Render a section of a Regulation.
ELEMENT section to render.
PREFIX text to appear before rendering.
FORMAT format to use, see `cadlaw-render-format-plist'"
  (assert (equal 'Section (car element)))
  (let* ((text (cadlaw--list-to-string (cddr (assoc 'Text (cddr element)))))
        (label (cadlaw--list-to-string (cddr (assoc 'Label (cddr element)))))
         (children (remove-if-not (lambda(x) (member (car x)
                                                '(Subsection Paragraph Definition)))
                                  (cddr element)))
         (processed (mapconcat
                     (lambda(x)
                       (cadlaw-apply-render
                        x
                        (when (member (car children)
                                      '(Definition Paragraph)) "  ")
                        format))
                     children "\n"))
        (marginal-note (cadlaw--list-to-string (cddr (assoc 'MarginalNote (cddr element))))))
    (cadlaw--render-helper format marginal-note text (when (> (length processed) 0) processed)
                          label prefix)))

(defun cadlaw--render-subsection (element prefix format)
    "Render a subsection of a Regulation.
ELEMENT section to render.
PREFIX text to appear before rendering.
FORMAT format to use, see `cadlaw-render-format-plist'"
  (assert (equal 'Subsection (car element)))
  (let* ((text (cadlaw--list-to-string (cddr (assoc 'Text (cddr element)))))
         (label (cadlaw--list-to-string (cddr (assoc 'Label (cddr element)))))
         (children (remove-if-not (lambda(x) (member (car x) '(Definition Paragraph))) (cddr element)))
         (processed (mapconcat (lambda(x) (cadlaw-apply-render x "  " format)) children "\n"))
         (marginal-note (cadlaw--list-to-string (cddr (assoc 'MarginalNote (cddr element))))))
    (cadlaw--render-helper format marginal-note text (when (> (length processed) 0) processed)
                          label prefix)))

(defun cadlaw--render-paragraph (element prefix format)
    "Render a paragraph of a Regulation.
ELEMENT section to render.
PREFIX text to appear before rendering.
FORMAT format to use, see `cadlaw-render-format-plist'"
  
  (assert (equal 'Paragraph (car element)))
  (let ((text (cadlaw--list-to-string (cddr (assoc 'Text (cddr element)))))
        (label (cadlaw--list-to-string (cddr (assoc 'Label (cddr element))))))
    ;;(concat prefix label " " text)
    (cadlaw--render-helper format nil text nil label prefix)))

(defun cadlaw--render-definition (element prefix format)
    "Render a subsection of a Regulation.
ELEMENT section to render.
PREFIX text to appear before rendering.
FORMAT format to use, see `cadlaw-render-format-plist'"
  
  (assert (equal 'Definition (car element)))
  (let ((text (cadlaw--list-to-string (cddr (assoc 'Text (cddr element)))))
        (label (cadlaw--list-to-string (cddr (assoc 'Label (cddr element))))))
    ;;(concat prefix text) ;; ERROR will not print without lael
    (cadlaw--render-helper format nil text nil (or label "") prefix)))

;; Stub -- to expand
(defun cadlaw--render-heading (element prefix)
    "Render heading of a Regulation.
ELEMENT section to render.
PREFIX text to appear before rendering.
FORMAT format to use, see `cadlaw-render-format-plist'"
    t)

(defun cadlaw-apply-render (element prefix format)
  "Render an element of a Regulation.
ELEMENT section to render.
PREFIX text to appear before rendering.
FORMAT format to use, see `cadlaw-render-format-plist'"
  (let ((type (car element)))
    (cond ((equalp type 'Paragraph) (cadlaw--render-paragraph element prefix format))
          ((equalp type 'Subsection) (cadlaw--render-subsection element prefix format))
          ((equalp type 'Section) (cadlaw--render-section element prefix format))
          ((equalp type 'Definition) (cadlaw--render-definition element prefix format)))))

(defun cadlaw-insert-section (law section)
  "Interactive function for inserting LAW SECTION into current emacs buffer at point."
  (interactive "sLaw: \nsSection: ")
  (insert (cadlaw-apply-render (cadtrp-ess-get-section* (intern-soft law) section) "" emacs)))

(provide 'cadlaw)
;;; cadlaw.el ends here
