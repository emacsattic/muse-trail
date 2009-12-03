;;; muse-trail -- trails for muse projects. 

;; $Rev: 754 $
;; $Date: 2008-02-15 16:48:33 +0000 (Fri, 15 Feb 2008) $
;; $Author: npl25 $

;; This file is not part of Emacs

;; Author: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Maintainer: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Website: http://www.russet.org.uk

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This file provides the ability to add "trails" through a set of muse files;
;; in short to provide a natural ordering for files, rather like LaTeX2HTML or
;; Info files for instance. At the moment, only HTML publishing output is
;; supported, although the system would probably extend more naturally to more
;; linear formats.

;; For some websites, you need to work through from start to finish or,
;; alternatively, follow one or more paths through the pages. This is
;; particularly true for tutorial websites where the reader needs to see one
;; part first to make sense of the later pages. Basically, you need a book. 
;; Trails add this functionality - although unlike a book, it's possible for a
;; single page to occur in multiple places. 
; 
;; A Trail consists of one or more pages. A page can itself contain a trail --
;; so you can do subtrails. When published each page gets a next, previous
;; and up link if they exist. If the page is, itself, a trail an index of all
;; files below it will be added. 

;; To get this to work, first you must define a trail. Here for example is the
;; test trail I have used from development. You can specify links, text for
;; those links and subtrails. 
;; 
;; (setq test-muse-trail-test-trail
;;       `(
;;         ,(muse-trail 
;;           :text "Trail 1, Item 1" 
;;           :link "trail-1-item-1")
;;
;;         ,(muse-trail
;;           :text "Trail 1, Item 2"
;;           :link "trail-1-item-2")
;;       
;;         ,(muse-trail
;;           :text "subtrail 1 -index"
;;           :link "subtrail-1"
;;           :trail 
;;           `(
;;             ,(muse-trail
;;               :text "Trail 2, Item 1"
;;               :link "trail-2-item-1")
;;             ,(muse-trail
;;               :text "Trail 2, Item 2"
;;               :link "trail-2-item-2")))))
;;
;; Next you add the trail to your project. I use my own muse-settings.el which
;; allows me to create and configure projects in the same location as the muse
;; files; this is what it looks like. 
;;
;; (muse-settings-add-project
;;  `("trail"
;;    (,(muse-settings-local-path) :default "index.html"
;;     :trail ,test-muse-trail-test-trail)
;;    (:base "trail" :path ,(muse-settings-local-path))))
;;
;; Finally, you need to add publishing support. I do this in my header and
;; footer files. So
;;  
;; <lisp>(muse-trail-html-navigation-table)</lisp>
;; 
;; adds a cutesy little navigation bar in which I put in my header.xml. Then
;; in my footer.xml I add
;; 
;; <lisp>(muse-trail-html-trail-if-trail)</lisp>
;;
;; which adds a un-numbered listing of the trail underneath the current file,
;; if it has one. Finally, you need an index file called "trail-index.muse".
;; This will be the index for the entire project. 
;;

;;; Notes
;; 
;; A few more things to add here. Stylesheet support is needed. Full trail
;; information for placement above or to the left of the main content would be
;; good. A "<trail>" tag to control where the trail listing occurs. 
;;
;; Finally I want to do some nifty stuff, like adding timing information to
;; each item, and adding it up for entire trail. 

;;; Status
;; 
;;
;; This code is in active development. The core stuff works, but the
;; documentation is lacking and there are some nice features that have not
;; been described yet. 
;; 
;; Feedback is welcome. 
;;

;; begin basic data structure stuff
(require 'cl)

(defstruct muse-trail-item text link trail)

(defalias 'muse-trail 'make-muse-trail-item)

;; trail querying

(defun muse-trail-contains-link-p (trail file)
  "return t if trail contains item linking to file"
  (if (muse-trail-contains-link-item trail file)
      t nil))


(defun muse-trail-contains-link-item (trail file)
  "Returns item linking to file"
  (car 
   (member*
    (file-name-sans-extension 
     (file-name-nondirectory file))
    trail
    :test
    (lambda(file item)
      (equal file
             (muse-trail-item-link item))))))

(defun muse-trail-recursive-contains-link-p(trail file)
  "Returns true if the trail or substrails contain a link to the file."
  (if (muse-trail-recursive-contains-link-item trail file)
      t nil))


(defun muse-trail-recursive-contains-link-item (trail file)
  "returns item linking to file in trail or subtrail"
  (let ((file-stem (file-name-sans-extension 
                    (file-name-nondirectory file)))
        (item (car trail)))
    (if 
        ;; we found it...
        (equal (muse-trail-item-link item)
               file-stem)
        ;; return it
        item
      ;; we have a subtrail, so check that
      (or (and (muse-trail-item-trail-p item)
               (muse-trail-recursive-contains-link-item 
                (muse-trail-item-trail item) file))
          ;; search the rest of the list
          (if (cdr trail)
              (muse-trail-recursive-contains-link-item 
               (cdr trail) file)
            nil)))))


(defun muse-trail-fetch-up (trail item)
  "Returns the item up from ITEM in TRAIL. 
Returns nil if there is no item"
  (let ((current-item (car trail)))
    (cond
     ;;no trail, no up
     ((not trail)
      nil)
     ;; we found it...
     ((and 
       (muse-trail-item-trail-p current-item)
       (member item 
               (muse-trail-item-trail current-item)))
      ;; return it
      current-item)
     ;; we not found it
     (t
      (or 
       ;; but a subtrail beckons
       (and (muse-trail-item-trail-p current-item)
            (muse-trail-fetch-up 
             (muse-trail-item-trail current-item) item))
       ;; search the rest of the list
       (muse-trail-fetch-up
        (cdr trail) item))))))


(defun muse-trail-fetch-next (trail item)
  "Returns the item next from ITEM in TRAIL.
Returns nil if there is no item"
  ;; we have it...
  (cond
   ;; no trail, no next
   ((not trail)
    nil)
   ;; we found it, return next
   ((equal (car trail) item)
    (cadr trail))
   ;; we not found it
   (t
    (or 
     ;; but a subtrail so look in that
     (and (muse-trail-item-trail-p (car trail))
          (muse-trail-fetch-next
           (muse-trail-item-trail (car trail)) item))
     ;; or the tail of this list
     (muse-trail-fetch-next (cdr trail) item)))))


(defun muse-trail-fetch-previous (trail item)
  "Returns the item previous from the ITEM in TRAIL. 
Returns nil if there is no item."
  (let ((previous-item))
    (muse-trail-fetch-previous-1 trail item)))
  
(defun muse-trail-fetch-previous-1 (trail item)
  (cond 
   ;; end of trail
   ((not trail)
    nil)
   ;; we found
   ((equal (car trail) item)
    ;; so it were the one before
    previous-item)
   ;; we not found it
   (t
    (or
     ;; but a subtrail
     (and 
      (muse-trail-item-trail-p (car trail))
      (muse-trail-fetch-previous
       (muse-trail-item-trail (car trail)) item))
     ;; so remember this one and then check the rest
     (and (setq previous-item (car trail))
          (muse-trail-fetch-previous-1 (cdr trail) item))))))
                               


;; item querying
(defun muse-trail-item-trail-p (item)
  "Returns non-nil if the item has a subtrail"
  (if (muse-trail-item-trail item)
      t
    nil))


;; publishing trails, next, up and previous buttons
;; this is going to be HTML specific 
(defun muse-trail-add-trail(trail &optional project)
  "Helper function to add a trail to an existing project"
  (let* ((project (muse-project project))
         (old-trail (muse-trail-project-trail)))
    (when (member :trail (cadr project))
      (delq old-trail (cadr project))
      (delq :trail (cadr project)))
    (nconc (cadr project) `(:trail ,trail))))


(defun muse-trail-project-trail(&optional project)
  (setq project (muse-project project))
  (muse-get-keyword :trail (cadr project)))
  
(defun muse-trail-html-top()
  (format "<b>Top:</b> <a href=\"index.html\">Index</a>"))

(defun muse-trail-html-up()
  (let* ((trail
          (muse-trail-project-trail))
         (up-item 
          (muse-trail-fetch-up
           trail 
           (muse-trail-recursive-contains-link-item 
            trail muse-publishing-current-file))))
    (muse-trail-html-link "Up" up-item)))


(defun muse-trail-html-link (title item)
  (if item     
      (format
       "<b>%s</b>: <a href=\"%s.html\">%s</a>"
       title
       (muse-trail-item-link item)
       (muse-trail-item-text item))
    ""))


(defun muse-trail-html-next()
  (let* ((trail 
          (muse-trail-project-trail))
         (item 
          (muse-trail-fetch-next
           trail
           (muse-trail-recursive-contains-link-item 
            trail muse-publishing-current-file))))
    (muse-trail-html-link "Next" item)))

(defun muse-trail-html-previous()
  (let* ((trail 
          (muse-trail-project-trail))
         (item 
          (muse-trail-fetch-previous
           trail
           (muse-trail-recursive-contains-link-item 
            trail muse-publishing-current-file))))
    (muse-trail-html-link "Previous" item)))


(defun muse-trail-html-navigation-table-maybe()
  (when (muse-trail-project-trail)
    (muse-trail-html-navigation-table)))

(defun muse-trail-html-navigation-table()
  (format "<div class=\"trail-nav-bar\">%s&nbsp;%s %s %s</div>"
          (muse-trail-html-top)
          (muse-trail-html-previous)
          (muse-trail-html-up)
          (muse-trail-html-next)))


(defun muse-trail-html-trail (trail)
  "Return the HTML for given trail"
  (concat
   "<div class=\"trail-itemized\">\n"
   (muse-trail-html-trail-0 trail)
   "</div>\n"))

(defun muse-trail-html-trail-0 (trail)
  (concat "<ul>\n"
          (muse-trail-html-trail-1 trail)
          "</ul>\n"))

(defun muse-trail-html-trail-1 (trail)
  (let ((current-item (car trail)))
    (cond
     ((not trail)
      "")
     ;; current item is subtrail
     ((muse-trail-item-trail-p current-item)
      (concat
       ;; publish this item
       (muse-trail-html-from-item current-item)
       ;; publish the subtrail
       (muse-trail-html-trail-0 (muse-trail-item-trail current-item))
       ;; publish the rest of the list
       (muse-trail-html-trail-1 (cdr trail))))
     ;;current item is, er, an item
     (t
      (concat
       ;; publish this item
       (muse-trail-html-from-item current-item)
       ;; publish the rest of the list
       (muse-trail-html-trail-1 (cdr trail)))))))
                                        

(defun muse-trail-html-from-item (item)
  (format 
   "<li><a href=\"%s.html\">%s</a></li>\n" 
   (muse-trail-item-link current-item)
   (muse-trail-item-text current-item)))

  
(defun muse-trail-html-trail-maybe()
  "Return the HTML for current publishing item if it's a trail, or the index"
  (let* ((index-p
          (when (equal 
                   (file-name-nondirectory muse-publishing-current-file)
                   "index.muse")
            (muse-trail-project-trail)))
         (item
          ;; don't bother if we are index
          (unless index-p
            (muse-trail-recursive-contains-link-item
             (muse-trail-project-trail) 
             muse-publishing-current-file)))
         (subtrail
          (when item
            (muse-trail-item-trail-p item)
            (muse-trail-item-trail item)))
         (trail (or index-p subtrail)))
    (if trail
        (muse-trail-html-trail trail))))
        

(defun muse-trail-html-trail-tag (beg end attrs)
  (muse-insert-markup 
   (or (muse-trail-html-trail-maybe)
       " ")))


(add-to-list 'muse-html-markup-tags
             '("trail" t t t muse-trail-html-trail-tag))


;; TODO -- test and stuff...
;; nifty nav bar. 
;; only works sensibly with single inheritance
(defun muse-trail-nav-bar(&optional trail)
  
  ;; put start and stop on this...
  (muse-trail-nav-bar-1 
   (or trail
    (muse-trail-project-trail))))

(defun muse-trail-nav-bar-1(trail)
  (let ((downtrail)
        (item (car trail)))
    ;; terminate
    (if (not trail)
        (progn
          ;; publish the first item
          (muse-trail-nav-publish-item item)           
          ;; check whether the item is a trail and contains this file. If so,
          ;; remember it. 
          (if (and 
               (muse-trail-item-trail-p item)
               (muse-trail-recursive-contains-link-p
                muse-publishing-current-file))
              (setq downtrail item))
          ;; publish the trail
          (muse-trail-nav-bar-1 (cdr trail))))
    ;; we have finished publishing the trail
    ;; now publish the next item if it exists
    (when downtrail 
      (muse-trail-nav-bar downtrail))))


;; (defun muse-trail-html-banner()
;;   (format 
;;    "<span class=\"trail-banner\">%s</span>"
;;    (muse-trail-html-banner-1)))

;; (defun muse-trail-html-banner-1()
;;   (let ((trail (muse-trail-project-trail)))
    

;; some commands

(defun muse-trail-visit-next()
  (interactive)
  (let* ((trail 
          (muse-trail-project-trail))
         (item 
          (muse-trail-fetch-next
           trail
           (muse-trail-recursive-contains-link-item 
            trail (buffer-file-name)))))
    (if item
        (muse-visit-link (muse-trail-item-link item))
      (message "Node has no next"))))
  

(defun muse-trail-visit-prev()
 (interactive)
  (let* ((trail 
          (muse-trail-project-trail))
         (item 
          (muse-trail-fetch-previous
           trail
           (muse-trail-recursive-contains-link-item 
            trail (buffer-file-name)))))
    (if item
        (muse-visit-link (muse-trail-item-link item))
      (message "Node has no previous"))))

(defun muse-trail-visit-up()
  (interactive)
  (let* ((trail 
          (muse-trail-project-trail))
         (item 
          (muse-trail-fetch-up
           trail
           (muse-trail-recursive-contains-link-item 
            trail (buffer-file-name)))))
    (if item
        (muse-visit-link (muse-trail-item-link item))
      (message "Node has no up"))))

(defun muse-trail-visit-down()
  (interactive)
  (let* ((trail 
          (muse-trail-project-trail))
         (current-item 
          (muse-trail-recursive-contains-link-item 
           trail (buffer-file-name)))
         (down-trail (muse-trail-item-trail current-item)))
    (if down-trail
        (muse-visit-link 
         (concat (muse-trail-item-link (car down-trail)) ".muse"))
      (message "Node has no down"))))


(define-key muse-mode-map [(control ?c) (control ?,)] 'muse-trail-visit-prev)
(define-key muse-mode-map [(control ?c) (control ?.)] 'muse-trail-visit-next)
(define-key muse-mode-map [(control ?c) (control ?;)] 'muse-trail-visit-up)
(define-key muse-mode-map [(control ?c) (control ?/)] 'muse-trail-visit-down)



(provide 'muse-trail)