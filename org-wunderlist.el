;;; org-wunderlist.el --- Org sync with Wunderlist -*- lexical-binding: t -*-

;; Author: myuhe <yuhei.maeda_at_gmail.com>
;; URL: https://github.com/myuhe/org-wunderlist.el
;; Version: 0.1
;; Maintainer: myuhe
;; Copyright (C) :2015 myuhe all rights reserved.
;; Created: :15-06-28
;; Package-Requires: ((request-deferred "0.2.0") (alert "1.1") (emacs "24") (cl-lib "0.5") (org "8.2.4"))
;; Keywords: convenience,

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published byn
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 0:110-1301, USA.

;;; Commentary:
;;
;; Put the org-wunderlist.el to your
;; load-path.
;; Add to .emacs:
;; (require 'org-wunderlist)
;;
;;; Changelog:
;; 2015-06-28 Initial release.

;;; Code:

(require 'alert)
(require 'json)
(require 'request-deferred)
(require 'org-element)
(require 'org-archive)
(require 'cl-lib)
(require 's)

;; Customization
(defgroup org-wlist nil "Org sync with Wunderlist"
  :tag "Org Wunderlist"
  :group 'org)

(defcustom org-wlist-client-id nil
  "Client ID for OAuth."
  :group 'org-wlist
  :type 'string)

(defcustom org-wlist-token nil
  "Google calendar secret key for OAuth."
  :group 'org-wlist
  :type 'string)

(defcustom org-wlist-file nil
  "File for synchronize Wunderlist."
  :group 'org-wlist
  :type 'file)

(defcustom org-wlist-store-change t
  "  "
  :group 'org-wlist
  :type 'boolean)

(defcustom org-wlist-icon "WunderList.png"
  "org-gcal icon filename."
  :group 'org-gcal
  :type `(choice  ,@(mapcar (lambda (c)
                       `(const :tag ,c ,c))
                            '("WunderList.png"
                              "WunderList_black.png"
                              "WunderList_black2.png"
                              "WunderList_white.png"
                              "WunderList_white2.png"
                              "WunderList.svg"
                              "WunderList2.svg"
                              "WunderList3.svg"))))

(defconst org-wlist-url "https://a.wunderlist.com/api/v1/")

(defvar org-wlist-plist '(:id nil :rev nil :list nil :list-pos))

(defmacro org-wlist-append-to-list (to lst)
  `(setq ,to (append ,to ,lst )))

(defun org-wlist-fetch ()
  (interactive)
  (remove-hook 'after-change-functions 'org-wlist--collect-change-pos t)
  (let ((buf  (org-wlist--get-buffer)))
  (deferred:$
    (deferred:parallel
      (mapcar (lambda (endpoint)
                (org-wlist--request endpoint))
              '("root" "lists" "list_positions")))
    (deferred:nextc it
      (lambda (res)
        (plist-put org-wlist-plist :id  (plist-get (car res) :id))
        (plist-put org-wlist-plist :rev (plist-get (car res) :revision))
        (plist-put org-wlist-plist :list  (cadr res))
        (plist-put org-wlist-plist :list-pos  (caddr res))
        (mapcar (lambda (x)
                  (number-to-string (plist-get x :id)) ) (cadr res))))
    (deferred:nextc it
      (lambda (id)
        (deferred:$
          (deferred:parallel
            (apply 'append
                   (cl-mapcar (lambda (name)
                                (cl-mapcar (lambda (endpoint params)
                                             (org-wlist--request endpoint `(("list_id" . ,params))))
                                           (make-list (length id) name) id))
                              '("tasks" "task_positions" "subtask" "subtask_positions" "notes" "reminders"))))
          (deferred:nextc it
            (lambda (res)
              (cl-mapcar (lambda (lst task task-pos subtask subtask-pos note reminder)
                           (plist-put lst :task task)
                           (plist-put lst :task-pos task-pos)
                           (plist-put lst :subtask subtask)
                           (plist-put lst :subtask-pos subtask-pos)
                           (plist-put lst :note note)
                           (plist-put lst :reminder reminder))
                         (plist-get org-wlist-plist :list)
                         (org-wlist--map (number-sequence 0                 (1- (length id))) res)
                         (org-wlist--map (number-sequence (length id)       (1- (* 2 (length id)))) res)
                         (org-wlist--map (number-sequence (* 2 (length id)) (1- (* 3 (length id)))) res)
                         (org-wlist--map (number-sequence (* 3 (length id)) (1- (* 4 (length id)))) res)
                         (org-wlist--map (number-sequence (* 4 (length id)) (1- (* 5 (length id)))) res)
                         (org-wlist--map (number-sequence (* 5 (length id)) (1- (* 6 (length id)))) res))
              (with-current-buffer buf
                (save-excursion
                  (erase-buffer)
                  (insert (org-wlist--dump))
                  (save-buffer)
                  (org-global-cycle 64)
                  (add-hook 'after-change-functions 'org-wlist--collect-change-pos nil t)))))))))))

(defun org-wlist--dump ()
  (let (str-lst)
    (cl-loop for lst-id across (plist-get (elt (plist-get org-wlist-plist :list-pos) 0) :values)
             do
             (cl-loop for lst-value across (plist-get org-wlist-plist :list)
                      when (eq lst-id (plist-get lst-value :id))
                      do
                      (setq str-lst
                            (org-wlist--append-list str-lst lst-value))
                      (cl-loop for task-pos-id across (org-wlist--get-prop-value lst-value :task-pos)
                               do
                               (cl-loop for task-value across (plist-get lst-value :task)
                                        when (equal task-pos-id (plist-get task-value :id))
                                        do
                                        (setq str-lst
                                              (org-wlist--append-header str-lst task-value))
                                        (setq str-lst
                                              (org-wlist--loop  lst-value :reminder  :task_id task-value :id
                                                                'org-wlist--append-remind str-lst))
                                        (setq str-lst
                                              (org-wlist--loop  lst-value :note  :task_id task-value :id
                                                                'org-wlist--append-note str-lst))
                                        (org-wlist-append-to-list str-lst (list "  :END:\n"))
                                        (setq str-lst
                                              (org-wlist--loop  lst-value :note  :task_id task-value :id
                                                                'org-wlist--append-content str-lst))
                                        (cl-loop for subtask-pos-id across (org-wlist--get-prop-value
                                                                            lst-value :subtask-pos)
                                                 do
                                                 (cl-loop  for subtask-value across (plist-get lst-value :subtask)
                                                           when (equal subtask-pos-id (plist-get subtask-value :id))
                                                           do (setq str-lst
                                                                    (org-wlist--append-subtask str-lst subtask-value))))))))
    (mapconcat 'identity str-lst "")))

(defun org-wlist-post ()
  (interactive)
  (save-excursion
    (org-back-to-heading)
  (let* ((next-head (org-wlist--get-next-headline))
         (elem (org-element-headline-parser next-head t))
         (title (org-element-property :title elem))
         (level (org-element-property :level elem))
         (id (org-wlist--get-valid-id elem))
         (complete (when (string= "DONE"
                                  (org-element-property :todo-keyword elem)) t))
         (star (when (eq 65 (org-element-property :priority elem)) t))
         (rev (org-wlist--string-to-number-safe
               (org-element-property :REV elem)))
         (remind (org-element-property :REMIND elem))
         (remind-rev (org-wlist--string-to-number-safe
                      (org-element-property :REMIND-REV elem)))
         (remind-id (org-wlist--string-to-number-safe
                     (org-element-property :REMIND-ID elem)))
         (deadline (cadr (org-element-property :deadline elem)))
         (year (plist-get deadline :year-end))
         (month (plist-get deadline :month-end))
         (day (plist-get deadline :day-end))
         (date (when year
                 (concat (number-to-string year) "-" (format "%02d" month)"-" (format "%02d" day))))
         (parent-id 
          (save-excursion
            (outline-up-heading 1)
            (org-wlist--string-to-number-safe
            (org-element-property :ID (org-element-headline-parser next-head t)))))
         (note  (if (plist-get (cadr elem) :contents-begin)
                    (replace-regexp-in-string
                     " *\\(.*\\(?:\n.*\\)*?\\) :END:\n" ""
                     (buffer-substring-no-properties
                      (plist-get (cadr elem) :contents-begin)
                      (plist-get (cadr elem) :contents-end))) ""))
         (note-rev (org-wlist--string-to-number-safe
                    (org-element-property :NOTE-REV elem)))
         (note-id (org-wlist--string-to-number-safe
                   (org-element-property :NOTE-ID elem))))
    (if id
        (if (eq level 2)
            (unless (and (string= title (org-wlist--get-prop id :task :title))
                         (eq complete nil)
                         (string= date (org-wlist--get-prop id :task :due_date))
                         (eq star (if (eq ':json-false (org-wlist--get-prop id :task :starred)) nil t)))
              (org-wlist--post-request "PATCH" (concat "tasks/" (number-to-string id)) :task
                                       `(("revision" . ,rev)
                                         ("title" . ,title)
                                         ("completed" . ,complete)
                                         ("due_date" . ,date)
                                         ("starred" . ,star))))
          (unless (and (string= title (org-wlist--get-prop id :subtask :title))
                       (eq complete nil))
            (org-wlist--post-request "PATCH" (concat "subtasks/" id) :subtask
                                     `(("revision" . ,rev)
                                       ("title" . ,title)
                                       ("completed" . ,complete)))))
      (if (eq level 2)
          (org-wlist--post-request "POST" "tasks" :task
                                   `(("list_id" . ,parent-id)
                                     ("title" . ,title)
                                     ("due_date" . ,date)))
        (org-wlist--post-request "POST" "subtasks" :task
                                 `(("task_id" . ,parent-id)
                                   ("title" . ,title)))))
    (when remind
      (let* ((iso-date (org-wlist--format-org2iso
                        (string-to-number (substring remind 1 5))
                        (string-to-number (substring remind 6 8))
                        (string-to-number (substring remind 9 11))
                        (string-to-number (substring remind 14 16))
                        (string-to-number (substring remind 17 19)) t)) )
        (if remind-rev
            (unless (string= (org-wlist--get-prop remind-id :reminder :date) iso-date)
              (org-wlist--post-request
               "PATCH"
               (concat "reminders/" (number-to-string remind-id)) :reminder
               `(("revision" . ,remind-rev)
                 ("id" . ,remind-id)
                 ("date" . ,iso-date))))
          (org-wlist--post-request "POST" "reminders" :reminder
                                   `(("task_id" . ,id)
                                     ("date" . ,iso-date))))))
    (when (and (not (string= "" note))  (eq level 2))
      (if note-rev
          (unless (string= (org-wlist--get-prop note-id :note :content) note)
            (org-wlist--post-request "PATCH" (concat "notes/" (number-to-string note-id)) :note
                                     `(("revision" . ,note-rev)
                                       ("content" . ,note))))
        (org-wlist--post-request "POST" "notes" :note
                                 `(("task_id" . ,id)
                                   ("content" . ,note))))))))

(defun org-wlist-post-all ()
  (interactive)
  (let* ((buf (get-file-buffer org-wlist-file))
         (buffer-undo-list t)
         (inhibit-read-only t)
         (inhibit-point-motion-hooks t)
         (inhibit-modification-hooks t)
         deactivate-mark buffer-file-name buffer-file-truename)
    (with-current-buffer buf
      (cl-loop with pos = (point-min)
               for next = (next-single-property-change pos 'org-wlist)
               while next do
               (goto-char next)
               (org-wlist-post)
               (alert  "test":title "test")
               (setq pos (1+ next))
               finally
               (remove-text-properties (point-min) (point-max) '(org-wlist nil))))))

(defun org-wlist-change-pos ()
    (interactive)
  (org-wlist-change-pos-1 'org-wlist--pos-buffer))

(defun org-wlist-change-pos-1  (pos-fun)
  (let* ((pos-lst (funcall pos-fun)))
    (unless (equal (vconcat (car pos-lst)) (org-wlist--get-list-pos-prop :values))
      (org-wlist--post-request
       "PATCH"  (concat "list_positions/"
                        (number-to-string (org-wlist--get-list-pos-prop :id)))
       `(("values"  . ,(vconcat   (car pos-lst)))
         ("revision" . ,(org-wlist--get-list-pos-prop :revision)))))
    (cl-loop for lst-id in (car pos-lst)
             for task-id in (cadr pos-lst)
             unless (eq task-id nil)
             do (cl-loop for lst-value across (plist-get org-wlist-plist :list)
                         when (eq (plist-get lst-value :id) lst-id)
                         do  (unless (equal
                                      (vconcat task-id)
                                      (plist-get (elt (plist-get lst-value :task-pos) 0) :values))
                               (org-wlist--post-request
                                "PATCH"
                                (concat "task_positions/"
                                        (number-to-string (plist-get (elt (plist-get lst-value :task-pos) 0) :id))) :task-pos
                                        `(("values"  . ,(vconcat   task-id))
                                          ("revision" . ,(plist-get (elt (plist-get lst-value :task-pos) 0) :revision)))))))))

(defun org-wlist--post-pos ()
  (interactive)
  (deferred:$
    (request-deferred
     (concat org-wlist-url "tasks")
     :type "GET"
     :params `(("list_id" . ,(org-wlist--get-parent-id)))
     :headers `(("X-Access-Token" . ,org-wlist-token)
                ("X-Client-ID" . ,org-wlist-client-id))
     :parser 'org-wlist--json-read)
    (deferred:nextc it
      (lambda (res)
        (org-wlist--post-request
         "PATCH" (concat "task_positions/" (org-wlist--get-parent-id)) :task-pos
         `(("values"  . ,(vconcat   (cl-loop for value across (request-response-data res)
                                             collect (plist-get value :id))))
           ("revision" . ,(org-wlist--get-prop (string-to-number (org-wlist--get-parent-id)) :task-pos :revision))))))))
        
(defun org-wlist--pos-buffer ()
  (let* ((local-id (mapcar 'string-to-number (with-current-buffer (org-wlist--get-buffer)
                     (org-element-map (org-element-parse-buffer) 'headline
                       (lambda (hl) (org-element-property :ID hl))))))
         (local-level  (with-current-buffer (org-wlist--get-buffer)
                        (org-element-map (org-element-parse-buffer) 'headline
                          (lambda (hl) (org-element-property :level hl))))))
  (cl-loop with cache-level  = 0
           with list
           with task
           with subtask
           with task-list
           with subtask-list
           for id in local-id
           for level in local-level
           when (eq level 1) collect id into list
           when (and (eq level 1) (>= cache-level level))
           collect task into task-list
           and do (setq task nil)
           when (eq level 2) 
           collect id into task
           when (and (< level 3) (or (> cache-level level) (and (eq cache-level 2) (eq level 2))))
           collect subtask into subtask-list
           and do (setq subtask nil)
           when (eq level 3)
           collect id into subtask
           do (setq cache-level level)
           finally
           (when (not (eq cache-level 3))
             (setq task-list (append task-list (list task))))
           (when (not (eq cache-level 1))
             (setq subtask-list (append subtask-list (list subtask))))
           (return `(,list ,task-list ,subtask-list)))))

(add-hook 'find-file-hook
          (lambda ()
            (when (and (eq org-wlist-store-change t)
                       (eq (current-buffer) (org-wlist--get-buffer)))
              (add-hook 'after-change-functions 'org-wlist--collect-change-pos nil t))))

(defun org-wlist--collect-change-pos (&optional beg end len)
  (save-excursion
    (when (eq  (current-buffer) (org-wlist--get-buffer))
      (org-back-to-heading)
      (put-text-property (point) (1+ (point)) 'org-wlist t))
    (when (eq  (buffer-base-buffer (current-buffer)) (org-wlist--get-buffer))
        (remove-hook 'after-change-functions 'org-wlist--collect-change-pos t))))

(defun org-wlist--collect-change-pos-1 ()
  (save-excursion
      (org-back-to-heading)
      (put-text-property (point) (1+ (point)) 'org-wlist t)))

(add-hook 'org-capture-before-finalize-hook 'org-wlist--collect-change-pos-1)

(defun org-wlist--get-prop (id resource prop)
    (cl-loop for lst-value across (plist-get org-wlist-plist :list)
             with match
             do (setq match (cl-loop for task-value across (plist-get lst-value resource)
                                   when (equal (plist-get task-value :id) 
                                                id)
                                   return (plist-get task-value prop)))
             when match
             return match))

(defun org-wlist--increase-id (resource)
  (save-excursion
    (org-back-to-heading)
    (let (level)
      (if (or (eq resource :reminder)
              (eq resource :note))
        (let* ((elem (org-element-headline-parser (point-max) t))
               (rev (org-wlist--string-to-number-safe (org-element-property
                                       (if (eq resource :note)
                                           :NOTE-REV :REMIND-REV) elem))))
          (re-search-forward (if (eq resource :note)
                                           ":NOTE-REV:" ":REMIND-REV:") nil t)
          (re-search-forward "[0-9].*" nil t)
          (replace-match (number-to-string (1+ rev))))  
      (while (not (eq level 1))
        (let* ((elem (org-element-headline-parser (point-max) t))
               (rev (org-wlist--string-to-number-safe
                     (org-element-property :REV elem))))
          (setq level  (org-element-property :level elem))
          (re-search-forward ":REV:" nil t)
          (re-search-forward "[0-9].*" nil t)
          (replace-match (number-to-string (1+ rev)))
          (unless (eq level 1)
          (outline-up-heading 1))))))))

(defun org-wlist--request (endpoint &optional params)
  (deferred:$
    (request-deferred (concat org-wlist-url endpoint)
                      :type "GET"
                      :params params
                      :headers `(("X-Access-Token" . ,org-wlist-token)
                                 ("X-Client-ID" . ,org-wlist-client-id))
                      :parser 'org-wlist--json-read)
    (deferred:nextc it
      (lambda (res)
        (request-response-data res)))))

(defun org-wlist--post-request (type endpoint resource data)
  (deferred:$
    (request-deferred (concat org-wlist-url endpoint)
                      :type type
                      :data (json-encode data)
                      :headers  `(("X-Access-Token" . ,org-wlist-token)
                                  ("X-Client-ID" . ,org-wlist-client-id)
                                  ("Content-Type" . "application/json"))
                      :parser 'org-wlist--json-read
                      :error
                      (cl-function (lambda (&key error-thrown)
                                     (message "Got error: %S" error-thrown))))
    (deferred:nextc it
      (lambda (res)
        (let ((dat (request-response-data res)))
          (cond
           ((eq resource :task)
            (alert "Success" :title "Task"))
            ((eq resource :subtask)
            (alert "Success" :title "SubTask"))
           ((eq resource :reminder)
            (alert "Success" :title "Reminder"))
           ((eq resource :note)
            (alert "Success" :title "Note")))
          (if (string= type "POST")
              (org-wlist--post-pos)
            (org-wlist--increase-id resource)))))))

(defun org-wlist--get-list-pos-prop (prop)
  (plist-get (elt (plist-get org-wlist-plist :list-pos) 0) prop))

(defun org-wlist--get-task-prop (lst task prop)
  (plist-get (elt (org-wlist--get-list-prop lst :task) task) prop))
  
(defun org-wlist--get-list-prop (lst prop)
    (plist-get (elt (plist-get org-wlist-plist :list) lst) prop))

(defun org-wlist--map (lst seq)
  (mapcar
   (lambda (x) (nth x seq)) lst))

(defun org-wlist--get-next-headline ()
  (save-excursion
    (outline-next-heading)
    (point)))

(defun org-wlist--get-parent-id ()
  (save-excursion
    (outline-up-heading 1)
    
     (org-element-property :ID (org-element-headline-parser (point-max) t) )))

(defun org-wlist--format-org2iso (year mon day &optional hour min tz)
  (concat
   (format-time-string
    (if (or hour min) "%Y-%m-%dT%H:%M" "%Y-%m-%d")
    (seconds-to-time
     (-
      (time-to-seconds
       (encode-time 0
                    (if min min 0)
                    (if hour hour 0)
                    day mon year))
      (if tz
          (car (current-time-zone)) 0))))
   (when (or hour min) ":00.000Z")))

(defun org-wlist--format-iso2org (str &optional tz)
  (let ((plst (org-wlist--parse-date str)))
    (concat
     "<"
     (format-time-string
      (if (< 11 (length str)) "%Y-%m-%d %a %H:%M" "%Y-%m-%d %a")
      (seconds-to-time
       (+ (if tz (car (current-time-zone)) 0)
          (org-wlist--time-to-seconds plst))))
     ">")))

(defun org-wlist--time-to-seconds (plst)
  (time-to-seconds
   (encode-time
    (plist-get plst :sec)
    (plist-get plst :min)
    (plist-get plst :hour)
    (plist-get plst :day)
    (plist-get plst :mon)
    (plist-get plst :year))))

(defun org-wlist--parse-date (str)
  (list :year (string-to-number (org-wlist--safe-substring str 0 4))
        :mon  (string-to-number (org-wlist--safe-substring str 5 7))
        :day  (string-to-number (org-wlist--safe-substring str 8 10))
        :hour (string-to-number (org-wlist--safe-substring str 11 13))
        :min  (string-to-number (org-wlist--safe-substring str 14 16))
        :sec  (string-to-number (org-wlist--safe-substring str 17 19))))

(defun org-wlist--safe-substring (string from &optional to)
  "Calls the `substring' function safely.
\nNo errors will be returned for out of range values of FROM and
TO.  Instead an empty string is returned."
  (let* ((len (length string))
         (to (or to len)))
    (when (< from 0)
      (setq from (+ len from)))
    (when (< to 0)
      (setq to (+ len to)))
    (if (or (< from 0) (> from len)
            (< to 0) (> to len)
            (< to from))
        ""
      (substring string from to))))

(defun org-wlist--string-to-number-safe (string)
  "Safely convert STRING to a number.
If STRING is of string type, and a numeric string (see
`s-numeric?'), convert STRING to a number and return it.
Otherwise return nil."
  (when (and (stringp string) (s-numeric? string))
    (string-to-number string)))

(defun org-wlist--json-read ()
  (let ((json-object-type 'plist))
    (goto-char (point-min))
    (re-search-forward "[\\[{]" nil t)
    (beginning-of-line)
    (delete-region (point-min) (point))
    (goto-char (point-min))
    (json-read))) 

(defun org-wlist--get-valid-id (elem)
  (if (org-element-property :ID elem)
      (org-wlist--string-to-number-safe
       (format "%05.00d"
               (org-wlist--string-to-number-safe
                (org-element-property :ID elem))))
    nil))

(defun org-wlist--append-list (str-lst plst)
  (append
   str-lst (list (concat "* " (plist-get plst :title)  "\n"
                         "  :PROPERTIES:\n"
                         "  :ID: " (number-to-string (plist-get plst :id)) "\n"
                         "  :REV: " (number-to-string (plist-get plst :revision)) "\n"
                         "  :END:\n"))))

(defun org-wlist--append-header (str-lst plst)
  (org-wlist-append-to-list
   str-lst
   (list (concat
          "** TODO " (unless (eq ':json-false (plist-get plst :starred)) "[#A] ")
          (plist-get plst :title)  "\n"
          (when (plist-get plst :due_date)
            (concat
             "   DEADLINE: "
             (format-time-string "<%Y-%m-%d %a>\n"
                                 (apply #'encode-time
                                        (mapcar (lambda (x) (if (null x) 0 x))
                                                (parse-time-string (plist-get plst :due_date)))))))
          "   :PROPERTIES:\n"
          "   :ID: " (number-to-string (plist-get plst :id)) "\n"
          "   :REV: " (number-to-string (plist-get plst :revision)) "\n"))))

(defun org-wlist--get-buffer ()
  (or (get-file-buffer org-wlist-file)
      (find-file-noselect org-wlist-file)))

(defun org-wlist--append-remind (str-lst plst)
  (append
   str-lst (list (concat "   :REMIND: " (org-wlist--format-iso2org (plist-get plst :date) t)  "\n")
                 (concat "   :REMIND-REV: " (number-to-string (plist-get plst :revision))  "\n")
                 (concat "   :REMIND-ID: " (number-to-string (plist-get plst :id))  "\n"))))

(defun org-wlist--append-note (str-lst plst)
  (append
   str-lst (list (concat "   :NOTE-REV: " (number-to-string (plist-get plst :revision))  "\n")
                 (concat "   :NOTE-ID: " (number-to-string (plist-get plst :id))  "\n"))))

(defun org-wlist--append-content (string-list plst)
(append string-list (list (concat "" (plist-get plst :content)  "\n"))))

(defun org-wlist--get-prop-value (lst-value prop)
  (plist-get (elt (plist-get lst-value prop) 0) :values))

(defun org-wlist--append-subtask (str-lst plst)
   (append str-lst (list (concat "*** TODO " (plist-get plst :title)  "\n"
                         "   :PROPERTIES:\n"
                         "   :ID: " (number-to-string (plist-get plst :id)) "\n"
                         "   :REV: " (number-to-string (plist-get plst :revision)) "\n"
                         "   :END:\n"))))

(defun org-wlist--loop (base-prop base-id child-id parent-prop parent-id fun str-lst) 
  (cl-loop for plst across (plist-get base-prop base-id)
           when (equal (plist-get plst child-id) (plist-get parent-prop parent-id))
           do (setq str-lst (funcall fun str-lst plst)))
  str-lst)

(defun org-wlist--notify (title mes)
  (let ((file (expand-file-name (concat user-emacs-directory
                                        "org-wunderlist/"
                                        org-wlist-icon)))
                (mes mes)
                (title title))
    (if (file-exists-p file)
        (if (eq system-type 'gnu/linux)
            (alert mes :title title :icon file)
          (alert mes :title title))
      (deferred:$
        (deferred:url-retrieve
          (concat "https://raw.githubusercontent.com/myuhe/org-wunderlist.el/master/"
                  org-wlist-icon))
        (deferred:nextc it
          (lambda (buf)
            (with-current-buffer buf
               (let ((tmp (substring (buffer-string) (+ (string-match "\n\n" (buffer-string)) 2))))
                 (erase-buffer)
                 (insert tmp)
                 (write-file file)))
            (kill-buffer buf)
            (if (eq system-type 'gnu/linux)
            (alert mes :title title :icon file)
          (alert mes :title title))))))))

(provide 'org-wunderlist)

;;; org-wunderlist.el ends here
