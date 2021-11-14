;;; tsa.el --- RFC3161 compliant Time Stamp Authority client -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jesse Kelly

;; Author: Jesse Kelly <jessemkelly@gmail.com>
;; Keywords: RFC3161, TSA, timestamp
;; Package-Requires: ((emacs "27.2") (cl-lib "1.0") (dash "2.17") (org "9.4.6") (org-ml "5.7.1") (request "2.7.2"))
;; Version: 0.0.1

;; TODO: Extract common logic from tsa--request-body and tsa--get-properties
;; TODO: Modify tsa--get-property to work with raw string rather than buffer
;; TODO: Use org-ml everywhere
;; TODO: Name "token" the right thing (might be overloaded right now, too)
;; TODO: Scan for other todos in this file

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'org)
(require 'org-element)
(require 'org-ml)
(require 'url)

(setq tsa-no-token-message "No RFC3161 token present under heading")

(defcustom tsa-drawer-name "RFC3161-TOKEN"
  "Name of the drawer that holds tokens for headings whose contents are timestamped."
  :type 'string
  :group 'tsa)

(defcustom tsa-url "https://freetsa.org/tsr"
  "Time Stamp Authority URL against which requests are made."
  :type 'string
  :group 'tsa)

(defcustom tsa-root-ca "/Users/jesse/Code/elisp/tsa/castore/freetsa-ca.pem"
  "File path to root certificate for to use to verify tokens."
  :type 'string
  :group 'tsa)

(defcustom tsa-tag-name "timestamped"
  "Tag added to headings whose contents are timestamped."
  :type 'string
  :group 'tsa)

(defun tsa--request-body (text-file)
  "Generate an RFC3161-compliant request body for timestamping specified text file.

Returns the file path to the request body file."
  (let ((req-file (make-temp-file "tsa.el.request")))
    (call-process "openssl"
                  nil ;; Do not pipe input to process
                  nil ;; Do not save stdout in emacs
                  nil ;; Do not display output in emacs
                  "ts"
                  "-query"
                  "-data"
                  text-file
                  "-no_nonce"
                  "-cert"
                  "-sha512"
                  "-out"
                  req-file)
    req-file))

(defun tsa--curl-post (server req-file)
  "POST contents of 'req-file' request to 'server' using external cURL process.

Returns path to the response file."
  (let ((resp-file  (make-temp-file "tsa.el.response"))
        (stdout-buf (generate-new-buffer "*tsa curl stdout*")))
    (call-process "curl"
                  nil ;; Do not pipe input to process
                  (list stdout-buf t)
                  nil ;; Do not display output in emacs
                  "-v"
                  "-H"
                  "Content-Type: application/timestamp-query"
                  "--data-binary"
                  (concat "@" req-file)
                  "--output"
                  resp-file
                  server)
    resp-file))

(defun tsa--verify-token (token text ca-file)
  "Verify an RFC3136 token using a time stamp authority's public certificate.

TOKEN is RFC3136 token to verify.
TEXT is text that the TOKEN was generated for.
CA-STORE is URI of CA certificate store; may use 'file:' scheme.

Returns t if token is verified; f, otherwise."
  (let ((token-file-name (make-temp-file "tsa.el.token"))
        ;; TODO remove
        ;; (coding-system-for-read  'no-conversion)
        ;; (coding-system-for-write 'no-conversion)
        )
    (message "tsa--verify-token: Writing token to " token-file-name)
    (with-temp-file token-file-name
      (set-buffer-multibyte nil)
      (insert token))
    (with-temp-buffer
      (insert text)
      (call-process-region (point-min)
                           (point-max)
                           "openssl"
                           t         ;; DELETE the text
                           '(t       ;; Write stdout to temp buffer
                             nil)    ;; Ignore stderr
                           nil       ;; DISPLAY
                           "ts"
                           "-verify"
                           "-in"
                           token-file-name
                           "-data"
                           "/dev/stdin"
                           "-CAfile"
                           ca-file)
      (buffer-string))))

(defun tsa--get-properties (token)
  "Get the properties of an RFC3136 token."
  (with-temp-buffer
    (insert token)
    (call-process-region (point-min)
                         (point-max)
                         "openssl"
                         t         ;; DELETE the text
                         '(t       ;; Write stdout to temp buffer
                           nil)    ;; Ignore stderr
                         nil       ;; DISPLAY
                         "ts"
                         "-reply"
                         "-in"
                         "/dev/stdin"
                         "-text")
    (buffer-string)))

(defun tsa--get-property (token property)
  "Get a property of an RFC3136 token.

TOKEN is the RFC3136 token to get a property of.
PROPERTY is string prefix to use to extract output from 'openssl ts -reply'."
  (with-temp-buffer
    (insert (tsa--get-properties token))
    (goto-char (point-min))
    (search-forward property)
    (buffer-substring (point) (line-end-position))))

(defun tsa--is-token-drawer (node)
  "Return t if 'node' is a drawer with drawer name 'tsa-drawer-name'."
  (and (org-ml-is-type 'drawer node)
       (string= (org-ml-get-property :drawer-name node)
                tsa-drawer-name)))

(defun tsa--contents-as-text (heading)
  "Get all contents and children of heading with any RFC3136 token drawer removed."
  (->> heading
       (org-ml-headline-map-section
         (-partial #'-remove #'tsa--is-token-drawer))
       org-ml-get-children
       (-map #'org-ml-to-trimmed-string)
       (-reduce #'concat)))

(defun tsa--add-drawer-to-section (drawer heading)
  "Add drawer as first child after any property drawer to existing section of subtree."
  (org-ml-headline-map-section
    (lambda (children)
      (let ((first-child (car children)))
        (if (org-ml-is-type 'property-drawer first-child)
            (cons first-child
                  (cons drawer (cdr children)))
          (cons drawer children))))
    heading))

(defun tsa--add-drawer-to-heading (drawer heading)
  "Add section to heading and add drawer to section."
  (org-ml-map-children
    (lambda (children)
      (-> drawer
          org-ml-build-section
          (cons children)))
    heading))

(defun tsa--add-drawer (drawer heading)
  "Insert drawer under heading after any property drawer and before any contents."
  (if (org-ml-headline-get-section heading)
      (tsa--add-drawer-to-section drawer heading)
    (tsa--add-drawer-to-heading drawer heading)))

(defun tsa--get-token-drawer (heading)
  "Get the token drawer, if one exists, for heading."
  (->> heading
       org-ml-headline-get-section
       (org-ml-match '(:first (:and drawer
                                    (:drawer-name tsa-drawer-name))))
       car))

(defun tsa--create-token (server text-file)
  "Create an RFC3161 token for the specified text.

SERVER is the URL of the time stamp authority to make the request to.
TEXT is the text to generate the RFC3161 token for.

Returns the token returned by the time stamp authority."
  (let* ((req-file  (tsa--request-body     text-file))
         (resp-file (tsa--curl-post server req-file )))
    (message (concat "Request body written to "  req-file ))
    (message (concat "Response body written to " resp-file))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally resp-file)
      (buffer-substring-no-properties (point-min)
                                      (point-max)))))

(defun tsa--get-token ()
  "Get existing RFC3136 token for heading at point."
  (if-let* ((drawer (->> (org-ml-parse-this-subtree)
                      tsa--get-token-drawer))
            (begin         (org-ml-get-property :contents-begin drawer))
            (end           (org-ml-get-property :contents-end   drawer))
            (encoded-token (buffer-substring-no-properties begin end)))
    (base64-decode-string encoded-token)))

(defun tsa-get-heading-timestamp()
  "Get the time at which the heading at point was time stamped."
  (when-let ((token (tsa--get-token)))
    (tsa--get-property token
                       "Time stamp: ")))

(defun tsa--token-drawer (token)
  (->> token
       base64-encode-string
       org-ml-build-paragraph
       (org-ml-build-drawer tsa-drawer-name)))

(defun tsa--toggle-timestamp-tag (heading)
  (unless (org-ml-headline-has-tag tsa-tag-name heading)
    (org-ml-insert-into-property :tags 0 tsa-tag-name heading)))

(defun tsa--indent-subtree (subtree)
  (let* ((begin (org-element-property :begin subtree))
         (end   (org-element-property :end   subtree)))
    (indent-region begin end)))

(defun tsa-timestamp-heading ()
  "Generate an RFC3136 token for heading at point.

The token is placed in a drawer prior to the contents of the heading.
If there is already a token drawer, its contents are not included in the
timestamp"
  (interactive)
  (tsa--indent-subtree (org-ml-parse-this-subtree))
  (let ((heading   (org-ml-parse-this-subtree))
        (text-file (make-temp-file "tsa.el.text")))
    (with-temp-file text-file
      (insert (tsa--contents-as-text heading)))
    (message (concat "Text to timestamp written to " text-file))
    (org-ml-update (-compose #'tsa--toggle-timestamp-tag
                             (-partial #'tsa--add-drawer-to-section
                                       (->> text-file
                                            (tsa--create-token tsa-url)
                                            tsa--token-drawer)))
                   heading)
    (tsa--indent-subtree (org-ml-parse-this-subtree))))

(defun tsa-show-timestamp ()
  "Display the time at which the heading at point was time stamped.

The time is displayed as a transient message."
  (interactive)
  (message (or (tsa-get-heading-timestamp)
               tsa-no-token-message)))

(defun tsa-show-timestamp-properties ()
  "Display the properties for the RFC3136 token attached to the heading at point.

The properties are displayed in a temporary buffer."
  (interactive)
  (if-let ((token (tsa--get-token)))
      (with-output-to-temp-buffer "*timestamp properties*"
        (print (tsa--get-properties token)))
    (message tsa-no-token-message)))

(defun tsa-show-verification ()
  "Display the properties for the RFC3136 token attached to the heading at point.

The verification status is displayed as a transient message."
  (interactive)
  (message
   (if-let ((token (tsa--get-token)))
       (tsa--verify-token token
                          (-> (org-ml-parse-this-subtree)
                              tsa--contents-as-text)
                          tsa-root-ca)
     tsa-no-token-message)))

(defun tsa-show-token ()
  "Display the raw RFC3136 token attached to the heading at point.

The token is displayed in a temporary buffer."
  (interactive)
  (if-let ((token (tsa--get-token)))
      (let ((token-buffer (generate-new-buffer "*tsa token*")))
        (with-current-buffer token-buffer
          (set-buffer-multibyte nil)
          (insert token))
        (display-buffer token-buffer))
    (message tsa-no-token-message)))

(defun tsa-debug ()
  (interactive)
  (with-output-to-temp-buffer "*tsa debug*"
    (->> (tsa--get-token)
         print)))
