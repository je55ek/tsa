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

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'org)
(require 'org-element)
(require 'org-ml)
(require 'request)


(defcustom tsa-drawer-name "RFC3161-TOKEN"
  "Name of the drawer that holds tokens for headings whose contents are timestamped."
  :type 'string
  :group 'tsa)

(defcustom tsa-url "https://freetsa.org/tsr"
  "Time Stamp Authority URL against which requests are made."
  :type 'string
  :group 'tsa)

(defun tsa--request-body (text)
  "Generate an RFC3161-compliant request body for timestamping specified text."
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
                         "-query"
                         "-data"
                         "/dev/stdin"
                         "-no_nonce"
                         "-cert"
                         "-sha512")
    (buffer-string)))

(defun tsa--verify-token (token text ca-store)
  "Verify an RFC3136 token using a time stamp authority's public certificate.

TOKEN is RFC3136 token to verify.
TEXT is text that the TOKEN was generated for.
CA-STORE is URI of CA certificate store; may use 'file:' scheme.

Returns t if token is verified; f, otherwise."
  (let ((token-file-name (make-temp-file "tsa.el.token"))
        (coding-system-for-read  'no-conversion)
        (coding-system-for-write 'no-conversion))
    (message token-file-name)
    (with-temp-file token-file-name
      (insert token)
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
                             "-CAstore"
                             ca-store)
        (buffer-string)))))

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

(defun tsa--section-no-drawer ()
  "Get the section node under a heading with any RFC3136 token drawer removed."
  (->> (org-ml-parse-this-section)
       (org-ml-map-children
         (-partial #'-remove
                   (lambda (e)
                     (and (org-ml-is-type 'drawer e)
                          (string= (org-ml-get-property :drawer-name e)
                                   tsa-drawer-name)))))))

(defun tsa--insert-token (text server)
  "Insert RFC3161 token for contents of heading at point.

TEXT is the text to generate the RFC3161 token for.
SERVER is the URL of the time stamp authority to make the request to.

Returns base64 encoded token."
  (let ((body (tsa--request-body text)))
    (request server
     :type     "POST"
     :encoding 'binary
     :data     body
     :headers  '(("Content-Type" . "application/timestamp-query"))
     :parser   'buffer-string
     :complete (cl-function
                (lambda (&key data &allow-other-keys)
                  (insert (base64-encode-string data))
                  (next-line)
                  (org-cycle)
                  (let* ((element (org-element-at-point))
                         (begin   (org-element-property :begin element))
                         (end     (org-element-property :end   element)))
                    (indent-region begin end)))))))

(defun tsa--get-token ()
  "Get existing RFC3136 token for heading at point."
  (org-back-to-heading-or-point-min)
  (org-narrow-to-subtree)
  (let* ((ast    (org-element-parse-buffer))
         (drawer (org-element-map
                     ast
                     'drawer
                     (lambda (d)
                       (when (string= tsa-drawer-name
                                      (org-element-property :drawer-name d))
                         d))
                     nil  ;; INFO (nil is default)
                     t))  ;; Stop at first match
         (begin         (org-element-property :contents-begin drawer))
         (end           (org-element-property :contents-end   drawer))
         (encoded-token (buffer-substring-no-properties begin end))
         (decoded-token (with-temp-buffer
                          (org-mode)
                          (insert encoded-token)
                          (org-unindent-buffer)
                          (base64-decode-region (point-min)
                                                (point-max))
                          (buffer-string))))
    (widen)
    decoded-token))

(defun tsa-get-heading-timestamp()
  "Get the time at which the heading at point was time stamped."
  (tsa--get-property (tsa--get-token)
                     "Time stamp: "))

(defun tsa-timestamp-heading-2 ()
  "Generate an RFC3136 token for heading at point.

The token is placed in a :RFC3136-TOKEN: drawer prior to the contents of the heading."
  (interactive)
  (org-back-to-heading-or-point-min)
  (org-toggle-tag "timestamped" 'on)
  (let* ((element (org-ml-parse-this-element))
         (begin   (org-ml-get-property :contents-begin element))
         (end     (org-ml-get-property :contents-end   element))
         (section (tsa--section-no-drawer)))
    (goto-char begin)
    (org-insert-drawer nil tsa-drawer-name)
    (tsa--insert-token (org-ml-to-trimmed-string section)
                       tsa-url)))

(defun tsa-timestamp-heading ()
  "Generate an RFC3136 token for heading at point.

The token is placed in a :RFC3136-TOKEN: drawer prior to the contents of the heading."
  (interactive)
  (org-back-to-heading-or-point-min)
  (org-toggle-tag "timestamped" 'on)
  (let* ((element (org-element-at-point))
         (begin   (org-element-property :contents-begin element))
         (end     (org-element-property :contents-end   element))
         (text    (buffer-substring-no-properties begin end)))
    (goto-char begin)
    (org-insert-drawer nil tsa-drawer-name)
    (tsa--insert-token text tsa-url)))

(defun tsa-show-timestamp ()
  "Display the time at which the heading at point was time stamped.

The time is displayed as a transient message."
  (interactive)
  (message (tsa-get-heading-timestamp)))

(defun tsa-show-timestamp-properties ()
  "Display the properties for the RFC3136 token attached to the heading at point.

The properties are displayed in a temporary buffer."
  (interactive)
  (with-output-to-temp-buffer "*timestamp properties*"
    (print (tsa--get-properties (tsa--get-token)))))

(defun tsa-show-verification ()
  "Display the properties for the RFC3136 token attached to the heading at point.

The verification status is displayed as a transient message."
  (interactive)
  (with-output-to-temp-buffer "*timestamp properties*"
    (print (tsa--verify-token (tsa--get-token)
                              (-> (tsa--section-no-drawer) (org-ml-to-trimmed-string))
                              "file:///Users/jesse/Code/elisp/tsa/castore/"))))

(defun tsa-debug ()
  (interactive)
  (with-output-to-temp-buffer "*timestamp properties*"
    (print (tsa--section-no-drawer))))
