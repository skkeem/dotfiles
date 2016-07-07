#|
  This file is a part of CL-Locale package.
  URL: http://github.com/arielnetworks/cl-locale
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  CL-Locale is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage cl-locale
  (:use :cl
        :cl-locale.core
        :cl-locale.reader
        :cl-locale.syntax)
  (:nicknames :locale)
  (:export :*default-locale*
           :*locale*
           :*dictionary-tables*
           :register-dictionary
           :define-dictionary
           :current-dictionary
           :i18n
           :i18n-unformatted
           :enable-locale-syntax
           :locale-syntax
           :i18n-reader
           :i18n-unformatted-reader))
