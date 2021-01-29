;;; colortrans.el --- simple conversion tool between truecolor and xterm -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020  Gerry Agbobada

;; Author: Gerry Agbobada ( gagbobada+git@gmail.com )
;; Version: 0.0.1
;; Created: May 9th, 2019
;; Modified: February 7th, 2020
;; Keywords: color

;; This file is not part of GNU Emacs.

;;; License:
;; You can redistribute this program and/or modify it under the terms of the MIT License

;;; Commentary:
;; This module provides conversion utilities between arbitrary 24 bit colors and colors from
;; given lists.
;; The module provides 2 functions which give the closest 256-color color and the closest named
;; color in the current emacsen.
;;
;; We avoid defined-colors to convert to xterm because it does not have
;; only xterm palette colors (see M-x list-colors-display)

;;; Code:
(require 'color)
(require 'cl-macs)
(require 'dash)


(defun colortrans--normalize-rgb (rgb_int_val)
  "Return RGB_INT_VAL scaled from 0-255 to 0-1.0.
   Values are truncated to the thousandth."
  (let ((truncator 0.0009765625)
        (max_rgb_val 255.0))
    (* (truncate (/ rgb_int_val max_rgb_val) truncator) truncator)))

(defun colortrans--hsv-distance  (col1 col2)
  "Return an estimation of the distance between COL1 and COL2."
  ;; As all values for the hsv vector are normalized to [0; 1],
  ;; the LOWER the "norm_exponents" value, the HIGHER the impact of the associated component.
  ;; i.e. having '(2 2 1) in norm_exponents mean you prefer matching value over hue or saturation
  (let* ((norm_exponents '(2 2 1))
         (hue_diff (abs (- (aref col1 0) (aref col2 0))))
         (min_hue_diff (/ (min hue_diff (- (* 2 float-pi) hue_diff)) (* 2 float-pi)))
         (sat_diff (abs (- (aref col1 1) (aref col2 1))))
         (val_diff (abs (- (aref col1 2) (aref col2 2))))
         (col_diff (list min_hue_diff sat_diff val_diff)))
    (-sum (--zip-with (expt it other) col_diff norm_exponents)))
  )

(defun colortrans-hex-to-vec (rrggbb)
  "Convert color *rrggbb from “\"rrggbb\"” string to a elisp vector [r g b], where the values are from 0 to 1.

Note: The input string must NOT start with “#”."
  (vector
   (colortrans--normalize-rgb (string-to-number (substring rrggbb 0 2) 16))
   (colortrans--normalize-rgb (string-to-number (substring rrggbb 2 4) 16))
   (colortrans--normalize-rgb (string-to-number (substring rrggbb 4) 16))))

(defun colortrans-vec-rgb-to-vec-hsv (rgb_comp)
  "Convert a color reprensented by [r g b] vector RGB_COMP to
[h s v] hue saturation value vector."
  (vconcat (color-rgb-to-hsv
            (aref rgb_comp 0)
            (aref rgb_comp 1)
            (aref rgb_comp 2))))

(defconst colortrans--xtermlut
  `(("color-16" . ,(vector 0  0  0))
    ("color-17" . ,(vector 0  0  95))
    ("color-18" . ,(vector 0  0  135))
    ("color-19" . ,(vector 0  0  175))
    ("color-20" . ,(vector 0  0  215))
    ("color-21" . ,(vector 0  0  255))
    ("color-22" . ,(vector 0  95  0))
    ("color-23" . ,(vector 0  95  95))
    ("color-24" . ,(vector 0  95  135))
    ("color-25" . ,(vector 0  95  175))
    ("color-26" . ,(vector 0  95  215))
    ("color-27" . ,(vector 0  95  255))
    ("color-28" . ,(vector 0  135  0))
    ("color-29" . ,(vector 0  135  95))
    ("color-30" . ,(vector 0  135  135))
    ("color-31" . ,(vector 0  135  175))
    ("color-32" . ,(vector 0  135  215))
    ("color-33" . ,(vector 0  135  255))
    ("color-34" . ,(vector 0  175  0))
    ("color-35" . ,(vector 0  175  95))
    ("color-36" . ,(vector 0  175  135))
    ("color-37" . ,(vector 0  175  175))
    ("color-38" . ,(vector 0  175  215))
    ("color-39" . ,(vector 0  175  255))
    ("color-40" . ,(vector 0  215  0))
    ("color-41" . ,(vector 0  215  95))
    ("color-42" . ,(vector 0  215  135))
    ("color-43" . ,(vector 0  215  175))
    ("color-44" . ,(vector 0  215  215))
    ("color-45" . ,(vector 0  215  255))
    ("color-46" . ,(vector 0  255  0))
    ("color-47" . ,(vector 0  255  95))
    ("color-48" . ,(vector 0  255  135))
    ("color-49" . ,(vector 0  255  175))
    ("color-50" . ,(vector 0  255  215))
    ("color-51" . ,(vector 0  255  255))
    ("color-52" . ,(vector 95  0  0))
    ("color-53" . ,(vector 95  0  95))
    ("color-54" . ,(vector 95  0  135))
    ("color-55" . ,(vector 95  0  175))
    ("color-56" . ,(vector 95  0  215))
    ("color-57" . ,(vector 95  0  255))
    ("color-58" . ,(vector 95  95  0))
    ("color-59" . ,(vector 95  95  95))
    ("color-60" . ,(vector 95  95  135))
    ("color-61" . ,(vector 95  95  175))
    ("color-62" . ,(vector 95  95  215))
    ("color-63" . ,(vector 95  95  255))
    ("color-64" . ,(vector 95  135  0))
    ("color-65" . ,(vector 95  135  95))
    ("color-66" . ,(vector 95  135  135))
    ("color-67" . ,(vector 95  135  175))
    ("color-68" . ,(vector 95  135  215))
    ("color-69" . ,(vector 95  135  255))
    ("color-70" . ,(vector 95  175  0))
    ("color-71" . ,(vector 95  175  95))
    ("color-72" . ,(vector 95  175  135))
    ("color-73" . ,(vector 95  175  175))
    ("color-74" . ,(vector 95  175  215))
    ("color-75" . ,(vector 95  175  255))
    ("color-76" . ,(vector 95  215  0))
    ("color-77" . ,(vector 95  215  95))
    ("color-78" . ,(vector 95  215  135))
    ("color-79" . ,(vector 95  215  175))
    ("color-80" . ,(vector 95  215  215))
    ("color-81" . ,(vector 95  215  255))
    ("color-82" . ,(vector 95  255  0))
    ("color-83" . ,(vector 95  255  95))
    ("color-84" . ,(vector 95  255  135))
    ("color-85" . ,(vector 95  255  175))
    ("color-86" . ,(vector 95  255  215))
    ("color-87" . ,(vector 95  255  255))
    ("color-88" . ,(vector 135  0  0))
    ("color-89" . ,(vector 135  0  95))
    ("color-90" . ,(vector 135  0  135))
    ("color-91" . ,(vector 135  0  175))
    ("color-92" . ,(vector 135  0  215))
    ("color-93" . ,(vector 135  0  255))
    ("color-94" . ,(vector 135  95  0))
    ("color-95" . ,(vector 135  95  95))
    ("color-96" . ,(vector 135  95  135))
    ("color-97" . ,(vector 135  95  175))
    ("color-98" . ,(vector 135  95  215))
    ("color-99" . ,(vector 135  95  255))
    ("color-100" . ,(vector 135  135  0))
    ("color-101" . ,(vector 135  135  95))
    ("color-102" . ,(vector 135  135  135))
    ("color-103" . ,(vector 135  135  175))
    ("color-104" . ,(vector 135  135  215))
    ("color-105" . ,(vector 135  135  255))
    ("color-106" . ,(vector 135  175  0))
    ("color-107" . ,(vector 135  175  95))
    ("color-108" . ,(vector 135  175  135))
    ("color-109" . ,(vector 135  175  175))
    ("color-110" . ,(vector 135  175  215))
    ("color-111" . ,(vector 135  175  255))
    ("color-112" . ,(vector 135  215  0))
    ("color-113" . ,(vector 135  215  95))
    ("color-114" . ,(vector 135  215  135))
    ("color-115" . ,(vector 135  215  175))
    ("color-116" . ,(vector 135  215  215))
    ("color-117" . ,(vector 135  215  255))
    ("color-118" . ,(vector 135  255  0))
    ("color-119" . ,(vector 135  255  95))
    ("color-120" . ,(vector 135  255  135))
    ("color-121" . ,(vector 135  255  175))
    ("color-122" . ,(vector 135  255  215))
    ("color-123" . ,(vector 135  255  255))
    ("color-124" . ,(vector 175  0  0))
    ("color-125" . ,(vector 175  0  95))
    ("color-126" . ,(vector 175  0  135))
    ("color-127" . ,(vector 175  0  175))
    ("color-128" . ,(vector 175  0  215))
    ("color-129" . ,(vector 175  0  255))
    ("color-130" . ,(vector 175  95  0))
    ("color-131" . ,(vector 175  95  95))
    ("color-132" . ,(vector 175  95  135))
    ("color-133" . ,(vector 175  95  175))
    ("color-134" . ,(vector 175  95  215))
    ("color-135" . ,(vector 175  95  255))
    ("color-136" . ,(vector 175  135  0))
    ("color-137" . ,(vector 175  135  95))
    ("color-138" . ,(vector 175  135  135))
    ("color-139" . ,(vector 175  135  175))
    ("color-140" . ,(vector 175  135  215))
    ("color-141" . ,(vector 175  135  255))
    ("color-142" . ,(vector 175  175  0))
    ("color-143" . ,(vector 175  175  95))
    ("color-144" . ,(vector 175  175  135))
    ("color-145" . ,(vector 175  175  175))
    ("color-146" . ,(vector 175  175  215))
    ("color-147" . ,(vector 175  175  255))
    ("color-148" . ,(vector 175  215  0))
    ("color-149" . ,(vector 175  215  95))
    ("color-150" . ,(vector 175  215  135))
    ("color-151" . ,(vector 175  215  175))
    ("color-152" . ,(vector 175  215  215))
    ("color-153" . ,(vector 175  215  255))
    ("color-154" . ,(vector 175  255  0))
    ("color-155" . ,(vector 175  255  95))
    ("color-156" . ,(vector 175  255  135))
    ("color-157" . ,(vector 175  255  175))
    ("color-158" . ,(vector 175  255  215))
    ("color-159" . ,(vector 175  255  255))
    ("color-160" . ,(vector 215  0  0))
    ("color-161" . ,(vector 215  0  95))
    ("color-162" . ,(vector 215  0  135))
    ("color-163" . ,(vector 215  0  175))
    ("color-164" . ,(vector 215  0  215))
    ("color-165" . ,(vector 215  0  255))
    ("color-166" . ,(vector 215  95  0))
    ("color-167" . ,(vector 215  95  95))
    ("color-168" . ,(vector 215  95  135))
    ("color-169" . ,(vector 215  95  175))
    ("color-170" . ,(vector 215  95  215))
    ("color-171" . ,(vector 215  95  255))
    ("color-172" . ,(vector 215  135  0))
    ("color-173" . ,(vector 215  135  95))
    ("color-174" . ,(vector 215  135  135))
    ("color-175" . ,(vector 215  135  175))
    ("color-176" . ,(vector 215  135  215))
    ("color-177" . ,(vector 215  135  255))
    ("color-178" . ,(vector 215  175  0))
    ("color-179" . ,(vector 215  175  95))
    ("color-180" . ,(vector 215  175  135))
    ("color-181" . ,(vector 215  175  175))
    ("color-182" . ,(vector 215  175  215))
    ("color-183" . ,(vector 215  175  255))
    ("color-184" . ,(vector 215  215  0))
    ("color-185" . ,(vector 215  215  95))
    ("color-186" . ,(vector 215  215  135))
    ("color-187" . ,(vector 215  215  175))
    ("color-188" . ,(vector 215  215  215))
    ("color-189" . ,(vector 215  215  255))
    ("color-190" . ,(vector 215  255  0))
    ("color-191" . ,(vector 215  255  95))
    ("color-192" . ,(vector 215  255  135))
    ("color-193" . ,(vector 215  255  175))
    ("color-194" . ,(vector 215  255  215))
    ("color-195" . ,(vector 215  255  255))
    ("color-196" . ,(vector 255  0  0))
    ("color-197" . ,(vector 255  0  95))
    ("color-198" . ,(vector 255  0  135))
    ("color-199" . ,(vector 255  0  175))
    ("color-200" . ,(vector 255  0  215))
    ("color-201" . ,(vector 255  0  255))
    ("color-202" . ,(vector 255  95  0))
    ("color-203" . ,(vector 255  95  95))
    ("color-204" . ,(vector 255  95  135))
    ("color-205" . ,(vector 255  95  175))
    ("color-206" . ,(vector 255  95  215))
    ("color-207" . ,(vector 255  95  255))
    ("color-208" . ,(vector 255  135  0))
    ("color-209" . ,(vector 255  135  95))
    ("color-210" . ,(vector 255  135  135))
    ("color-211" . ,(vector 255  135  175))
    ("color-212" . ,(vector 255  135  215))
    ("color-213" . ,(vector 255  135  255))
    ("color-214" . ,(vector 255  175  0))
    ("color-215" . ,(vector 255  175  95))
    ("color-216" . ,(vector 255  175  135))
    ("color-217" . ,(vector 255  175  175))
    ("color-218" . ,(vector 255  175  215))
    ("color-219" . ,(vector 255  175  255))
    ("color-220" . ,(vector 255  215  0))
    ("color-221" . ,(vector 255  215  95))
    ("color-222" . ,(vector 255  215  135))
    ("color-223" . ,(vector 255  215  175))
    ("color-224" . ,(vector 255  215  215))
    ("color-225" . ,(vector 255  215  255))
    ("color-226" . ,(vector 255  255  0))
    ("color-227" . ,(vector 255  255  95))
    ("color-228" . ,(vector 255  255  135))
    ("color-229" . ,(vector 255  255  175))
    ("color-230" . ,(vector 255  255  215))
    ("color-231" . ,(vector 255  255  255))
    ;; Gray-scale range.
    ("color-232" . ,(vector 8 8 8))
    ("color-233" . ,(vector 18 18 18))
    ("color-234" . ,(vector 28 28 28))
    ("color-235" . ,(vector 38 38 38))
    ("color-236" . ,(vector 48 48 48))
    ("color-237" . ,(vector 58 58 58))
    ("color-238" . ,(vector 68 68 68))
    ("color-239" . ,(vector 78 78 78))
    ("color-240" . ,(vector 88 88 88))
    ("color-241" . ,(vector 98 98 98))
    ("color-242" . ,(vector 108 108 108))
    ("color-243" . ,(vector 118 118 118))
    ("color-244" . ,(vector 128 128 128))
    ("color-245" . ,(vector 138 138 138))
    ("color-246" . ,(vector 148 148 148))
    ("color-247" . ,(vector 158 158 158))
    ("color-248" . ,(vector 168 168 168))
    ("color-249" . ,(vector 178 178 178))
    ("color-250" . ,(vector 188 188 188))
    ("color-251" . ,(vector 198 198 198))
    ("color-252" . ,(vector 208 208 208))
    ("color-253" . ,(vector 218 218 218))
    ("color-254" . ,(vector 228 228 228))
    ("color-255" . ,(vector 238 238 238)))
  "Look-up table associating 256-color names to actual rgb colors.

Starts from color-16 as color-[0;15] are reserved for terminal colorscheme and
change depending on terminal configuration.")

(defconst colortrans--clut
  (cl-loop for (key . value) in colortrans--xtermlut
           collect (cons key (vconcat (mapcar 'colortrans--normalize-rgb value))))
  "Look-up table associating 256-color named to [0;1] normalized rgb colors.

Starts from color-16 as color-[0;15] are reserved for terminal colorscheme and
change depending on terminal configuration.")

(defun colortrans--closest-to-hex-in-alist (rrggbb candidates)
  "Returns the closest color to RRGGBB (in 24-bit hex string format)
in CANDIDATES.

CANDIDATES must be an alist with descriptive names as keys, and a vector
[r g b] as value (r g b in interval [0, 1]).

Note: RRGGBB must NOT start with a '#'"
  (let* ((hsv_target (colortrans-vec-rgb-to-vec-hsv
                      (colortrans-hex-to-vec rrggbb))))
    (--min-by
     (> (colortrans--hsv-distance hsv_target
                                  (colortrans-vec-rgb-to-vec-hsv (cdr it)))
        (colortrans--hsv-distance hsv_target
                                  (colortrans-vec-rgb-to-vec-hsv (cdr other))))
     candidates)))

(defun colortrans-hex-to-xterm (rrggbb)
  "Give color “\"rrggbb\"” string's closest xterm palette counterpart.

Note: The input string must NOT start with “#”."
  (interactive "sColor to translate (do _not_ include #): ")
  (let ((colortrans--matching-color
         (colortrans--closest-to-hex-in-alist rrggbb colortrans--clut)))
    (when (interactive-p)
      (message "Color matching %s is %s = %s" rrggbb (car colortrans--matching-color) (apply 'color-rgb-to-hex (append (cdr colortrans--matching-color) '(2))))
      )
    colortrans--matching-color))

(defun colortrans-hex-to-256-hex (rrggbb)
  "Give color “\"rrggbb\"” string's closest xterm palette counterpart in #rrggbb form.

Note: The input string must NOT start with “#”."
  (interactive "sColor to translate (do _not_ include #): ")
  (let* ((colortrans--matching-color
          (colortrans-hex-to-xterm rrggbb)))
    (colortrans--retval (apply 'color-rgb-to-hex (append (cdr colortrans--matching-color) '(2))))
    (when (interactive-p)
      (message "Color matching %s is %s = %s" rrggbb (car colortrans--matching-color) colortrans--retval)
      )
    colortrans--retval))

(defun colortrans-hex-to-named (rrggbb)
  "Give color “\"rrggbb\"” string's closest named color counterpart.

Note: The input string must NOT start with “#”."
  (interactive "sColor to translate (do _not_ include #): ")
  (let* ((named_colors (cl-loop for col_name in (defined-colors)
                                collect (cons col_name (vconcat (color-name-to-rgb col_name)))))
         (colortrans--matching-color
          (colortrans--closest-to-hex-in-alist rrggbb named_colors))
         )
    (when (interactive-p)
      (message "Color matching %s is %s = %s" rrggbb (car colortrans--matching-color) (apply 'color-rgb-to-hex (append (cdr colortrans--matching-color) '(2))))
      )
    colortrans--matching-color))

(defun colortrans-hex-to-named-name (rrggbb)
  "Give color “\"rrggbb\"” string's closest named color counterpart as the color's name.

Note: The input string must NOT start with “#”."
  (interactive "sColor to translate (do _not_ include #): ")
  (let* ((colortrans--matching-color (colortrans-hex-to-named rrggbb))

         (colortrans--retval (car colortrans--matching-color))
         )
    (when (interactive-p)
      (message "Color matching %s is %s = %s" rrggbb (car colortrans--matching-color) (apply 'color-rgb-to-hex (append (cdr colortrans--matching-color) '(2))))
      )
    colortrans--retval))

(defun colortrans-hex-to-named-hex (rrggbb)
  "Give color “\"rrggbb\"” string's closest named color counterpart as the color's hex string.

Note: The input string must NOT start with “#”."
  (interactive "sColor to translate (do _not_ include #): ")
  (let* ((colortrans--matching-color (colortrans-hex-to-named rrggbb))

         (colortrans--retval (apply 'color-rgb-to-hex (append (cdr colortrans--matching-color) '(2)))))
    (when (interactive-p)
      (message "Color matching %s is %s = %s" rrggbb (car colortrans--matching-color) (apply 'color-rgb-to-hex (append (cdr colortrans--matching-color) '(2))))
      )
    colortrans--retval))

(provide 'colortrans)

;;; colortrans.el ends here
