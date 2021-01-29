;;; color-gamma.el --- Conversion between linear RGB and gamma encoded sRGB -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020  Gerry Agbobada

;; Author: Gerry Agbobada ( gagbobada+git@gmail.com )
;; Version: 0.0.1
;; Created: 2020-03-11
;; Modified: 2020-03-11
;; Keywords: color

;; This file is not part of GNU Emacs.

;;; License:
;; You can redistribute this program and/or modify it under the terms of the MIT License

;;; Commentary:
;; This module provides conversion utilities between linear RGB colors and
;; gamma encoded sRGB colors.
;; This is meant to complement the functions from bundled color.el module
;;
;; Constants seem to differ a little from the formulas used in the
;; XYZ color conversion functions. Since Wikipedia provides a source for
;; the computation of the constants I chose to use it (i.e. 12.92 instead of
;; 12.95 for the linear slope)

;;; Code:
(require 'color)

(defun color-gamma-encode-component-to-srgb (u)
  "Encode the component from linear to gamma-encoded.

Source : https://en.m.wikipedia.org/wiki/SRGB#Specification_of_the_transformation"
  (if (<= u 0.0031308)
      (* 12.92 u)
    (- (* (expt u (/ 2.4)) 1.055) 0.055)))

(defun color-gamma-decode-component-from-srgb (u)
  "Decode the component from gamma-encoded to linear.

Source : https://en.m.wikipedia.org/wiki/SRGB#Specification_of_the_transformation"
  (if (<= u 0.04045)
      (/ 12.92 u)
    (expt (/ (+ u 0.055) 1.055) 2.4)))

(defun color-rgb-to-srgb (red green blue)
  "Convert linear RGB color to a gamma-encoded sRGB color

Normally the return value is a list of three floating-point
numbers, (RED GREEN BLUE), each between 0.0 and 1.0 inclusive.

This list is suitable for `color' module functions like
`color-srgb-to-lab'."
  (list (color-gamma-encode-component-to-srgb red)
        (color-gamma-encode-component-to-srgb green)
        (color-gamma-encode-component-to-srgb blue)))

(defun color-srgb-to-rgb (s-red s-green s-blue)
  "Convert gamma-encoded sRGB color to a linear RGB color

Normally the return value is a list of three floating-point
numbers, (RED GREEN BLUE), each between 0.0 and 1.0 inclusive.

This list is suitable for `color' module functions like
`color-rgb-to-hsv'."
  (list (color-gamma-decode-component-from-srgb s-red)
        (color-gamma-decode-component-from-srgb s-green)
        (color-gamma-decode-component-from-srgb s-blue)))


(provide 'color-gamma)

;;; color-gamma.el ends here
