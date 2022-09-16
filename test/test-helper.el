;;; test-helper.el --- Helper for tests
;;
;; Copyright (C) 2022 Ellis Kenyő
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;  Test helpers
;;
;;; Code:

(require 'f)
(require 'buttercup)

(when (require 'undercover nil t)
  (undercover "*.el"
              (:exclude "*-test.el")))

(require 'exercism (f-expand "exercism.el" (f-parent (f-parent (f-this-file)))))

(provide 'test-helper)
;;; test-helper.el ends here
