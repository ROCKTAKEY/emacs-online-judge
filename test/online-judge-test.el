;;; online-judge-test.el --- test for online-judge

;; Copyright (C) 2019  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords:

;; Version: 0.0.0
;; Package-Requires:
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

;;; Commentary:

;;

;;; Code:

(ert-deftest atcoder-abc020-or-more ()
  (let ((online-judge--set t)
        (online-judge--host 'atcoder)
        (online-judge--contest "abc020")
        (online-judge--problem "a"))
    (should
     (string= (online-judge--atcoder-url)
              "https://atcoder.jp/contests/abc020/tasks/abc020_a"))))

(ert-deftest atcoder-abc019-or-less ()
  (let ((online-judge--set t)
        (online-judge--host 'atcoder)
        (online-judge--contest "abc019")
        (online-judge--problem "a"))
    (should
     (string= (online-judge--atcoder-url)
              "https://atcoder.jp/contests/abc019/tasks/abc019_1"))))

(ert-deftest atcoder-arc035-or-more ()
  (let ((online-judge--set t)
        (online-judge--host 'atcoder)
        (online-judge--contest "arc035")
        (online-judge--problem "a"))
    (should
     (string= (online-judge--atcoder-url)
              "https://atcoder.jp/contests/arc035/tasks/arc035_a"))))

(ert-deftest atcoder-arc034-or-less ()
  (let ((online-judge--set t)
        (online-judge--host 'atcoder)
        (online-judge--contest "arc034")
        (online-judge--problem "a"))
    (should
     (string= (online-judge--atcoder-url)
              "https://atcoder.jp/contests/arc034/tasks/arc034_1"))))

(ert-deftest atcoder-chokudai001 ()
  (let ((online-judge--set t)
        (online-judge--host 'atcoder)
        (online-judge--contest "chokudai001")
        (online-judge--problem "a"))
    (should
     (string= (online-judge--atcoder-url)
              "https://atcoder.jp/contests/chokudai001/tasks/chokudai_001_a"))))

(ert-deftest atcoder-chokudai-others ()
  (let ((online-judge--set t)
        (online-judge--host 'atcoder)
        (online-judge--contest "chokudai002")
        (online-judge--problem "a"))
    (should
     (string= (online-judge--atcoder-url)
              "https://atcoder.jp/contests/chokudai002/tasks/chokudai002_a"))))

(provide 'online-judge-test)
;;; online-judge-test.el ends here
