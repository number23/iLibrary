;; Emacs Calender
;;
;; Emacs 中有日历，而且可以称之为一个系统，因为其中除了最常用的日历之外，
;; 还有其他的近十种历法，其中有日记、约会提醒、纪念日提示以及节假日提示等
;; 等。其中的历法包括中国的农历、希伯来历、伊斯兰历、法国革命历、中美玛雅
;; 历等等，可以根据经纬度告知你的所在的每天日出日落的时间等等。
;;
;; holiday-fixed m d   固定阳历节日， m 月 d 日
;; holiday-float m w n 浮动阳历节日， m 月的第 n 个星期 w%7
;;
;; ----------------------------------------------
;; .	跳回当前天
;; o	跳到某一个月
;; g d	跳到某年某月某日
;; g c	跳到某年某星期的星期几
;; g C	跳到阴历的某一天
;; p C	显示当前的阴历日期
;; h	显示当前节日
;; i d	加入当前这一天的日程安排
;; i w	加入每周这一天的日程安排
;; i m	加入每月这一天的日程安排
;; i y	加入每年这一天的日程安排
;; i a	加入周年纪念（anniversary），比如生日等
;; d	察看当前日期的diary
;; -----------------------------------------------
;;

(defun animals(birthyear)
  "Calculate the Chinese aninal by year"
  (let ((x (% (- 1997 birthyear) 12)))
    (cond ((or (= x 1) (= x -11))  "鼠")
          ((= x 0)                 "牛")
          ((or (= x 11) (= x -1))  "虎")
          ((or (= x 10) (= x -2))  "兔")
          ((or (= x 9) (= x -3))   "龙")
          ((or (= x 8) (= x -4))   "蛇")
          ((or (= x 7) (= x -5))   "马")
          ((or (= x 6) (= x -6))   "羊")
          ((or (= x 5) (= x -7))   "猴")
          ((or (= x 4) (= x -8))   "鸡")
          ((or (= x 3) (= x -9))   "狗")
          ((or (= x 2) (= x -10))  "猪")
          )
    )
  )

;; 保存日记的文件
(setq diary-file "~/.emacs.d/diary")
(setq diary-mail-addr "number23.cn@gmail.com")
(add-hook 'diary-hook 'appt-make-list)
;;appointment
(setq appt-issue-message t)

(setq calendar-remove-frame-by-deleting t)
(setq mark-holidays-in-calendar t)
(setq view-calendar-holidays-initially nil)

(setq holiday-general-holidays nil)
(setq christian-holidays nil)
(setq hebrew-holidays nil)
(setq islamic-holidays nil)
(setq solar-holidays nil)
(setq bahai-holidays nil)

(setq general-holidays '((holiday-fixed 1 1   "元旦")
                         (holiday-fixed 2 14  "情人节")
                         (holiday-fixed 4 1   "愚人节")
                         (holiday-fixed 12 25 "圣诞节")
                         (holiday-fixed 10 1  "国庆节")
                         (holiday-float 5 0 2 "母亲节")   ;5月的第二个星期天
                         (holiday-float 6 0 3 "父亲节")
                         ))

(setq local-holidays '((holiday-chinese 1 15  "元宵节 (正月十五)")
                       (holiday-chinese 5 5   "端午节 (五月初五)")
                       (holiday-chinese 9 9   "重阳节 (九月初九)")
                       (holiday-chinese 8 15  "中秋节 (八月十五)")
                       ;; 生日
                       (holiday-chinese 4 13 "老婆生日")
                       (holiday-chinese 11 15 "儿子生日")
                       ))

(setq chinese-calendar-celestial-stem
      ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])
(setq chinese-calendar-terrestrial-branch
      ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])

(setq mark-diary-entries-in-calendar t
      appt-issue-message nil
      mark-holidays-in-calendar t
      view-calendar-holidays-initially nil)

(setq diary-date-forms '((year "/" month "/" day "[^/0-9]"))
      calendar-date-display-form '(year "/" month "/" day)
      calendar-time-display-form
      '(24-hours ":" minutes (if time-zone " (") time-zone (if time-zone ")")))

(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

(autoload 'chinese-year "cal-china" "Chinese year data" t)

(defun holiday-chinese (cmonth cday string)
  "Chinese calendar holiday, month and day in Chinese calendar (CMONTH, CDAY).

If corresponding MONTH and DAY in gregorian calendar is visible,
the value returned is the list \(((MONTH DAY year) STRING)).
Returns nil if it is not visible in the current calendar window."
  (let* ((m displayed-month)
	 (y displayed-year)
	 (gdate (calendar-gregorian-from-absolute
		 (+ (cadr (assoc cmonth (chinese-year y))) (1- cday))))
	 (gm (car gdate))
	 (gd (cadr gdate))
	 (gy (caddr gdate)))
    (increment-calendar-month m y (- 11 gm))
    (if (> m 9)
        (list (list (list gm gd gy) string)))))

(defun birthday-fixed (month day string)
  "Holiday on MONTH, DAY (Gregorian) called STRING.
If MONTH, DAY is visible, the value returned is the list (((MONTH DAY year)
STRING)).  Returns nil if it is not visible in the current calendar window."
  (let ((m displayed-month)
        (y displayed-year)
        (animal (animals (string-to-number (nth 1 (split-string string "(")))))
        )
    (increment-calendar-month m y (- 11 month))
    (if (> m 9)
      (list (list (list month day y) string animal)))))

;; 设置颜色显示
(setq calendar-load-hook
      '(lambda ()
         (set-face-foreground 'diary-face   "skyblue")
         (set-face-background 'holiday-face "slate blue")
         (set-face-foreground 'holiday-face "white")))


(global-set-key (kbd "<f11>") 'calendar)
(provide 'calendar-setting)
