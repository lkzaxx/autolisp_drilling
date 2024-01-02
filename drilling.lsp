(defun C:dri () 
  (setvar "DIMZIN" 0) ;精度補0
  (setvar "osmode" 0)
  ;(setvar "cmdecho" 0;指令執行過程不回應
  ;---------------------副程式---------------------;

  ;;----------------------將每行各區排列成字串-----------------
  (defun readline () 
    (setq aa 0)
    (setq ok (list))
    (setq word "")
    (setq n 0)

    (while aa  ;;while
      (setq aa (read-char f))
      (cond 
        ((= aa 10)
         (setq aa nil) ;;LINE末端值=10
         (if (= n 1) 
           (progn 
             (setq ok (cons word OK))
             (setq word "")
             (setq n 0)
           )
         )
         (setq ok (reverse ok))
        )
        ((= aa 32) ;;空格=10
         (if (= n 1) 
           (progn 
             (setq ok (cons word OK))
             (setq word "")
             (setq n 0)
           )
         )
        )
        ((= aa 9) ;;TAB=9
         (if (= n 1) 
           (progn 
             (setq ok (cons word OK))
             (setq word "")
             (setq n 0)
           )
         )
        )
        ((= aa nil) (setq aa nil))
        ((setq word (strcat word (chr aa))) (setq n 1))
      )
    ) ;;while
    (princ)
  )
  ;;----------------------將每行各區排列成字串end-----------------

  ;;----------------------繪出鑽孔-------------------------------
  (defun drawdrilling (ok x) 
    (setq name (car ok))
    (setq soilnum (atoi (car (cdr ok))))
    (setq ninfo (atoi (car (cdr (cdr ok)))))
    (setq rqdinfo (atoi (car (cdr (cdr (cdr ok))))))
    (readline)
    (setq el (atof (car OK)))
    (setq maxv (atof (car OK)))
    (readline)
    (if (= (car ok) "non") 
      (readline)
      (progn 
        (setq watertime (car ok))
        (readline)
        (setq waterdepth (atof (car ok)))
        ;----------------gwater
        (setvar "clayer" "鑽孔文字")
        (command "-insert" 
                 "__water"
                 (strcat (rtos x 2 2) "," (rtos (- el (atof (car ok)))))
                 scale
                 scale
                 "0"
                 watertime
        )
      )
    )
    ;----------------title
    (setq titlecoordinate (strcat (rtos x 2 2) "," (rtos (+ el 0.5) 2 2)))
    (command "-insert" 
             "__title"
             titlecoordinate
             scale
             scale
             "0"
             name
             (strcat "EL." (rtos el 2 3))
    )
    ;----------------drawdrilling
    (setq retangletop el)
    (repeat soilnum 
      (readline)
      (setq ok (cdr ok))
      (setq soilname (car OK))
      (setq soilnamepic (strcat "__bor" soilname))
      (setq coordinate (strcat (rtos x 2 2) "," (rtos retangletop 2 2)))
      (setq soildepth (- el (atof (cadr OK))))
      (setq soildepthtext (atof (cadr OK)))
      (setq retangleleft (- x (* 5 scale)))
      (setq retangleright (+ x (* 5 scale)))
      (setq lefttop (strcat (rtos retangleleft 2 2) "," (rtos retangletop 2 2)))
      (setq rightdown (strcat (rtos retangleright 2 2) "," (rtos soildepth 2 2)))
      (setq textcoordinate (strcat (rtos x 2 2) "," (rtos soildepth 2 2)))
      (setvar "clayer" "鑽孔柱狀圖")
      (command "rectang" "w" "0" lefttop rightdown)
      (command "-insert" soilnamepic coordinate scale scale "0") ;圖塊插入 圖塊名 座標 X比例 Y比例 旋轉
      (command "xclip" "l" "" "n" "r" lefttop rightdown) ;圖塊縮小 l=上一次圖塊 ""=enter
      (setvar "clayer" "鑽孔文字")
      (command "-insert" 
               "__depth"
               textcoordinate
               scale
               scale
               "0"
               (rtos soildepthtext 2 3)
      )
      (setq retangletop soildepth)
    )
    (setq minv soildepth)
    (setq deep soildepth)
    (setq detextcoordinate (strcat (rtos x 2 2) "," (rtos (- deep 1) 2 2)))
    (command "-insert" "__depth" detextcoordinate scale scale "0" "DEPTH")
    (setq detextcoordinate (strcat (rtos x 2 2) "," (rtos (- deep 1.8) 2 2)))
    (command "-insert" "__depth" detextcoordinate scale scale "0" "(M)")
    (princ)
  )
  ;;----------------------繪出鑽孔end----------------------------
  ;;----------------------
  ;;----------------------鑽孔資料-------------------------------
  (defun drawdrillingdata () 
    (setvar "clayer" "鑽孔文字")
    (repeat ninfo 
      (readline)
      (setq n (caddr OK))
      (setq soildepth (- el (atof (cadr OK))))
      (setq textcoordinate (strcat (rtos x 2 2) "," (rtos soildepth 2 2)))
      (command "-insert" "__ntext" textcoordinate scale scale "0" n)
    )
    (setq ntextcoordinate (strcat (rtos x 2 2) "," (rtos (- deep 1) 2 2)))
    (command "-insert" "__ntext" ntextcoordinate scale scale "0" "(N)")

    (repeat rqdinfo 
      (readline)
      (setq rqd (caddr OK))
      (setq soildepth (- el (atof (cadr OK))))
      (setq rqdcoordinate (strcat (rtos x 2 2) "," (rtos soildepth 2 2)))
      (command "-insert" "__rqd" rqdcoordinate scale scale "0" rqd)
    )
    (setq rqtextcoordinate (strcat (rtos x 2 2) "," (rtos (- deep 1) 2 2)))
    (command "-insert" "__rqd" rqtextcoordinate scale scale "0" "RQD")
    (setq rqtextcoordinate (strcat (rtos x 2 2) "," (rtos (- deep 1.8) 2 2)))
    (command "-insert" "__rqd" rqtextcoordinate scale scale "0" "(%)")
    (princ)
  )
  ;;----------------------鑽孔資料end----------------------------
  ;;----------------------
  ;;----------------------圖框-------------------------------------
  (defun drawframe () 
    (setvar "clayer" "鑽孔圖框")
    (setq ruli 35) ;;尺規離柱之變數
    (setq litr 5) ;;小尺規距離變數
    (setq maxf (fix maxh)) ;;變整數
    (setq minf (fix minh)) ;;變整數
    (setq rulermx (* disx (- nx 1)))
    (setq rulermx (fix (+ rulermx (* (* ruli 2) scale))))
    (while (/= (rem maxf 10) 0) 
      (setq maxf (+ maxf 1))
    )
    (while (/= (rem minf 10) 0) 
      (setq minf (- minf 1))
    )
    (if (< (abs (- maxf maxh)) 4) 
      (setq maxf (+ maxf 10))
    )
    (if (< (abs (- minf minh)) 4) 
      (setq minf (- minf 10))
    )

    (while (/= (rem rulermx 5) 0) 
      (setq rulermx (+ rulermx 1))
    )
    (setq rulernum (fix (/ rulermx 5)))
    ;;-----------尺規
    ;;y軸
    (setq zerocoor (strcat (rtos (- glx (* ruli scale)) 2 2) "," (rtos minf 2 2)))
    (setq zerocoor2 (strcat (rtos (- glx (* ruli scale)) 2 2) "," (rtos maxf 2 2)))
    (command "line" zerocoor zerocoor2 "")
    (setq zerocoor (strcat (rtos (+ (- glx (* ruli scale)) rulermx) 2 2) 
                           ","
                           (rtos minf 2 2)
                   )
    )
    (setq zerocoor2 (strcat (rtos (+ (- glx (* ruli scale)) rulermx) 2 2) 
                            ","
                            (rtos maxf 2 2)
                    )
    )
    (command "line" zerocoor zerocoor2 "") ;;尾端Y軸
    ;;x軸
    (setq zerocoor (strcat (rtos (- glx (* ruli scale)) 2 2) "," (rtos minf 2 2)))
    (setq zerocoor2 (strcat (rtos (+ (- glx (* ruli scale)) rulermx) 2 2) 
                            ","
                            (rtos minf 2 2)
                    )
    )
    (setq zerocoor3 (strcat (rtos (- glx (* ruli scale)) 2 2) 
                            ","
                            (rtos (+ 10 minf) 2 2)
                    )
    )
    (command "line" zerocoor zerocoor2 "")
    (command "copy" "l" "" zerocoor "A" (+ (/ (- maxf minf) 10) 1) zerocoor3) ;;每10m一條線
    ;;y小尺規
    (setq zerocoor (strcat (rtos (- glx (* ruli scale)) 2 2) "," (rtos minf 2 2)))
    (setq zerocoor2 (strcat (rtos (- glx (+ (* litr scale) (* ruli scale))) 2 2) 
                            ","
                            (rtos minf 2 2)
                    )
    )
    (setq zerocoor3 (strcat (rtos (- glx (* ruli scale)) 2 2) 
                            ","
                            (rtos (+ 5 minf) 2 2)
                    )
    )
    (command "line" zerocoor zerocoor2 "")
    (command "copy" "l" "" zerocoor "A" (+ (/ (- maxf minf) 5) 1) zerocoor3)
    ;;x小尺規
    (setq zerocoor (strcat (rtos (- glx (* ruli scale)) 2 2) "," (rtos minf 2 2)))
    (setq zerocoor2 (strcat (rtos (- glx (* ruli scale)) 2 2) 
                            ","
                            (rtos (- minf (* litr scale)) 2 2)
                    )
    )
    (setq zerocoor3 (strcat (rtos (+ 5 (- glx (* ruli scale))) 2 2) 
                            ","
                            (rtos minf 2 2)
                    )
    )
    (command "line" zerocoor zerocoor2 "")
    (command "copy" "l" "" zerocoor "A" (+ rulernum 1) zerocoor3)
    ;;y軸文字
    (setq ty minf)
    (while (<= ty maxf) 
      (setq rqtextcoordinate (strcat (rtos (- glx (* ruli scale)) 2 2) 
                                     ","
                                     (rtos ty 2 2)
                             )
      )
      (command "-insert" "__ruler" rqtextcoordinate scale scale "0" ty "")
      (setq ty (+ ty 10))
    )
    ;;x軸文字
    (setq tx (- glx (* ruli scale)))
    (setq txx 0)
    (while (<= tx (+ (- glx (* ruli scale)) rulermx)) 
      (setq rqtextcoordinate (strcat (rtos tx 2 2) 
                                     ","
                                     (rtos (- minf (* litr scale)) 2 2)
                             )
      )
      (command "-insert" "__ruler" rqtextcoordinate scale scale "0" "" txx)
      (setq tx  (+ tx 10)
            txx (+ txx 10)
      )
    )
    ;;-----------圖框
    (setq fraco 35) ;;圖框留白係數
    (setq frab 1) ;;圖框寬度係數
    (setq signh 3) ;;字型高度係數 簽名上方線所需寬度: (3/2線寬+字寬)
    (setq maxf (+ maxf (* fraco scale)))
    (setq minf (- minf (* fraco scale)))
    (setq retangleleft (- (- glx (* ruli scale)) (* fraco scale)))
    (setq retangleright (+ (+ (- glx (* ruli scale)) rulermx) (* fraco scale)))
    (setq lefttop (strcat (rtos retangleleft 2 2) "," (rtos maxf 2 2)))
    (setq rightdown (strcat (rtos retangleright 2 2) "," (rtos minf 2 2)))
    (command "rectang" "w" (* frab scale) lefttop rightdown)
    (command "-insert" "__sign" rightdown scale scale "0" "聯興工程顧問有限公司")
    (setq lefttop (strcat (rtos retangleleft 2 2) 
                          ","
                          (rtos (+ (* signh scale) minf (* frab 1)) 2 2)
                  )
    )
    (setq rightdown (strcat (rtos retangleright 2 2) 
                            ","
                            (rtos (+ (* signh scale) minf (* frab 1)) 2 2)
                    )
    )
    (command "pline" lefttop "w" (* frab scale) (* frab scale) rightdown "")
    (princ)
  )
  ;;----------------------圖框end----------------------------------
  ;;----------------------
  ;;----------------------鑽孔全-----------------------------------
  (defun drillingall (glx) 
    (readline)
    (if (= ok nil) 
      (setq nok 1)
      (setq nok 0)
    )
    (setq disx (* 45 scale))
    (setq nx 0)
    (while ok  ;while
      (setq x (+ glx (* nx disx)))
      (drawdrilling ok x)
      ;;一排10個鑽孔
      (if (= nx 0) 
        (progn 
          (setq maxh el)
          (setq minh el)
        )
      )
      (if (> maxv maxh) 
        (setq maxh maxv)
      )
      (if (< minv minh) 
        (setq minh minv)
      )
      (drawdrillingdata)
      (setq nx (+ nx 1))
      (if (= nx 10) 
        (setq ok nil)
        (readline)
      )
    ) ;;while
    ;--------繪出圖框
    (if (= nok 0) 
      (drawframe)
    )
    ;--------繪出圖框
    (princ)
  )
  ;;----------------------鑽孔全end--------------------------------


  ;---------------------副程式end------------------;
  ;-------------------------------------------------
  ;-------------------------------------------------
  ;---------------------主程式---------------------;

  (setq f (open (getfiled "開啟檔案" "d:/" "txt" 2) "r"))
  ;(setq f (open "d:/drilling.txt" "r"))
  (setq glx 0) ;設定全域座標
  (setq x 0) ;設定變動座標
  (setq y 0) ;設定變動座標
  (setq width 3) ;原鑽孔寬=10
  (setq scale (* width 0.1)) ;計算縮放比例

  (command ".-layer" "n" "鑽孔文字" "c" "7" "鑽孔文字" "") ;;建立鑽孔文字圖層
  (command ".-layer" "n" "鑽孔柱狀圖" "c" "7" "鑽孔柱狀圖" "") ;;建立鑽孔柱狀圖圖層
  (command ".-layer" "n" "鑽孔圖框" "c" "7" "鑽孔圖框" "") ;;建立鑽孔圖框圖層

  ;--------繪出鑽孔
  (drillingall glx)


  (while (= nx 10) 
    (setq glx (+ glx (- retangleright retangleleft)))
    ;--------繪出鑽孔
    (drillingall glx)
  )

  ;);指令執行過程不回應
  (close f)
  (setvar "osmode" 16383) ;;恢復鎖點
  (princ)
  ;---------------------主程式end------------------;
)