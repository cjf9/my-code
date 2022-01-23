

; ulm | useless-library-for-math

; 第2版
; 最后改于2021年10月







;                                      ---  说明  ---
;
;
; 1 
; 2 
;   （1）
;   （2）函数名有时用过去分词代表名词，比如culled 代表culled matrix，指被cull掉某些行列之后留下的子矩阵。
;       
;   （3）
;   （4）
; 3 
;   （1）用返回值而非副作用。
;   （2）
;    (3) 在有必要时，仍用lst-, int-等前缀标明该变量的类型
; 4 
;   （1） 定义matrix为表中表，一般的矩阵长这样：
;
;                         ((1 2 3)
;                          (4 5 6)
;                          (7 8 9))
;       
;       列矩阵长这样：
;                         ((1)
;                          (2)
;                          (3))
;          
; 5 所有运算基于矩阵完成。不能定义对象为行列式，只能定义与之长相相同的矩阵A，然后通过(det A)等方式操作。








;                              ---   已实现函数表  ---
;
; 第一部分：
; 1 (bubble-sort predicate lst)      对列表lst，以谓词predicate进行排序
; 2 (quick-sort predicate lst)       （ulm里只在matrix?里需排序，但尚未改用quick-sort，可自己改）
;
;
; 第二部分：
; 1  (matrix? M)                     判断M是否为上面定义的矩阵
; 2  (mprint M)                      以直观的样子打印矩阵M
; 3  (culled M i j)                  得到M剔除i行j列后的子矩阵（一般取子矩阵都是留下取中的，这个后面也实现了即submatrix）
; 4  (det M)                         得到方阵M的行列式|M|的数值
; 5  (cofactor M i j)                取矩阵M对应行列式在(i,j)元的余子式
;    (M-ij M i j)                    M-ij是cofactor的别名，已绑定在一起
; 6  (algeraic-cofactor M i j)       求M在(i,j)的代数余子式
;    (A-ij M i j)                    A-ij是algebraic-cofactor的别名
; 7  (diag e1 e2 e3 ...)             生成e1,e2...为元素的对角矩阵。我靠这个没做好，应该把参数做成表的，要不怎么用
; 8  (scalar-matrix n obj)           生成n维obj的纯量矩阵，单位矩阵是其特例
; 9  (mget M i j)                    取得矩阵(i,j)元
; 10 (mreplace M i j obj)            将矩阵M的(i,j)元替换为obj,返回替换后的矩阵（前面说过了，Ulm尽量用返回值）
; 11 (mplus M1 M2)                   矩阵加法
; 12 (ktimes k M)                    以k数乘矩阵M
; 13 (mtimes M1 M2)                  矩阵乘法
; 14 (mpower M k)                    方针M的k次幂
; 15 (transpose M)                   矩阵M转置（若求转置行列式，无非是(det (transpose M))）
; 16 (cofactor-matrix M)             余子矩阵(就是每个元素的cofactor对应的矩阵，与伴随矩阵互为转置)
;    (cof M)                         余子矩阵的别名
; 17 (adjoint-matrix M)              伴随矩阵
;    (adj M)                         伴随矩阵的别名
; 18 (inverse M)                     逆矩阵（不要故意喂奇异矩阵给它，错误是未推导的）
; 19 (submatrix M lst-row lst-column)后面两个参数是两个表，指明你希望留下的行和列，据此取得子矩阵返回
; 20 (minor M lst-row lst-column)    求M对应的行列式的，指定行列的k阶子式
; 21 (row-ecchange M r1 r2)          交换M的r1,r2两行
; 22 (augmentd-matrix A b)           合并A,b为一个增广矩阵
; 23 (trace M)                       求矩阵M的迹
;    (tr M)                          trace的别名
; 24 (solve-equation A b)            解简单的线性方程组（用户自己把多余的方程丢了）








;                                     ---   代码  ---


; 一、一些用作依赖的函数：

; 1 
; 冒泡排序 
(define (bubble-sort predicate list1)
  (define vec (list->vector list1))
  (define tmp 0) 
  (let loop ((coordinate-end (- (length list1) 1)))
    (if (> coordinate-end 0)
        (let loop2 ((coordinate 0))
          (if (< coordinate coordinate-end)
              (if (predicate (vector-ref vec coordinate) (vector-ref vec (+ coordinate 1))) ;“不稳定”
                  (loop2 (+ coordinate 1))
                  (begin
                    (set! tmp (vector-ref vec coordinate))
                    (vector-set! vec coordinate (vector-ref vec (+ coordinate 1)))
                    (vector-set! vec (+ coordinate 1) tmp)
                    (loop2 (+ coordinate 1))))
              (loop (- coordinate-end 1))))
         (vector->list vec))))


; 2
; 快速排序
(define (quick-sort predicate lst)
  (define (side-effect-iter v start end)
    (if (not (>= start end))
        (let ((pivot (vector-ref v start)))
          (let loop ((p1 start) (p2 end) (status 2))
            (if (= p1 p2)  
                (begin (vector-set! v p1 pivot)
                       (if (> p1 0)
                           (side-effect-iter v start (- p1 1)))
                       (if (< p1 end)
                           (side-effect-iter v (+ p1 1) end))) 
                (cond 
                  ((= status 2) (if (predicate pivot (vector-ref v p2))
                                    (loop p1 (- p2 1) 2)
                                    (begin (vector-set! v p1 (vector-ref v p2))
                                           (loop (+ p1 1) p2 1))))
                  ((= status 1) (if (predicate (vector-ref v p1) pivot)
                                    (loop (+ p1 1) p2 1)
                                    (begin (vector-set! v p2 (vector-ref v p1))
                                           (loop p1 (- p2 1) 2))))))))))  
  (let ((v (list->vector lst)))
    (side-effect-iter v 0 (- (vector-length v) 1))
    (vector->list v)))   





; 二、Linear algebra 

; 1
; 判断是否本文所定义的矩阵，也就是表中表且内表里全是数字。
; 依赖于bubble-sort
(define (matrix? M)
  (define (list-in-list? L) 
    (let ((jduge (apply + (map (lambda (x) (if (list? x) 0 1)) L))))
      (if (= jduge 0)
          #t
          #f)))
  (define (all-number? L)
    (let ((jduge (apply + (map (lambda (x) (if (number? x) 0 1)) L))))
      (if (= jduge 0)
          #t
          #f)))
  (define (all-number-in-inner-list? L)
    (let ((jduge (apply + (map (lambda (x) (if (equal? x #t) 0 1)) (map all-number? L)))))
      (if (= jduge 0)
          #t
          #f)))
  (define (get-row-length L) (map (lambda (x) (length x)) L))
  ;开始判断
  (if (list? M)
      (if (list-in-list? M)
          (let ((row-length (bubble-sort < (get-row-length M))))
            ;判断每行长度是否一样
            (if (= (car row-length) (car (list-tail row-length (- (length row-length) 1)))) 
                (if (all-number-in-inner-list? M)
                    #t
                    #f)
                #f))
          #f)
      #f))
; 当年思路之混乱！这里要想把代码写清楚，应该是用一个and, 所有条件满足返回#t, 或者用or，后面的不用计算更快。



; 2
; 按照肉眼习惯将矩阵打印到屏幕端口
; 依赖matrix?
(define (mprint M)
  (if (matrix? M)
      (let loop ((mtmp M) (cnt 1))
        (if (null? mtmp)
            (newline)
            (begin
              (display "row")
              (display cnt)
              (display "-> ")
              (for-each (lambda (x) (display x) (display #\space) x) (car mtmp))
              (newline)
              (loop (cdr mtmp) (+ cnt 1)))))
      (display "这不是一个矩阵！(Not a matrix! )\n")))



; 3
; 剔除矩阵M第i行和j列，得到新矩阵
; tmd当时我为啥要把变量名命成这个样子，真想拿键盘拍死那个我
(define (culled M i j)
  ;为了后面好写(用map),做一个剔除表中第k元素的过程
  (define (culling-k k L)
    (let loop ((cnt 1) (LT L))
      (if (< cnt k)
          (cons
           (car LT)
           (loop (+ cnt 1) (cdr LT)))
          (cdr LT))))
    (let ((M-without-row-i
            (let loop ((cnt-i 1) (MTT M))
              (if (< cnt-i i)
                  (cons
                    (car MTT)
                    (loop (+ cnt-i 1) (cdr MTT)))
                  (cdr MTT)))))
         (map (lambda (L) (culling-k j L)) M-without-row-i)))



; 4
; 求取方阵M对应的行列式|M|
; 依赖于culled
(define (det M)
  ;求二阶行列式,为了求高阶行列式(代数余子式展开)做递归而准备
  (define (det-2 D)
    (- (* (caar D) (cadadr D)) (* (cadar D) (caadr D))))
  ;开始求值
  (let ((order (length M)))
    (cond ((= order 1) (caar M))
          ((= order 2) (det-2 M))
          ;约定总是按第一行的代数余子式展开
          (else (let expansion-loop ((cnt 1) (row-1 (car M)))
                  (if (<= cnt order)
                      (+ (* (expt -1 (+ cnt 1)) (car row-1) (det (culled M 1 cnt)))
                         (expansion-loop (+ cnt 1) (cdr row-1)))
                      0))))))


; 5
; 求矩阵M对应的行列式关于(i, j)元的余子式（只有矩阵一个数据类型，所以这实际上是从方阵到对应行列式的余子式）
; 依赖于det 和 culled
(define (cofactor M i j)
  (det (culled M i j)))

; 给cofactor取个别名，叫M-ij
(define M-ij cofactor)



; 6
; 求矩阵M对应的行列式关于(i, j)元的代数余子式（只有矩阵一个数据类型，所以这实际上是从方阵到对应行列式的代数余子式）
; 依赖于cofactor 
(define (algebraic-cofactor M i j)
  (* (expt -1 (+ i j)) (cofactor M i j)))

; 给algebraic-cofactor取个别名，叫A-ij
(define A-ij algebraic-cofactor)



; 7
; 由 (diag e1 e2 e3 ...) 生成对应的对角矩阵（diagonal matrix）
(define diag
  (lambda args
    (let ((dim (length args)))
      (let loop ((mtmp args) (ptr 1))
        (if (null? mtmp)
            '()
            (cons (let loop2 ((cnt 1))
                    (if (<= cnt dim)
                        (cons (if (= cnt ptr)
                                  (car mtmp)
                                  0)
                              (loop2 (+ cnt 1)))
                        '()))
                  (loop (cdr mtmp) (+ ptr 1))))))))


; 8
; 生成以obj为对角线元素的n阶纯量矩阵（标量矩阵）（单位矩阵是其特例）
(define (scalar-matrix n obj)
  (let loop ((ptr 1))
    (if (<= ptr n)
        (cons (let loop2 ((cnt 1))
                (if (<= cnt n)
                    (cons (if (= cnt ptr)
                              obj
                              0)
                          (loop2 (+ cnt 1)))
                    '()))
              (loop (+ ptr 1)))
        '())))



; 9
; 取得矩阵某元素
(define (mget M i j)
  (let loop-i ((cnt-i i) (use-whose-first-row M))
    (if (= cnt-i 1)
        (let loop-j ((cnt-j j) (the-row (car use-whose-first-row)))
          (if (= cnt-j 1)
              (car the-row)
              (loop-j (- cnt-j 1) (cdr the-row))))
        (loop-i (- cnt-i 1) (cdr use-whose-first-row)))))



; 10
; 替换矩阵某元素
(define (mreplace M i j obj)
  (let loop-i ((cnt-i 1) (tmp-to-change-whose-first-row M))
    (if (< cnt-i i)
        (cons (car tmp-to-change-whose-first-row)
              (loop-i (+ cnt-i 1) (cdr tmp-to-change-whose-first-row)))
        (cons (let loop-j ((cnt-j 1) (the-row (car tmp-to-change-whose-first-row)))
                    (if (< cnt-j j)
                        (cons (car the-row)
                              (loop-j (+ cnt-j 1) (cdr the-row)))
                        (cons obj (cdr the-row)))) 
              (cdr tmp-to-change-whose-first-row)))))



; 11
; 矩阵加法
; 依赖于mget
(define (mplus M1 M2)
  (define row (length M1))
  (define column (length (car M1)))
  (let loop ((cnt-i 1))
    (if (<= cnt-i row)
        (cons (let loop2 ((cnt-j 1))
                (if (<= cnt-j column)
                    (cons (+ (mget M1 cnt-i cnt-j) (mget M2 cnt-i cnt-j))
                          (loop2 (+ cnt-j 1)))
                    '()))
              (loop (+ cnt-i 1)))
        '())))



; 12
; 矩阵数乘, 即将矩阵每个数字乘以一个常数k
; 依赖于mget
(define (ktimes k M)
  (define row (length M))
  (define column (length (car M)))
  (let loop ((cnt-i 1))
    (if (<= cnt-i row)
        (cons (let loop2 ((cnt-j 1))
                (if (<= cnt-j column)
                    (cons (* k (mget M cnt-i cnt-j))
                          (loop2 (+ cnt-j 1)))
                    '()))
              (loop (+ cnt-i 1)))
        '())))



; 13
; 矩阵乘法
; 依赖于mget, 
(define (mtimes M1 M2)
  (define a (length M1))       ;a行b列之a
  (define b (length (car M1))) ;a行b列之b
  (define c (length (car M2))) ;b行c列之c
  ;先做个生成第(i,j)元素的函数
  (define (make i j)
    (let loop ((cnt 1))
      (if (<= cnt b)
          (+ (* (mget M1 i cnt) (mget M2 cnt j)) (loop (+ cnt 1)))
          0)))
  (let loop-i ((cnt-i 1))
    (if (<= cnt-i a)
        (cons (let loop-j ((cnt-j 1))
                (if (<= cnt-j c)
                    (cons (make cnt-i cnt-j)
                          (loop-j (+ cnt-j 1)))
                    '()))
              (loop-i (+ cnt-i 1)))
        '())))


; 14
; 方阵的幂
; 依赖于mtimes
(define (mpower M k)
  (if (= k 1)
      M
      (mtimes M (mpower M (- k 1)))))    
    

; 15
; 转置
; 依赖于mget
(define (transpose M)
  (define row (length M))
  (define column (length (car M)))
  (let loop-j ((cnt-j 1))
    (if (<= cnt-j column)
        (cons (let loop-i ((cnt-i 1))
                (if (<= cnt-i row)
                    (cons (mget M cnt-i cnt-j)
                          (loop-i (+ cnt-i 1)))
                    '()))
              (loop-j (+ cnt-j 1)))
        '())))



; 16
; 求余子矩阵（代数余子式又称余因子，其构成余因子矩阵，简称余子矩阵，余子阵，和伴随矩阵是转置关系）
; 依赖于algrbraic-cofactor
; 查到多个翻译，我觉得既然余子式是cofactor，cofactor-matrix比较恰当
; （此处用到的翻译是93年数学名词标准，余子式cofactor, 代数余子式algebraic factor, 子式用minor, 正在开始看英语书，发现错误再来改吧哎）
(define (cofactor-matrix M)
  (define row (length M))
  (define column (length (car M)))
   (let loop-i ((cnt-i 1))
      (if (<= cnt-i row)
          (cons (let loop-j ((cnt-j 1))
                  (if (<= cnt-j column)
                      (cons (algebraic-cofactor M cnt-i cnt-j)
                            (loop-j (+ cnt-j 1)))
                      '()))
                (loop-i (+ cnt-i 1)))
          '())))

; 给余子矩阵取个别名
(define cof cofactor-matrix)



; 17
; 求伴随矩阵（adjoint matrix）
; 依赖于cofactor-matrix, 和transpose
(define (adjoint-matrix M)
  (transpose (cofactor-matrix M)))

; 给伴随矩阵取个别名
(define adj adjoint-matrix)




; 18
; 求逆矩阵（inverse matrix）
; 依赖于adjiont-matrix, ktimes, 还有det
(define (inverse M)
  (ktimes (/ 1 (det M))
          (adjoint-matrix M)))


; 19
; 取得子矩阵(submatrix)
; lst-row 与 lst-column是两个表，指明要取哪些行和列
; 依赖于mget
(define (submatrix M lst-row lst-column)
  (let loop-i ((row-tmp lst-row))
    (if (null? row-tmp)
        '()
        (cons (let ((cnt-i (car row-tmp)))
                (let loop-j ((column-tmp lst-column))
                  (if (null? column-tmp)
                      '()
                      (cons (let ((cnt-j (car column-tmp)))
                              (mget M cnt-i cnt-j))
                            (loop-j (cdr column-tmp))))))
              (loop-i (cdr row-tmp))))))


; 20
; 求指定行列的k阶子式
; 同submatrix，用两个表指明行列
; 依赖于submatrix 和 det
(define (minor M lst-row lst-column)
  (det (submatrix M lst-row lst-column)))




; 21
; 交换矩阵两行
(define (row-exchange M r1 r2)
  (define (row-get M r)  ;临时定义一个用来取得某行的函数，但这个取出来是list
    (if (= r 1)
        (car M)
        (row-get (cdr M) (- r 1))))
  (let ((x1 (if (<= r1 r2) r1 r2)) (x2 (if (> r1 r2) r1 r2)))
    (let ((r1-tmp (row-get M x1))) ;在脑子里把整个递归计算的序列展开,就会很清晰
      (let loop ((MT M) (cnt-r 1))
        (if (< cnt-r x1)
            (cons (car MT) (loop (cdr MT) (+ cnt-r 1)))
              (if (= cnt-r x1)
                  (cons (row-get M x2) (loop (cdr MT) (+ cnt-r 1)))
                  (if (< cnt-r x2)
                      (cons (car MT) (loop (cdr MT) (+ cnt-r 1)))
                      (if (= cnt-r x2)
                          (cons r1-tmp (loop (cdr MT) (+ cnt-r 1)))
                          MT))))))))


; 22
; 求增广矩阵（augmented matrix）
(define (augmented-matrix A B)
  (map (lambda (x y) (append x y)) A B))


; 23
; 求矩阵的迹（trace）
; 依赖于mget
(define (trace M)
  (let ((row (length M)))
    (let loop ((cnt 1))
      (if (<= cnt row)
          (+ (mget M cnt cnt) (loop (+ cnt 1)))
          0))))

; 给迹取个别名
(define tr trace)



; 24
; 解线性方程组，给出解向量(diag x1 x2 x3... xn)（用户需要自己把多余的方程扔掉）
; 依赖于mreplace, det
; 暂时只做唯一解方程的情况（用Cramer's rule），因为多解的基础解系要用符号表示，将来如果闲的。。。算了不会有那个闲了
(define (solve-equation A b)
  (define (Dj-matrix j A b)  ;定义获取Dj对应矩阵的函数
    (let ((dim (length A)))
      (let loop ((mtmp A) (i 1) (mtmp2 b))
        (if (<= i dim)
            (loop (mreplace mtmp i j (caar mtmp2)) (+ i 1) (cdr mtmp2))
            mtmp))))
  (let ((dim (length A)))
    (let loop ((cnt 1))
      (if (<= cnt dim)
          (cons (list (/ (det (Dj-matrix cnt A b)) (det A)))
                (loop (+ cnt 1)))
          '()))))


