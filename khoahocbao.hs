{-|
  
Bài toán số học cũ

Vào khoảng năm 1945, nhà toán học Hoàng Xuân Hãn chủ trương “Báo Khoa Học”. 
Tôi nhớ được bài toán số học sau đây trong báo đó, 
xin ghi lại để quý vị nào có hứng thì giải đáp cho vui.

“Trong một cuộc chiến, vị sĩ quan chỉ huy chiến trường gửi công điện tới thượng cấp
 để báo cáo về tình hình của pháo binh. Nguyên văn như sau đây:

Rằng ta đã có KHOA đại pháo
Mất HỌC rồi, còn BÁO nữa thôi

Ghi chú trong sách mật mã: dấu nặng = thêm hoặc bớt 1 đơn vị, 
dấu sắc = thêm hoặc bớt 6 đơn vị.

Xin quý độc giả tính giùm số súng đại bác hiện có.”

HCD: Bài toán “mật mã” nầy sao mà khó quá. 
Chuyện quân sự đâu là chuyện đùa, lở người nhận được báo cáo tính sai thì sao?. 
(bài giải dưới cùng email nầy, nếu các bạn muốn thử sức xin khoan đọc lời giải)
Sau cùng lời giải về câu đố “Đại Pháo”

From: toronto-me...@googlegroups.com> On Behalf Of DaiKha Dinh
Sent: Saturday, May 21, 2022 12:48 CH
To:
Subject: Re: email BS Đinh Đại Kha: Bài toán của học giả Hoàng Xuân Hãn

Anh Vĩnh thân,
Vâng, xin sửa lại bài giải có lẽ còn chưa rõ nghĩa. 
Bắt đầu hàng bên phải, A trừ C còn O, có tới 3 ẩn số, không giải được, hãy gác một bên. 
Tới hàng O trừ Ọ còn Á, theo mật mã thì O và Ọ hơn kém nhau một đơn vị. 
Giả thử không có thêm 1 ở hàng thứ nhất đem qua thì Ọ hoặc kém O 1 đơn vị và Á là 1, 
 hoặc Ọ hơn O 1 đơn vị thì O phải mượn 1 và Á là 9. 
Nếu Ọ thêm 1 ở hàng bên phải mang qua thì Á là 0 hoặc là 8
1)Xin ghi lại Á=0 hoặc 1 hoặc 8 hoặc 9.
Tới hàng H trừ H còn B. Nếu không có 1 mang qua từ hàng bên phải thì B là 0.
 Không ổn vì B là chữ số đứng đầu con số BÁO không là 0 được.
 Vậy thì có thêm 1 từ bên phải mang qua và B=9 là kết quả chắc chắn.
 Á không là 9 được
Tới hàng cuối bên trái, hàng dưới mang 1 qua, K trừ 1 hết, vậy K=1 cũng chắc chắn.
 Á không là 1 được
Nhìn lại hàng H trừ H có thêm 1 mang qua, vậy Ọ + 1 (bên phải mang qua) phải lớn hơn O
 và Á=8 kết quả chắc chắn
Theo mật mã, A và Á hơn, kém nhau 6 đơn vị, A hơn Á 6 đơn vị không ổn vì lớn hơn 10,
 vậy thì A kém Á 6 đơn vi, A=2 cũng chắc chắn.
Trở lại hàng đầu bên phải, C có thể từ 3 tới 7
Nếu C=3 thì O bằng 9 không ổn, trùng với B
Nếu C=4 thì O bằng 8, cũng không ổn
Nếu C=5 thì O bằng 7, rồi Ọ là O+1 sẽ thành 8, cũng không ổn
Nếu C=6 thì O cũng bằng 6, không ổn.
Cuối cùng C=7 kết quả chắc chắn, nghĩa là A trừ C còn 5
Vậy thì O=5 và Ọ=6
Còn lại 2 chữ số chưa dùng là 3 và 4. Vậy H có thể là 3 hoặc 4 nhưng không thành vấn đề,
 vì dù H là 3 hay 4 thì số đại pháo hiện có cũng là BÁO=985
Xin hết


-}
import Data.List
decrypt ::[(Int,Int,Int,Int,Int,Int,Int,Int,Int,Int)]
decrypt = [(k,h,o,a,h,onang,c,b,asac,o) | 
   k <- [0..0], 
   h <- [0..9], 
   b <- [0..9],  b /= h, 
   o <- [0..9],  o /= h, o /= b,
   a <- [0..9],  a /= h, a /= b, a /= o,
   c <- [0..9],  c /= h, c /= b, c /= o, c /= a,
   asac <- [0..9], 
   asac == a - 6 || asac == a + 6,
   onang <- [0..8], 
   onang == o - 1 || onang == o + 1,
   k*1000 + h*100 + o*10 +a - (h*100 + onang*10 + c ) == (b*100 + asac*10 +o) ]

daiphao :: (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int)->(Int,Int,Int)
daiphao  (k,h,o,a,h2,onang,c,b,asac,o2) = (k*1000 +h*100 +o*10 +a, h2*100 +onang*10 +c, b*100 +asac*10 + o2)

result :: (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int)->Int
result (k,h,o,a,h2,onang,c,b,asac,o2) = b*100 +asac*10 +o2

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/=x) (rmdups xs)

solutions  = map  daiphao  decrypt

results = map result decrypt

decupleToList :: (a,a,a,a,a,a,a,a,a,a) -> [a]
decupleToList (a,b,c,d,e,f,g,h,i,j) = [a,b,c,d,e,f,g,h,i,j]

decuplesToList :: [(a,a,a,a,a,a,a,a,a,a)] -> [[a]]
decuplesToList = map decupleToList

main:: IO()
main =  do
  putStrLn ("** < KHOA-HỌC = BÁO >**")
  putStrLn (" ")
  putStr ("** Tổng cộng các mật mã có thể dùng : ")
  print (length $ decrypt)
  putStrLn (" ")
  putStr ("** Đại Pháo (Có, Mất, Còn) : ")
  print (daiphao $ head decrypt)
  putStr ("** mật mã 1: ")
  print (zip "KHOAHoCBaO"  (decupleToList $ head  decrypt) )
  putStrLn (" ")
  putStr ("** Đại Pháo (Có, Mất, Còn) : ")
  print (daiphao $ last decrypt)
  putStr ("** mật mã 48: ")
  print (zip "KHOAHoCBaO"  (decupleToList $ last  decrypt) )
  