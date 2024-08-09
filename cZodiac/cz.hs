import Data.Array (Array, listArray, (!))
import Data.Monoid ( mappend, (<>) ) -- pour op mappend <>
zip4 :: [a] -> [b] -> [c] -> [d] ->[(a,b,c,d)]
-- zip4 is not in Prekude like zip and zip3:
-- zip4 =  zipWith4 (,,)
zip4 (a:as) (b:bs) (c:cs) (d:ds) = (a,b,c,d) : zip4 as bs cs ds
zip4 _      _      _    _   = []
------------------- TRADITIONAL STRINGS -----------------------
ats :: Array Int (Char, String, String)
ats =
  listArray (0, 9) $
    zip3
      -- 天干 tiangan – 10 heavenly stems
      "甲乙丙丁戊己庚辛壬癸"
      (words "jiă yĭ bĭng dīng wù jĭ gēng xīn rén gŭi")
      (words "Giáp Ất Bính Đinh Mậu Kỷ Canh Tân Nhâm Qúi")
 
ads :: Array Int (String, String, String, String)
ads =
  listArray (0, 11) $
    zip4
      -- 地支 dizhi – 12 terrestrial branches
      (chars "子丑寅卯辰巳午未申酉戌亥")
      ( words $
          "zĭ chŏu yín măo chén sì "
            <> "wŭ wèi shēn yŏu xū hài"
      )
      ( words $
          "Tý Sửu Dần Mão Thìn Tỵ "
            <> "Ngọ Mùi Thân Dậu Tuất Hợi"
      )
      ( words $
          "Candide-Travailleur/Peu-fiable Patient-Prudent/Crédule Puissant-Concentré/Arriviste Optimiste-Curieux/Lâche Créatif-Généreux/Vaniteux Adaptable-Intellectuel/Flexueux "
           <> "Amical-Impétueux/Poltron Gentil-Souple/Buté Rationnel-Sûr/Roué Intuitif-Minutieux/Vaniteux Sincère-Responsable/Excité Volontaire-Fonceur/Brouillon"
      )

aws :: Array Int (String, String, String, String)
aws =
  listArray (0, 4) $
    zip4
      -- 五行 wuxing – 5 elements
      (chars "木火土金水")
      (words "mù huǒ tǔ jīn shuǐ")
      (words "Wood Fire Earth Metal Water")
      (words "Mộc Hoả Thổ Kim Thuỷ")
 
axs :: Array Int (String, String, String, String)
axs =
  listArray (0, 11) $
    zip4
      -- 十二生肖 shengxiao – 12 symbolic animals
      (chars "鼠牛虎兔龍蛇馬羊猴鸡狗豬")
      ( words $
          "shǔ niú hǔ tù lóng shé "
            <> "mǎ yáng hóu jī gǒu zhū"
      )
      ( words $
          "Rat Ox Tiger Rabbit Dragon Snake "
            <> "Horse Goat Monkey Rooster Dog Pig"
      )
      ( words $
          "Chuột Trâu Cọp Mèo Rồng Rắn "
            <> "Ngựa Dê Khỉ Gà Tuất Heo"
      )
 
ays :: Array Int (String, String, String, String)
-- 阴阳 yinyang
ays =
  listArray (0, 1) $
    zip4 (chars "阳阴") (words "yáng yīn")
                       (words "Yang Yin")
                       (words "Dương Âm")
 
chars :: String -> [String]
chars = (flip (:) [] <$>)
 
-------------------- TRADITIONAL CYCLES ------------------
f生肖五行年份 y =
  let i年份 = y - 4
      i天干 = rem i年份 10
      i地支 = rem i年份 12
      (h天干, p天干, v天干) = ats ! i天干
      (h地支, p地支, v地支, x地支) = ads ! i地支
      (h五行, p五行, e五行, v五行) = aws ! quot i天干 2
      (h生肖, p生肖, e生肖, v生肖) = axs ! i地支
      (h阴阳, p阴阳, e阴阳, v阴阳) = ays ! rem i年份 2
   in -- 汉子 Chinese characters
      [ [show y, h天干 : h地支, h五行, h生肖, h阴阳],
        -- Pinyin roman transcription
        [[], p天干 <> p地支, p五行, p生肖, p阴阳],
        -- English
        [[], p天干 <> p地支, e五行, e生肖, e阴阳],
        -- Vietnamese
        [[], v天干 <>"-"<> v地支, v五行, v生肖, v阴阳],
        [ [],
          show (rem i年份 60 + 1) <> "/60",
        --  e五行,
        --  e生肖,
          x地支,
          []
        ]
      ]
 
--------------------------- TEST -------------------------
main :: IO ()
main =
  mapM_ putStrLn $
    showYear
      <$> [1942,1943,1953,1956,1958,1966,1971,1973,1975,2005,2006]
 
------------------------ FORMATTING ----------------------
fieldWidths :: [[Int]]
fieldWidths =
  [ [6, 10, 7, 8, 3],
    [6, 11, 8, 8, 4],
    [6, 11, 8, 8, 4],
    [6, 11, 8, 8, 5],
    [6, 11, 32]
  ]
 
showYear :: Int -> String
showYear y =
  unlines $
    ( \(ns, xs) ->
        concat $
          uncurry (`justifyLeft` ' ')
            <$> zip ns xs
    )
      <$> zip fieldWidths (f生肖五行年份 y)
  where
    justifyLeft n c s = take n (s <> replicate n c)
