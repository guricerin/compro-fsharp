open System
open System.Collections.Generic

[<AutoOpen>]
module Cin =
    let read f = stdin.ReadLine() |> f
    let reada f = stdin.ReadLine().Split() |> Array.map f
    let readChars() = read string |> Seq.toArray
    let readInts() = readChars() |> Array.map (fun x -> Convert.ToInt32(x.ToString()))

[<AutoOpen>]
module Cout =
    let writer = new IO.StreamWriter(new IO.BufferedStream(Console.OpenStandardOutput()))
    let print (s: string) = writer.Write s
    let println (s: string) = writer.WriteLine s
    let inline puts (s: ^a) = string s |> println

//  　 　 　 　／ 　 　 　 　 　 　 ｀　､ 　 　 　 感謝するぜ　　お前と出会えた
// 　　　 　 　/　 　　　　　　　　ノﾉ　　ヽ
// 　　　　　 ,　　　　　ニニ彡'⌒　　　 /｀ヽ　　　　　　　 これまでの　　全てに
// 　　 　　　' 　 ニミ　ニニ彡　 　 　 〈ｒう├－-ミ
// 　　 　 　 { {　ニミ　} j ｊ ｊﾉｘ'ィイく　　}し｛＼　　 ｀丶、＿＿_／ニニニ
// 　　　　 　ｊ_ニニミV ﾊﾚﾉ ｘ＜⌒ヽ　 V ﾍ 　＼　　　 ＼ニニニニニニニ
// 　　　　　 {xミミｰ'ヾ(､ﾙ( 厶ｔｧァく⌒ヾ}　　）ﾊ::::::.　　　　＼ニニニニニニ
// 　　　　　彡ｨ'"＞tｧ}　＼(｀ニ彡 ノ` /ト=く 　 ::::::i　　　　　＼ニニニニニニ
// 　　 　　( 　 V＾`こ7　 _, ＼｀`ヾヽ｀　ﾉ|｀ヽ ヽ ｌ:::::|　　　　　　 ＼ニニニニニ
// 　　 　 　 　 ∧　　{ '　　｀ ﾉ＾ヽ 　 　{ ﾉ　　 　 !:::::|　　　＿__ノ^ヽニニニニニニ
// 　　　　　 ／.::::＼ゝヽ. _ノヽ｀｀ヽ, -――- 、 /:::::/　／ 　 　　￣｀ヽニニニニニ
// 　　 　 ／.::::::::::::::::>'"ﾉﾙﾊヽ`／ -―- ､⌒Ｖ::::::/.／／ ｊ＿__ノ、　　ヽニニニニニ
// 　　／ニニ、｀ヽ｀ヾﾍ{ {､ﾑイ　､＿（　　 >　 ＼／ （__ ノニニニ　　 　 ＼ニニニニ
// 　,仁ニニニ＼ヽヽヽ ∨　　　/ニﾆ＞彡＞--'）__ ノ　　 　｀ヽニ　　　　　＼ニニニ二
// 　ニニニニニﾆヽ 　 /　　 　 {ニニ＞ ´　｀¨¨´　　　　　　　　 ﾆ}　　　　　　＼＞''"´
// 　ニニニニニニﾆﾆ/　　　　 ∨　/　　　　　　　　　　　　　　　}八
// 　ニニニニニニﾆ./　 　 　 　 }ニ{　　　　　　　　　　　　　 　 ﾉニヽ　　　　 ﾉ
// 　ニニニニニﾆﾆ/　　　　　　 }ニﾊ　　　　　　　　 　 　 　 ／⌒ヽヽヽ ___彡
// 　ニニニニニﾆﾆ!　　　 　 　 ﾉニニヽ、　 　 　 　 　 　 ／　　　　　｀ ー=彡'ニニニニニ
// 　ニニニニニﾆﾆ}　　　　　　　　　　⌒`丶、　　　　 ／⌒ヽ　 ﾉ　　　　　ノ＿＿＿＿＿
// 　 ／￣￣￣｀ヽ/ヽ、　＿彡ﾍ{ ｛　　　 　 　 ＞ ､　/　　 　 ／ ￣￣￣
// 　　 　 ) ､　 　 /　　 ヾ､　　　 ヽ ヽ　 　 　 （ 　 　`{　　　 /
// 　//　⌒ヽ　 /　　　　〃 トミ　　＿__　＞--‐=､ 　 ヽ ＿ﾉ
// 　 { 　 　 　 /　　　　//　　　　 /　　　　　　　　 ＼__ﾉ

// -----------------------------------------------------------------------------------------------------



// -----------------------------------------------------------------------------------------------------
writer.Dispose()
