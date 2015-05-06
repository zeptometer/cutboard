# cutboard-macrocodeについて

(const data)

dataをデータスタックに積む

(lvar i j)

局所変数の値を参照してデータスタックに積む

(gvar sym)

トップレベル変数の値を参照してデータスタックに積む

(pop)

データスタックの先頭を取り除く

(tjump label)

データスタックの先頭がnon-nilである場合labelに分岐する

(fjump label)

データスタックの先頭がnilである場合labelに分岐する

(jump label)

labelへ無条件分岐

(return)

リターンスタックから分岐先をpopしてそこへ無条件分岐する

(args n)

スタックからn個のデータをpopして環境を作成する

(call)

リターンスタックにPCを格納してからデータスタック先頭の関数へ分岐する。

(fn label)

ラベルと現在の環境からクロージャを作成しデータスタックに積む

