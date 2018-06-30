# Haskell で作るリバーシ

[Rust で作るリバーシ](https://github.com/KOBA789/rust-reversi)の Haskell 版として作りました．

ちなみに，あまり深く考えずに真似しているので題材的に Haskell の学習に適切かどうかは微妙なところです．

## 進め方

`src/Reversi/Coord.hs` と `src/Reversi/Board.hs` にある `undefined` となっている関数に正しい実装を与えてください．
`Coord.hs`, `Board.hs` の順に上から変更することをお勧めします．

また，以降ではビルドツール [stack](https://docs.haskellstack.org/en/stable/README/) を用いて実装することを前提とする．

## ドキュメントの開き方

Haddock という機能により，コード中の型や関数についてのコメントを読みやすくレンダリングしたドキュメントを生成できます．

ドキュメントを生成・閲覧するには以下のコマンドを実行します。

```
$ stack haddock --open
```

## テストの実行

各実装がうまく書けているか確かめるため、テストを実行することができます。

全てのテストを実行するには以下のコマンドを実行します。

```
$ stack test
```

一部のテストケースのみを実行したい場合は次のようなオプションを付けます．

```
$  stack test --ta '-p "matrix"'
```

テストケースの絞り込みは部分一致である点に注意してください．

## ゲームの実行

全ての `undefined` を潰し，全てのテストも通るようになったらゲームを起動してみましょう．

ゲームを起動するには次のコマンドを実行します．

```
$ stack build
$ stack exec -- reversi
```

これでコンピューターと対戦することができます。

## 各ステップの模範解答の見方

模範解答は [`complete`](https://github.com/matsubara0507/haskell-reversi/commits/complete) ブランチにあります。

関数単位でコミットを分けてありますので、当該コミットの diff を確認してください。
