# Hello, Zig! 記録

* 0.11.0 stableを選択
* WSL2でsnapが使えない
* バイナリからインストール
* VSCodeでプロジェクトを開くといい感じのプラグインがあるのでインストール
* ZLSのインストールを促されるがずっと "Installing zls...: Downloading zls executable..." となって進まない
* ZLSをソースコードからビルドしようとするとzigのバージョン足りないと言われる
* 仕方ないのでmaster (zig-linux-x86_64-0.12.0-dev.170+750998eef.tar.xz) に書き換える
* zig build -Doptimize=RelaseSafeして~/.local/binに移動
* やっとVSCodeでまともに書けそうになった
* https://raw.githubusercontent.com/ziglang/zig/master/.gitignore
