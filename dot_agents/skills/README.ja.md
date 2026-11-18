# Skills

[English](README.md)

chezmoi で `~/.agents/skills/` に配置するエージェント用の skill。各ディレクトリの `SKILL.md` に、skill を使う条件と動作を定義している。

## 作業と委譲

| Skill | 説明 |
| --- | --- |
| [delegate-implementation](delegate-implementation/SKILL.md) | 実装タスクを新しい Herdr ペインの Sonnet エージェントに渡す。依頼内容は単独で読める形でスクラッチパッドに書く。`HERDR_ENV=1` が必要。 |
| [reports](reports/SKILL.md) | Herdr の各ペインで行った作業を、ペインごとのタブを持つ1つの Artifact ページにまとめる。図、関連するプロンプトと返答、そのペインへフォーカスを移すボタンを含む。`HERDR_ENV=1` が必要。 |
| [decision-list](decision-list/SKILL.md) | 直近の会話や委譲先エージェントの報告から、ユーザーが判断すべき事項を抜き出し、判断ごとに図と回答欄を持つ Artifact ページにする。 |
| [task-register](task-register/SKILL.md) | 説明文から、Obsidian の `91_Tasks/Lists` にタスクファイルの下書きを作る。今すぐ着手せず記録しておきたい作業に使う。 |
| [pr-screenshot](pr-screenshot/SKILL.md) | UI の変更を実際の画面で確認し、状態ごとにスクリーンショットを撮って `gh --attach` で PR に添付する。 |

## レビューと文章

| Skill | 説明 |
| --- | --- |
| [drunk-reviewer](drunk-reviewer/SKILL.md) | 深夜2時に数杯飲んだ中堅開発者の口調でコードレビューをし、difit のインラインコメントとして残す。 |
| [japanese-tech-writing](japanese-tech-writing/SKILL.md) | 技術ブログ記事、design doc、ポストモーテムなど日本語の技術文書の論証、演出の抑制、整形の規範。段落単位の編集も扱う。 |
| [information-architect](information-architect/SKILL.md) | 情報アーキテクチャ（サイトマップ、taxonomy、メタデータスキーマ、コンテンツモデル、ナビゲーション、ラベリング）を UX/UI デザイナーへの引き継ぎ成果物として設計または監査する。skill 名を明示したときか、エージェントから呼ばれたときだけ使う。 |

## 学習と思考

| Skill | 説明 |
| --- | --- |
| [kabeuchi](kabeuchi/SKILL.md) | 設計や計画について自由回答式の質問を1つずつ出し、設計理由をユーザー自身の言葉で説明できるようにする。 |
| [protege-test](protege-test/SKILL.md) | Claude が好奇心のある初心者役になり、直近の会話で扱った概念をユーザーに説明させて理解度を確かめる。 |
| [english-coach](english-coach/SKILL.md) | GitHub の Issue や PR での英語の読み書きを CEFR A2 想定で支援する。構成と定型表現を示し、ユーザーの下書きにヒントを出すが、代筆はしない。 |

## 日記

| Skill | 説明 |
| --- | --- |
| [write-diary](write-diary/SKILL.md) | Obsidian にある今日の AI 日記に作業の記録を追記する。明示的に頼まれたときだけ使う。 |
| [diary-report](diary-report/SKILL.md) | 指定期間の AI 日記を読み、傾向と次に取れる行動を一時的な単体 HTML レポートにまとめる。 |
