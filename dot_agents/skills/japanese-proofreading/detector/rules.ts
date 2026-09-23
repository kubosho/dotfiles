import type { Finding, Sentence } from "./detect.ts";

type Message = Pick<Finding, "reason" | "hint" | "requiredInformation">;

type Hit = Message & { sentence: Sentence; index: number; length: number };

type Rule = {
  id: string;
  /** The heading of the pattern in 日本語の不自然な言い回しの形式. */
  pattern: string;
  check(sentences: Sentence[]): Hit[];
};

function find(
  sentences: Sentence[],
  regex: RegExp,
  message: Pick<Message, "reason" | "hint"> & Partial<Pick<Message, "requiredInformation">>,
): Hit[] {
  return sentences.flatMap((sentence) =>
    Array.from(sentence.prose.matchAll(regex), (match) => ({
      sentence,
      index: match.index,
      length: match[0].length,
      requiredInformation: [],
      ...message,
    })),
  );
}

const codeToken =
  /`[^`\n]+`|[A-Za-z_]\w*(?:\.|::)[A-Za-z_]|\b[a-z]+[A-Z]\w*|\b[A-Z][a-z0-9]+[A-Z]\w*|\w+\(\)|\b[a-z0-9]+_[a-z0-9_]+/;

export const rules: Rule[] = [
  {
    id: "literal-verb-translation",
    pattern: "技術動詞の直訳で動作が特定できない",
    check: (sentences) =>
      find(sentences, /(?<![で役])割(?:る|った|って|ります|りました|れば|ろう)/g, {
        reason: "「割る」では、何をどう分けるのかが技術文として特定できない。",
        hint: "「分割する」など動作を特定できる動詞にし、分ける対象も明示する。",
      }).filter((hit) => !hit.sentence.prose.includes("掛け")),
  },
  {
    id: "request-in-condition",
    pattern: "依頼が条件節に埋まり、主節が書き手の動作になる",
    // The main clause is taken as the writer's action only when it has no subject marker and
    // ends with a verb.
    check: (sentences) =>
      find(
        sentences,
        /[てで](?:もらえれば|もらえたら|くれれば|くれたら|いただければ|いただけたら)/g,
        {
          reason: "相手にしてほしい動作が条件節にあり、主節が書き手の動作になっている。",
          hint: "依頼を主節に置く（例：「〜を実行してほしい」）。",
        },
      ).filter((hit) => {
        const mainClause = hit.sentence.prose.slice(hit.index + hit.length);
        return (
          !mainClause.startsWith("と") &&
          !/(?:です|だ|である|でしょう)[。！？!?]*\s*$/.test(mainClause) &&
          !/[がはも]|助か|ありがた|有り難|うれし|嬉し/.test(mainClause)
        );
      }),
  },
  {
    id: "calqued-metaphor",
    pattern: "英語では薄れた比喩を、日本語の比喩語に置き換える",
    check: (sentences) =>
      find(sentences, /骨格(?!筋|標本|推定|検出)/g, {
        reason:
          "skeleton などの比喩を直訳しており、日本語の「骨格」は解剖の意味が強く読み手の注意をそらす。",
        hint: "「構成」「ひな形」など、指す内容を直接表す語にする。",
      }),
  },
  {
    id: "foreign-field-term",
    pattern: "別分野の用語を説明に持ち込む",
    check: (sentences) =>
      find(
        sentences,
        /正本|限定が落ち|(?:比喩|意味|イメージ|概念|物語|文脈|印象|感覚|輪郭|問い|論点)(?:が|は)[^。\n]{0,15}?立ち上が/g,
        {
          reason:
            "法律、言語学、文芸批評など別分野の用語で、読み手にその分野での意味を考えさせる。",
          hint: "その語で指したい内容をそのまま書く（例：「正本」→「基準となる記述」）。",
        },
      ),
  },
  {
    id: "legal-metaphor",
    pattern: "法律の規制や違反の語を評価の比喩に使う",
    check: (sentences) =>
      find(sentences, /過積載|越権/g, {
        reason: "規制や違反の語で評価しており、何が余分で何を直すべきかが分からない。",
        hint: "何が余分なのか、どこからが範囲外なのかを具体的に書く。",
        requiredInformation: ["余分な要素、または範囲外の内容"],
      }),
  },
  {
    id: "nominalized-kango",
    pattern: "動詞で書ける内容を漢語の名詞に変える",
    check: (sentences) =>
      find(sentences, /不存在/g, {
        reason: "「存在しない」を法律などで使う漢語の名詞にしており、技術文では不自然。",
        hint: "「〜が存在しない場合」のように節のまま書く。",
      }),
  },
  {
    id: "person-word-for-code",
    pattern: "コードを主体としているのに人を指す語を使う",
    check: (sentences) =>
      find(sentences, /誰(?:が|から)/g, {
        reason: "コードの動作を尋ねているのに、人を指す「誰」を使っている。",
        hint: "「コード上のどの部分で」のように、コード上の箇所を尋ねる言い方にする。",
      }).filter(
        (hit) =>
          /変換|返(?:す|し|され)|呼|投げ|処理|生成|検証|捕捉|解析|パース|参照|書き換え|握りつぶ|ラップ/.test(
            hit.sentence.prose.slice(hit.index),
          ) && codeToken.test(hit.sentence.original),
      ),
  },
  {
    id: "unspecified-behavior",
    pattern: "動作の仕様を程度を表す言い方に置き換える",
    check: (sentences) =>
      find(
        sentences,
        /(?:無言で|黙って|静かに(?!な))[^。、\n]{0,12}?(?:降り|終了|終わ|抜け|失敗|落ち|戻|返|スキップ|無視|捨て|握りつぶ|動|進|通|処理|実行|成功|壊れ|exit|return)/g,
        {
          reason:
            "動作を程度や比喩で表しており、プログラムが何を出力し、どう終了するかが特定できない。",
          hint: "本文や文脈から特定できる実際の動作を書く。特定できなければ値を推測せず、不足している情報として報告する。",
          requiredInformation: ["実際の動作（戻り値、終了コード、出力の有無と出力先など）"],
        },
      ),
  },
  {
    id: "vague-evaluative-ending",
    pattern: "異なる結果を汎用的な評価語で閉じる",
    check: (sentences) => {
      const message = {
        reason: "汎用的な評価語で閉じており、何がどう変わったかが文章から読み取れない。",
        hint: "変更したこと、変化した対象、観察された結果を書く。",
        requiredInformation: ["変化した対象", "観察された結果"],
      };
      return [
        ...find(
          sentences,
          /(?:効(?:く|きます|きました|いた|いている|いています|いていた|いていました)|刺さ(?:る|ります|りました|った|っている|っています)|響(?:く|きます|きました|いた|いている|いています)|効果的(?:だ|です|だった|でした))(?=(?:ね|よ)?[。！？!?]*\s*$)/g,
          message,
        ),
        ...find(
          sentences,
          /(?:効く|効いた|効いている|刺さる|刺さった|響く|響いた)の(?:が|は)/g,
          message,
        ),
      ];
    },
  },
  {
    id: "sentence-connection",
    pattern: "文を「だから」やダッシュでつなぐ",
    check: (sentences) => [
      ...find(sentences, /(?<=^\s*(?:[-*+]\s+|\d+\.\s+)?)だから(?!といって|と言って)/g, {
        reason: "短文を「だから」でつないでおり、結論が後に回る。",
        hint: "結論を先にして一文にまとめる（例：「今から着手することで先行者有利を獲得」）。",
      }),
      ...find(sentences, /(?<=[^\s─—―]\s*)(?:─{2,}|—+|―+)(?=\s*[^\s─—―])/g, {
        reason: "ダッシュで文をつないでおり、前後の関係が分からない。",
        hint: "関係に応じて「A：B」「A（B）」「A → B」のように書く。",
        requiredInformation: ["ダッシュの前後の関係"],
      }),
    ],
  },
];
