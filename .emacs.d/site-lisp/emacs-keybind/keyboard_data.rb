Header = <<EOF
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
<title></title>
</head>
<body>
<p>
<h1>Index</h1>
<ol>
  <li><a href="#frequency">frequency</a></li>
  <li><a href="#keybind">keybind</a></li>
  <ol>
    <li><a href="#self">?</a></li>
    <li><a href="#control">C-?</a></li>
    <li><a href="#alt">M-?</a></li>
    <li><a href="#control_alt">C-M-?</a></li>
    <li><a href="#control_x_control">C-x C-?</a></li>
    <li><a href="#control_x">C-x ?</a></li>
    <li><a href="#control_x_4">C-x 4 ?</a></li>
    <li><a href="#control_x_5">C-x 5 ?</a></li>
    <li><a href="#control_x_a">C-x a ?</a></li>
    <li><a href="#control_x_n">C-x n ?</a></li>
    <li><a href="#control_x_r">C-x r ?</a></li>
    <li><a href="#control_x_v">C-x v ?</a></li>
    <li><a href="#control_c_control">C-c C-?</a></li>
    <li><a href="#control_c">C-c ?</a></li>
  </ol>
</ol>
EOF

Footer = <<EOF
</p>
</body>
</html>
EOF

HeaderHatena = <<EOF
**Emacs Keybind
EOF

KeyboardAscii =
  [
   # ESC Å` DEL
   [
    ['ESC'],
    ['F1'],
    ['F2'],
    ['F3'],
    ['F4'],
    ['F5'],
    ['F6'],
    ['F7'],
    ['F8'],
    ['F9'],
    ['F10'],
    ['F11'],
    ['F12'],
    ['INS'],
    ['DEL'],
   ],
   # îºäp/ëSäp Å` Backspace
   [
    ['`~', %w(` ~)],
    ['1!', %w(1 !)],
    ['2@', %w(2 @)],
    ['3#', %w(3 #)],
    ['4$', %w(4 \$)],
    ['5%', %w(5 %)],
    ['6^', %w(6 ^)],
    ["7&amp;", %w(7 &)],
    ['8*', %w(8 \*)],
    ['9(', %w(9 \\\()],
    ['0)', %w(0 \\\))],
    ['-_', %w(- _)],
    ['= +', %w(= \+)],
    ['BS'],
   ],
   # TAB Å` RET
   [
    ['TAB', %w(TAB)],
    ['Q', %w(q Q)],
    ['W', %w(w W)],
    ['E', %w(e E)],
    ['R', %w(r R)],
    ['T', %w(t T)],
    ['Y', %w(y Y)],
    ['U', %w(u U)],
    ['I', %w(i I)],
    ['O', %w(o O)],
    ['P', %w(p P)],
    ['[{', %w(\[ \{)],
    [']}', %w(\] \})],
    ['\|', %w(\\\\ \|)],
   ],
   # Caps Å` RET
   [
    ['Caps'],
    ['A', %w(a A)],
    ['S', %w(s S)],
    ['D', %w(d D)],
    ['F', %w(f F)],
    ['G', %w(g G)],
    ['H', %w(h H)],
    ['J', %w(j J)],
    ['K', %w(k K)],
    ['L', %w(l L)],
    [';:', %w(; :)],
    ["'\"", %w(' ")],
    [''],
    ['RET', %w(RET)],
   ],
   # Shift Å` Shift
   [
    ['Shift'],
    ['Z', %w(z Z)],
    ['X', %w(x X)],
    ['C', %w(c C)],
    ['V', %w(v V)],
    ['B', %w(b B)],
    ['N', %w(n N)],
    ['M', %w(m M)],
    [',&lt;', %w(, <)],
    ['.&gt;', %w(\. >)],
    ['/?', %w(\/ \?)],
    [''],
    [''],
    ['Shift'],
   ],
   # Ctrl Å` [right]
   [
    ['Ctrl'],
    ['Alt'],
    [''],
    [''],
    ['SPC', %w(SPC)],
    [''],
    [''],
    [''],
    ['Alt'],
    ['Ctrl'],
    ['[up]'],
    ['[down]'],
    ['[left]'],
    ['[right]'],
   ],
  ]

KeyboardJapanese =
  [
   # ESC Å` DEL
   [
    ['ESC'],
    ['F1'],
    ['F2'],
    ['F3'],
    ['F4'],
    ['F5'],
    ['F6'],
    ['F7'],
    ['F8'],
    ['F9'],
    ['F10'],
    ['F11'],
    ['F12'],
    ['INS'],
    ['DEL'],
   ],
   # îºäp/ëSäp Å` Backspace
   [
    ['one byte/two byte'],
    ['1!', %w(1 !)],
    ['2"', %w(2 ")],
    ['3#', %w(3 #)],
    ['4$', %w(4 \$)],
    ['5%', %w(5 %)],
    ['6&amp;', %w(6 &)],
    ["7'", %w(7 ')],
    ['8(', %w(8 \\\()],
    ['9)', %w(9 \\\))],
    ['0', %w(0)],
    ['-=', %w(- =)],
    ['^~', %w(^ ~)],
    ['\|', %w(\\\\ \|)],
    ['BS'],
   ],
   # TAB Å` RET
   [
    ['TAB', %w(TAB)],
    ['Q', %w(q Q)],
    ['W', %w(w W)],
    ['E', %w(e E)],
    ['R', %w(r R)],
    ['T', %w(t T)],
    ['Y', %w(y Y)],
    ['U', %w(u U)],
    ['I', %w(i I)],
    ['O', %w(o O)],
    ['P', %w(p P)],
    ['@`', %w(@ `)],
    ['[{', %w(\[ \{)],
    [''],
    ['RET', %w(RET)],
   ],
   # Caps Å` RET
   [
    ['Caps'],
    ['A', %w(a A)],
    ['S', %w(s S)],
    ['D', %w(d D)],
    ['F', %w(f F)],
    ['G', %w(g G)],
    ['H', %w(h H)],
    ['J', %w(j J)],
    ['K', %w(k K)],
    ['L', %w(l L)],
    [';+', %w(; \+)],
    [':*', %w(: \*)],
    [']}', %w(\] \})],
    [''],
    [''],                         # RETÇÃÇ›ì¡éÍÇ≈ìÒóÒÇ≈ï\é¶Ç∑ÇÈ
   ],
   # Shift Å` Shift
   [
    ['Shift'],
    ['Z', %w(z Z)],
    ['X', %w(x X)],
    ['C', %w(c C)],
    ['V', %w(v V)],
    ['B', %w(b B)],
    ['N', %w(n N)],
    ['M', %w(m M)],
    [',&lt;', %w(, <)],
    ['.&gt;', %w(\. >)],
    ['/?', %w(\/ \?)],
    ['\_', %w(\\\\ _)],
    [''],
    [''],
    ['Shift'],
   ],
   # Ctrl Å` [right]
   [
    ['Ctrl'],
    [''],
    ['Alt'],
    ['No conversion'],
    ['SPC', %w(SPC)],
    ['Conversion'],
    ['Alt'],
    ['Ctrl'],
    ['[up]'],
    ['[down]'],
    ['[left]'],
    ['[right]'],
   ],
  ]

